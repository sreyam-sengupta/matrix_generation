#!/usr/bin/env bash
# =============================================================================
# Master pipeline: generate matrices for BD-none and BD-high
#
# Runs the full pipeline sequentially for both BD scenarios:
#   1. raw + map + matrix phases  (2026-03-20_MMEmu_createMatrix-MP_loop_trimmed.R)
#   2. add bioenergy prices        (2026-03-20_add_bioenergy_prices.R)
#   3. add woodfuel to bioenergy   (2026-04-09_add_woodfuel_to_bioenergy.R)
#
# Usage:
#   bash 2026-03-20_run_matrix_pipeline.sh              # runs both BD-none and BD-high
#   bash 2026-03-20_run_matrix_pipeline.sh none         # runs BD-none only
#   bash 2026-03-20_run_matrix_pipeline.sh high         # runs BD-high only
#
# Parallel raw-phase option (optional):
#   Set PARALLEL=true to run all 7 BE-price instances in parallel per BD scenario.
#   Each instance handles one BE price level via BE_PRICE_FILTER.
#   Logs go to output/logs/.
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOOP_SCRIPT="$SCRIPT_DIR/2026-03-20_MMEmu_createMatrix-MP_loop_trimmed.R"
POST_SCRIPT="$SCRIPT_DIR/2026-03-20_add_bioenergy_prices.R"
WOOD_SCRIPT="$SCRIPT_DIR/2026-04-09_add_woodfuel_to_bioenergy.R"
LOG_DIR="$SCRIPT_DIR/output/logs"
mkdir -p "$LOG_DIR"

# BD scenarios to run (override with CLI argument)
ARG="${1:-both}"
if   [[ "$ARG" == "none" ]]; then BD_SCENARIOS=("none")
elif [[ "$ARG" == "high" ]]; then BD_SCENARIOS=("high")
else                               BD_SCENARIOS=("none" "high")
fi

# Set PARALLEL=true to run 7 BE-price instances in parallel (raw phase only)
PARALLEL="${PARALLEL:-false}"
BE_VALUES=(0 5 7 10 15 25 45)

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"; }

run_loop_sequential() {
    local bd="$1"
    local logfile="$LOG_DIR/matrix_loop_BD-${bd}_$(date '+%Y%m%d-%H%M%S').log"
    log "BD-${bd}: starting matrix loop (sequential) → $logfile"
    Rscript "$LOOP_SCRIPT" "$bd" 2>&1 | tee "$logfile"
    log "BD-${bd}: matrix loop done"
}

run_loop_parallel() {
    local bd="$1"
    log "BD-${bd}: starting matrix loop (parallel, 7 BE instances)"
    local pids=()
    for be in "${BE_VALUES[@]}"; do
        local logfile="$LOG_DIR/matrix_loop_BD-${bd}_BE${be}_$(date '+%Y%m%d-%H%M%S').log"
        log "  Launching BE=${be} instance → $logfile"
        BE_PRICE_FILTER="$be" Rscript "$LOOP_SCRIPT" "$bd" 2>&1 | tee "$logfile" &
        pids+=($!)
    done
    log "BD-${bd}: waiting for all 7 BE instances to finish..."
    for pid in "${pids[@]}"; do
        wait "$pid"
    done
    log "BD-${bd}: all BE instances done"

    # Run once more without filter to execute map+matrix phases (which aggregate all BEs)
    local logfile="$LOG_DIR/matrix_loop_BD-${bd}_map_matrix_$(date '+%Y%m%d-%H%M%S').log"
    log "BD-${bd}: running map+matrix phases → $logfile"
    Rscript "$LOOP_SCRIPT" "$bd" 2>&1 | tee "$logfile"
    log "BD-${bd}: map+matrix phases done"
}

run_post() {
    local bd="$1"
    local logfile="$LOG_DIR/add_BE_prices_BD-${bd}_$(date '+%Y%m%d-%H%M%S').log"
    log "BD-${bd}: adding bioenergy prices → $logfile"
    Rscript "$POST_SCRIPT" "$bd" 2>&1 | tee "$logfile"
    log "BD-${bd}: bioenergy prices added"
}

run_woodfuel_post() {
    local bd="$1"
    local logfile="$LOG_DIR/add_woodfuel_BD-${bd}_$(date '+%Y%m%d-%H%M%S').log"
    log "BD-${bd}: adding woodfuel to Primary Energy|Biomass → $logfile"
    Rscript "$WOOD_SCRIPT" "$bd" 2>&1 | tee "$logfile"
    log "BD-${bd}: woodfuel post-processing added"
}

# ---- Main ----
log "Pipeline starting. BD scenarios: ${BD_SCENARIOS[*]}"
log "Parallel raw phase: $PARALLEL"

for bd in "${BD_SCENARIOS[@]}"; do
    log "===== BD-${bd}: BEGIN ====="

    if [[ "$PARALLEL" == "true" ]]; then
        run_loop_parallel "$bd"
    else
        run_loop_sequential "$bd"
    fi

    run_post "$bd"
    run_woodfuel_post "$bd"

    log "===== BD-${bd}: DONE ====="
    log "Final matrix: $SCRIPT_DIR/output/2026-03-20_magpie_input_SSP2_BD-${bd}_with_BE_prices.csv"
    log "Final matrix (with woodfuel): $SCRIPT_DIR/output/2026-04-09_magpie_input_SSP2_BD-${bd}_with_woodfuel.csv"
done

log "All done!"
