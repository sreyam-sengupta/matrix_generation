#!/usr/bin/env bash
# =============================================================================
# Master pipeline: Sustainable CDR rev5 (baseline, food, water, biodiversity, all, water-bio)
#
# Runs the full pipeline per scenario:
#   1. raw + map + matrix phases  (2026-03-20_MMEmu_createMatrix-MP_loop_trimmed.R)
#   2. add bioenergy prices        (2026-03-20_add_bioenergy_prices.R)
#   3. add woodfuel to bioenergy   (2026-04-09_add_woodfuel_to_bioenergy.R)
#
# MAgPIE run folders (rev5):
#   baseline, food, water:
#     .../Sustainable_CDR_{variant}_rev5/SSP2_BD00/
#       SSP2_BD00_BE{xx}_G{yyyy}_demand
#       SSP2_BD00_BE{xx}_G0000_price
#   biodiversity, all, water-bio:
#     .../Sustainable_CDR_{variant}_rev5/SSP2_BD78/
#       SSP2_BD78_BE{xx}_G{yyyy}_demand
#       SSP2_BD78_BE{xx}_G0000_price
#
# Usage (baseline only):
#   cd .../matrix_creation
#   TARGET_SCENARIOS=baseline bash 2026-03-20_run_matrix_pipeline.sh
#
# All five scenario sets:
#   TARGET_SCENARIOS=baseline,food,water,biodiversity,all,water-bio bash 2026-03-20_run_matrix_pipeline.sh
#
# Parallel raw phase (7 BE shards):
#   PARALLEL=true TARGET_SCENARIOS=baseline bash 2026-03-20_run_matrix_pipeline.sh
#
# Optional overrides:
#   DATE_PREFIX=2026-06-05
#   MASPIE_OUTPUT_DIR=/path/to/SSP2_BD00   # explicit MAgPIE run folder
#   MAGPIE_OUTPUT_ROOT=/path/to/magpie/output
#   MATRIX_OUTPUT_DIR=/path/to/matrix/out  # default: output/rev5_new_mapping/<variant>
#   MATRIX_CREATION_ROOT=/path/to/repo     # default: directory containing this script
#   MAP_FILE=/path/to/mapping.csv          # default: $MATRIX_CREATION_ROOT/2026-06-05_MM_mapping_ds.csv
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOOP_SCRIPT="$SCRIPT_DIR/2026-03-20_MMEmu_createMatrix-MP_loop_trimmed.R"
POST_SCRIPT="$SCRIPT_DIR/2026-03-20_add_bioenergy_prices.R"
WOOD_SCRIPT="$SCRIPT_DIR/2026-04-09_add_woodfuel_to_bioenergy.R"

export MATRIX_CREATION_ROOT="${MATRIX_CREATION_ROOT:-$SCRIPT_DIR}"
export MAP_FILE="${MAP_FILE:-$MATRIX_CREATION_ROOT/2026-06-05_MM_mapping_ds.csv}"

MAGPIE_OUTPUT_ROOT="${MAGPIE_OUTPUT_ROOT:-}"
DATE_PREFIX="${DATE_PREFIX:-$(TZ=Europe/Vienna date +%Y-%m-%d)}"
TARGET_SCENARIOS="${TARGET_SCENARIOS:-baseline}"

PARALLEL="${PARALLEL:-false}"
BE_VALUES=(0 5 7 10 15 25 45)
VALID_SCENARIOS=(baseline food water biodiversity all water-bio)
BD78_SCENARIOS=(biodiversity all water-bio)

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"; }

magpie_input_dir() {
    local variant="$1"
    if [[ -n "${MASPIE_OUTPUT_DIR:-}" ]]; then
        echo "$MASPIE_OUTPUT_DIR"
        return
    fi
    if [[ -z "${MAGPIE_OUTPUT_ROOT:-}" ]]; then
        echo "ERROR: Set MASPIE_OUTPUT_DIR or MAGPIE_OUTPUT_ROOT before running the pipeline." >&2
        exit 1
    fi
    local ssp="SSP2_BD00"
    if [[ " ${BD78_SCENARIOS[*]} " =~ " ${variant} " ]]; then
        ssp="SSP2_BD78"
    fi
    echo "${MAGPIE_OUTPUT_ROOT}/Sustainable_CDR_${variant}_rev5/${ssp}"
}

preflight_check() {
    local variant="$1"
    local indir
    indir="$(magpie_input_dir "$variant")"
    if [[ ! -d "$indir" ]]; then
        echo "ERROR: MAgPIE output directory not found: $indir"
        exit 1
    fi
    local n_demand n_mif
    n_demand=$(find "$indir" -maxdepth 1 -type d -name '*_demand' 2>/dev/null | wc -l)
    n_mif=$(find "$indir" -maxdepth 1 -type d -name '*_demand' -exec test -f '{}/report.mif' \; -print 2>/dev/null | wc -l)
    log "${variant}: ${n_mif}/${n_demand} demand runs with report.mif in ${indir}"
    if [[ "$n_mif" -lt 84 ]]; then
        echo "WARNING: expected 84 demand runs with report.mif for ${variant}; found ${n_mif}"
    fi
}

run_loop_sequential() {
    local logfile="$LOG_DIR/matrix_loop_${SCENARIO}_$(date '+%Y%m%d-%H%M%S').log"
    log "${SCENARIO}: starting matrix loop (sequential) → $logfile"
    SCENARIO_VARIANT="$SCENARIO_VARIANT" DATE_PREFIX="$DATE_PREFIX" MATRIX_OUTPUT_DIR="$OUT_DIR" \
        MASPIE_OUTPUT_DIR="$MASPIE_IN" MAP_FILE="$MAP_FILE" MATRIX_CREATION_ROOT="$MATRIX_CREATION_ROOT" \
        Rscript "$LOOP_SCRIPT" 2>&1 | tee "$logfile"
    log "${SCENARIO}: matrix loop done"
}

run_loop_parallel() {
    log "${SCENARIO}: starting matrix loop (parallel, 7 BE instances)"
    local pids=()
    for be in "${BE_VALUES[@]}"; do
        local logfile="$LOG_DIR/matrix_loop_${SCENARIO}_BE${be}_$(date '+%Y%m%d-%H%M%S').log"
        log "  Launching BE=${be} instance → $logfile"
        SCENARIO_VARIANT="$SCENARIO_VARIANT" DATE_PREFIX="$DATE_PREFIX" MATRIX_OUTPUT_DIR="$OUT_DIR" \
            MASPIE_OUTPUT_DIR="$MASPIE_IN" MAP_FILE="$MAP_FILE" MATRIX_CREATION_ROOT="$MATRIX_CREATION_ROOT" \
            BE_PRICE_FILTER="$be" Rscript "$LOOP_SCRIPT" 2>&1 | tee "$logfile" &
        pids+=($!)
    done
    log "${SCENARIO}: waiting for all 7 BE instances to finish..."
    for pid in "${pids[@]}"; do
        wait "$pid"
    done
    log "${SCENARIO}: all BE raw shards done"

    local logfile="$LOG_DIR/matrix_loop_${SCENARIO}_map_matrix_$(date '+%Y%m%d-%H%M%S').log"
    log "${SCENARIO}: running map+matrix phases → $logfile"
    SCENARIO_VARIANT="$SCENARIO_VARIANT" DATE_PREFIX="$DATE_PREFIX" MATRIX_OUTPUT_DIR="$OUT_DIR" \
        MASPIE_OUTPUT_DIR="$MASPIE_IN" MAP_FILE="$MAP_FILE" MATRIX_CREATION_ROOT="$MATRIX_CREATION_ROOT" \
        PHASES_FILTER="map,matrix" Rscript "$LOOP_SCRIPT" 2>&1 | tee "$logfile"
    log "${SCENARIO}: map+matrix phases done"
}

run_post_be_prices() {
    local logfile="$LOG_DIR/add_BE_prices_${SCENARIO}_$(date '+%Y%m%d-%H%M%S').log"
    log "${SCENARIO}: adding bioenergy prices → $logfile"
    SCENARIO_VARIANT="$SCENARIO_VARIANT" DATE_PREFIX="$DATE_PREFIX" MATRIX_OUTPUT_DIR="$OUT_DIR" \
        MASPIE_OUTPUT_DIR="$MASPIE_IN" MAP_FILE="$MAP_FILE" MATRIX_CREATION_ROOT="$MATRIX_CREATION_ROOT" \
        Rscript "$POST_SCRIPT" 2>&1 | tee "$logfile"
    log "${SCENARIO}: bioenergy prices added"
}

run_post_woodfuel() {
    local logfile="$LOG_DIR/add_woodfuel_${SCENARIO}_$(date '+%Y%m%d-%H%M%S').log"
    log "${SCENARIO}: adding woodfuel to Primary Energy|Biomass → $logfile"
    SCENARIO_VARIANT="$SCENARIO_VARIANT" DATE_PREFIX="$DATE_PREFIX" MATRIX_OUTPUT_DIR="$OUT_DIR" \
        MASPIE_OUTPUT_DIR="$MASPIE_IN" MAP_FILE="$MAP_FILE" MATRIX_CREATION_ROOT="$MATRIX_CREATION_ROOT" \
        Rscript "$WOOD_SCRIPT" 2>&1 | tee "$logfile"
    log "${SCENARIO}: woodfuel post-processing done"
}

# ---- Main ----
IFS=',' read -r -a scenario_list <<< "$TARGET_SCENARIOS"
for scenario_item in "${scenario_list[@]}"; do
    SCENARIO_VARIANT="$(echo "$scenario_item" | xargs | tr '[:upper:]' '[:lower:]')"
    if [[ ! " ${VALID_SCENARIOS[*]} " =~ " ${SCENARIO_VARIANT} " ]]; then
        echo "Unsupported scenario variant: '$SCENARIO_VARIANT'"
        echo "Allowed: ${VALID_SCENARIOS[*]}"
        exit 1
    fi

    if [[ " ${BD78_SCENARIOS[*]} " =~ " ${SCENARIO_VARIANT} " ]]; then
        SCENARIO="SSP2_BD78_${SCENARIO_VARIANT}_rev5"
    else
        SCENARIO="SSP2_BD00_${SCENARIO_VARIANT}_rev5"
    fi

    MASPIE_IN="$(magpie_input_dir "$SCENARIO_VARIANT")"
    # Matrix output per scenario variant under output/rev5_new_mapping/{baseline,food,water,biodiversity,all,water-bio}
    OUT_DIR="$SCRIPT_DIR/output/rev5_new_mapping/$SCENARIO_VARIANT"
    LOG_DIR="$OUT_DIR/logs"
    mkdir -p "$LOG_DIR"

    preflight_check "$SCENARIO_VARIANT"

    log "Pipeline starting for ${SCENARIO}"
    log "MAgPIE input: ${MASPIE_IN}"
    log "Matrix output: ${OUT_DIR}"
    log "Parallel raw phase: $PARALLEL"
    log "DATE_PREFIX: $DATE_PREFIX"

    if [[ "$PARALLEL" == "true" ]]; then
        run_loop_parallel
    else
        run_loop_sequential
    fi

    run_post_be_prices
    run_post_woodfuel

    log "===== ${SCENARIO}: DONE ====="
    log "Matrix (base):           $OUT_DIR/${DATE_PREFIX}_magpie_input_${SCENARIO}.csv"
    log "Matrix (+ BE prices):    $OUT_DIR/${DATE_PREFIX}_magpie_input_${SCENARIO}_with_BE_prices.csv"
    log "Matrix (+ woodfuel):     $OUT_DIR/${DATE_PREFIX}_magpie_input_${SCENARIO}.csv"
done

log "All done!"
