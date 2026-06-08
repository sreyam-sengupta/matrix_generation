# Post-processing script: add three bioenergy price columns to the final matrix.
#
# This script reads the matrix CSV produced by the main loop script and adds:
#   P1 (new):    Price|Primary Energy|Biomass_exo_P1
#       = exogenous subsidy (i60_2ndgen_bioenergy_subsidy) read from price-driven fulldata.gdx
#         0 for BE00, non-zero linear trajectory for BE05..BE45
#         same value across all 12 GHG categories and all regions for a given BE level
#   P2 (new):    Price|Primary Energy|Biomass_endo_P2_price-driven
#       = endogenous Prices|Bioenergy (World) from the price-driven run's report.mif
#         same value for all 12 GHG categories within a BE level
#   P3 (new):    Price|Primary Energy|Biomass_endo_P3_demand-driven
#       = endogenous Prices|Bioenergy (World) from each demand-driven run's report.mif
#         unique per BE x GHG scenario
#
# The existing "Price|Primary Energy|Biomass" in the base matrix comes from Prices|Bioenergy
# in the demand-driven report.mif via the mapping file — identical to P3. It is removed here
# and replaced by the three distinct P1/P2/P3 columns described above.
#
# Unit: all three are output as "US$2005/GJ". MAgPIE reports Prices|Bioenergy and the
# exogenous subsidy in US$2017/GJ; this script applies the mapping-file factor
# (Prices|Bioenergy -> Price|Primary Energy|Biomass) to convert to US$2005/GJ.

library(gdx)
library(magclass)

# ===== Settings (rev5 Sustainable CDR; must match main loop script) =====
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(script_path) == 0) {
    return(normalizePath(getwd()))
  }
  dirname(normalizePath(script_path[1]))
}

MAGPIE_OUTPUT_ROOT <- Sys.getenv(
  "MAGPIE_OUTPUT_ROOT",
  "/p/projects/magpie/users/sreyamse/magpie/projects/PIK_2026-03-10/magpie/output"
)
MATRIX_CREATION_ROOT <- Sys.getenv("MATRIX_CREATION_ROOT", get_script_dir())

scenario_variant <- tolower(Sys.getenv("SCENARIO_VARIANT", "baseline"))
date_prefix <- Sys.getenv(
  "DATE_PREFIX",
  format(as.POSIXct(Sys.time(), tz = "Europe/Vienna"), "%Y-%m-%d")
)
valid_variants <- c("baseline", "food", "water", "biodiversity", "all", "water-bio")
bd78_variants <- c("biodiversity", "all", "water-bio")
if (!scenario_variant %in% valid_variants) {
  stop(
    "SCENARIO_VARIANT must be one of: ",
    paste(valid_variants, collapse = ", "),
    ". Got: ", scenario_variant
  )
}

ssp_subdir <- if (scenario_variant %in% bd78_variants) "SSP2_BD78" else "SSP2_BD00"
scenario_name <- ssp_subdir
scenario_tag <- paste0(ssp_subdir, "_", scenario_variant, "_rev5")

magpie_out_override <- Sys.getenv("MASPIE_OUTPUT_DIR", "")
if (nzchar(magpie_out_override)) {
  base_run_dir <- magpie_out_override
} else {
  base_run_dir <- file.path(
    MAGPIE_OUTPUT_ROOT,
    paste0("Sustainable_CDR_", scenario_variant, "_rev5"),
    ssp_subdir
  )
}
if (!dir.exists(base_run_dir)) {
  stop("MAgPIE output directory not found: ", base_run_dir)
}

matrix_dir <- Sys.getenv(
  "MATRIX_OUTPUT_DIR",
  file.path(MATRIX_CREATION_ROOT, "output", "rev5_new_mapping", scenario_variant)
)
message("Reading MAgPIE runs from: ", base_run_dir)
message("Matrix directory: ", matrix_dir)
matrix_in         <- file.path(matrix_dir, paste0(date_prefix, "_magpie_input_", scenario_tag, ".csv"))
matrix_out        <- file.path(matrix_dir, paste0(date_prefix, "_magpie_input_", scenario_tag, "_with_BE_prices.csv"))

be_values         <- c(0, 5, 7, 10, 15, 25, 45)
ghg_values        <- c(0, 10, 20, 50, 100, 200, 400, 600, 1000, 2000, 3000, 4000)
run_suffix_price  <- "G0000_price"
run_suffix_demand <- "_demand"

PRICE_UNIT <- "US$2005/GJ"
YEARS      <- c(1995, 2000, 2005, 2010, 2015, 2020, 2025,
                2030, 2035, 2040, 2045, 2050, 2055, 2060,
                2070, 2080, 2090, 2100, 2110)
# =====================

map_file <- Sys.getenv(
  "MAP_FILE",
  file.path(MATRIX_CREATION_ROOT, "2026-06-05_MM_mapping_ds.csv")
)
get_bioenergy_usd2017_to_2005_factor <- function(path) {
  if (!file.exists(path)) {
    stop("Mapping file not found: ", path)
  }
  mapping <- read.csv(path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
  target <- "Prices|Bioenergy (US$2017/GJ)"
  hits <- mapping[mapping$piam_variable == target, , drop = FALSE]
  if (nrow(hits) == 0) {
    stop("Could not find mapping row for: ", target)
  }
  as.numeric(hits$factor[1])
}
USD2017_TO_2005 <- get_bioenergy_usd2017_to_2005_factor(map_file)
message("Bioenergy price conversion factor (US$2017 -> US$2005): ", USD2017_TO_2005)

convert_bio_price_vals <- function(vals, factor) {
  out <- as.numeric(vals) * factor
  setNames(out, names(vals))
}

# Helper: extract Prices|Bioenergy for World from a report.mif
read_mif_bio_price <- function(mif_path) {
  if (!file.exists(mif_path)) {
    warning("MISSING report.mif: ", mif_path)
    return(setNames(rep(NA_real_, length(YEARS)), as.character(YEARS)))
  }
  lines      <- readLines(mif_path, warn = FALSE)
  hdr        <- lines[grepl("^Model;", lines)][1]
  hdr_cols   <- strsplit(hdr, ";")[[1]]
  yr_cols    <- as.integer(hdr_cols[6:length(hdr_cols[hdr_cols != ""])])

  target <- lines[grepl(";World;Prices\\|Bioenergy;", lines)]
  if (length(target) == 0) {
    warning("Prices|Bioenergy/World not found in: ", mif_path)
    return(setNames(rep(NA_real_, length(YEARS)), as.character(YEARS)))
  }
  vals <- suppressWarnings(as.numeric(strsplit(target[1], ";")[[1]][6:(5 + length(yr_cols))]))
  named_vals <- setNames(vals, as.character(yr_cols))
  # Return values aligned to YEARS (NA if year not present in mif)
  setNames(named_vals[as.character(YEARS)], as.character(YEARS))
}

message("Reading matrix: ", matrix_in)
if (!file.exists(matrix_in)) stop("Matrix file not found: ", matrix_in)
mat <- read.csv(matrix_in, stringsAsFactors = FALSE, check.names = FALSE)

# ---- Step 1: remove the existing "Price|Primary Energy|Biomass" rows ----
# These come from Prices|Bioenergy in the demand-driven report.mif (via mapping file),
# which is the same source as P3. We replace them with proper GDX-sourced P1 rows below.
p1_old <- "Price|Primary Energy|Biomass"
n_removed <- sum(mat$Variable == p1_old)
mat <- mat[mat$Variable != p1_old, ]
message("P1: removed ", n_removed, " existing '", p1_old, "' rows (replaced by GDX-sourced P1)")

# ---- Identify all regions in the matrix ----
all_regions <- unique(mat$Region)
year_cols   <- as.character(YEARS)

# Helper: build new rows for a price variable
make_price_rows <- function(var_name, bio_str, ghg_str, vals_by_year) {
  # vals_by_year: named numeric vector, names = year as character
  # Replicate across all regions (World + 12 MESSAGE regions)
  rows <- lapply(all_regions, function(reg) {
    row <- data.frame(
      Region   = reg,
      Variable = var_name,
      Unit     = PRICE_UNIT,
      SSPscen  = "SSP2",
      GHGscen  = paste0("GHG", formatC(as.integer(ghg_str), width = 3, flag = "0")),
      BIOscen  = paste0("BIO", bio_str),
      SDGscen  = "noSDG_rcpref",
      stringsAsFactors = FALSE
    )
    for (yr in year_cols) {
      row[[yr]] <- as.numeric(vals_by_year[yr])
    }
    row
  })
  do.call(rbind, rows)
}

new_rows_p1 <- list()
new_rows_p2 <- list()
new_rows_p3 <- list()

for (be in be_values) {
  be_str <- sprintf("%02d", be)

  # ---- P1: exogenous subsidy from price-driven fulldata.gdx ----
  price_run <- paste0(scenario_name, "_BE", be_str, "_", run_suffix_price)
  gdx_path  <- file.path(base_run_dir, price_run, "fulldata.gdx")
  if (!file.exists(gdx_path)) {
    warning("MISSING fulldata.gdx: ", gdx_path)
    p1_vals <- setNames(rep(NA_real_, length(YEARS)), as.character(YEARS))
  } else {
    gdx_obj <- readGDX(gdx_path, "i60_2ndgen_bioenergy_subsidy", react = "silent")
    if (is.null(gdx_obj)) {
      warning("i60_2ndgen_bioenergy_subsidy not found in: ", gdx_path)
      p1_vals <- setNames(rep(0, length(YEARS)), as.character(YEARS))
    } else {
      df_gdx  <- as.data.frame(gdx_obj)
      p1_named <- setNames(as.numeric(df_gdx$Value), as.character(df_gdx$Year))
      p1_vals  <- setNames(as.numeric(p1_named[as.character(YEARS)]), as.character(YEARS))
    }
  }
  p1_vals <- convert_bio_price_vals(p1_vals, USD2017_TO_2005)
  message(
    "P1 BE", be_str, ": read from ", price_run,
    " (GDX). Year 2050 value (US$2005/GJ) = ", p1_vals["2050"]
  )

  # ---- P2: price-driven report.mif (same for all GHG) ----
  mif_price <- file.path(base_run_dir, price_run, "report.mif")
  p2_vals   <- convert_bio_price_vals(read_mif_bio_price(mif_price), USD2017_TO_2005)
  message("P2 BE", be_str, ": read from ", price_run)

  for (ghg in ghg_values) {
    ghg_str <- sprintf("%04d", ghg)

    new_rows_p1[[length(new_rows_p1) + 1]] <- make_price_rows(
      "Price|Primary Energy|Biomass_exo_P1",
      be_str, ghg_str, p1_vals
    )

    new_rows_p2[[length(new_rows_p2) + 1]] <- make_price_rows(
      "Price|Primary Energy|Biomass_endo_P2_price-driven",
      be_str, ghg_str, p2_vals
    )

    # ---- P3: demand-driven report.mif (unique per BE x GHG) ----
    demand_run <- paste0(scenario_name, "_BE", be_str, "_G", ghg_str, run_suffix_demand)
    mif_demand <- file.path(base_run_dir, demand_run, "report.mif")
    p3_vals    <- convert_bio_price_vals(read_mif_bio_price(mif_demand), USD2017_TO_2005)

    new_rows_p3[[length(new_rows_p3) + 1]] <- make_price_rows(
      "Price|Primary Energy|Biomass_endo_P3_demand-driven",
      be_str, ghg_str, p3_vals
    )
  }
  message("P3 BE", be_str, ": all GHG done")
}

p1_df <- do.call(rbind, new_rows_p1)
p2_df <- do.call(rbind, new_rows_p2)
p3_df <- do.call(rbind, new_rows_p3)
message("P1 rows added: ", nrow(p1_df))
message("P2 rows added: ", nrow(p2_df))
message("P3 rows added: ", nrow(p3_df))

# Ensure column order matches the existing matrix before binding
for (yr in year_cols) {
  if (!yr %in% names(mat)) mat[[yr]] <- NA_real_
}
col_order  <- names(mat)
p1_df      <- p1_df[, col_order]
p2_df      <- p2_df[, col_order]
p3_df      <- p3_df[, col_order]

mat_out <- rbind(mat, p1_df, p2_df, p3_df)

write.csv(mat_out, matrix_out, row.names = FALSE, quote = c(1, 2, 3, 4, 5, 6, 7))
message("Done. Output written to: ", matrix_out)
message("Total rows: ", nrow(mat_out))
