#!/usr/bin/env Rscript

# Post-processing script: add woodfuel energy to Primary Energy|Biomass.
#
# Reads woodfuel demand from demand-driven fulldata.gdx files, converts mass to EJ,
# and adds to Variable == "Primary Energy|Biomass".
#
# Expects input matrix from add_bioenergy_prices (with P1/P2/P3 columns).
# Writes *_with_woodfuel.csv; does not alter the input file.

library(magclass)
library(gdx2)

# ===== Settings (rev5 Sustainable CDR; must match main loop script) =====
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(script_path) == 0) {
    return(normalizePath(getwd()))
  }
  dirname(normalizePath(script_path[1]))
}

MAGPIE_OUTPUT_ROOT <- Sys.getenv("MAGPIE_OUTPUT_ROOT", "")
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
matrix_in         <- file.path(matrix_dir, paste0(date_prefix, "_magpie_input_", scenario_tag, "_with_BE_prices.csv"))
matrix_out        <- file.path(matrix_dir, paste0(date_prefix, "_magpie_input_", scenario_tag, ".csv"))

be_values         <- c(0, 5, 7, 10, 15, 25, 45)
ghg_values        <- c(0, 10, 20, 50, 100, 200, 400, 600, 1000, 2000, 3000, 4000)
run_suffix_demand <- "_demand"

years <- c(1995, 2000, 2005, 2010, 2015, 2020, 2025,
           2030, 2035, 2040, 2045, 2050, 2055, 2060,
           2070, 2080, 2090, 2100, 2110)
year_cols <- as.character(years)

region_map <- c(
  AFR = "SubSaharanAfrica",
  CHA = "ChinaReg",
  CPA = "PlannedAsiaChina",
  EEU = "CentralEastEurope",
  FSU = "FormerSovietUnion",
  LAM = "LatinAmericaCarib",
  MEA = "MidEastNorthAfrica",
  NAM = "NorthAmerica",
  PAO = "PacificOECD",
  PAS = "OtherPacificAsia",
  SAS = "SouthAsia",
  WEU = "WesternEurope",
  GLO = "World"
)
# ==============================================================

if (!file.exists(matrix_in)) {
  stop("Matrix input file not found: ", matrix_in)
}
mat <- read.csv(matrix_in, stringsAsFactors = FALSE, check.names = FALSE)

sample_gdx <- file.path(base_run_dir, paste0(scenario_name, "_BE00_G0000", run_suffix_demand), "fulldata.gdx")
if (!file.exists(sample_gdx)) stop("Sample GDX not found: ", sample_gdx)

attr_obj <- readGDX(sample_gdx, "fm_attributes")[, , "woodfuel"][, , "ge"]
attr_df <- as.data.frame(attr_obj)
if (nrow(attr_df) == 0 || all(is.na(attr_df$Value))) {
  stop("Could not read woodfuel ge coefficient from fm_attributes in: ", sample_gdx)
}
woodfuel_ge <- as.numeric(attr_df$Value[which(!is.na(attr_df$Value))[1]])
message("Using woodfuel energy coefficient (GJ/tDM): ", woodfuel_ge)

extract_woodfuel_ej <- function(gdx_path) {
  if (!file.exists(gdx_path)) {
    stop("Missing GDX file: ", gdx_path)
  }

  wf_obj <- readGDX(gdx_path, "pm_demand_forestry")[, , "woodfuel"]
  wf_df <- as.data.frame(wf_obj)
  if (!all(c("Region", "Year", "Value") %in% names(wf_df))) {
    stop("Unexpected pm_demand_forestry structure in: ", gdx_path)
  }

  wf_df <- wf_df[wf_df$Year %in% years, c("Region", "Year", "Value")]
  wf_df$Value <- as.numeric(wf_df$Value) * woodfuel_ge / 1000

  wf_df$Region <- unname(region_map[wf_df$Region])
  wf_df <- wf_df[!is.na(wf_df$Region), , drop = FALSE]

  world_rows <- aggregate(Value ~ Year, data = wf_df, sum, na.rm = TRUE)
  world_rows$Region <- "World"
  wf_df <- rbind(wf_df, world_rows[, c("Region", "Year", "Value")])

  wf_df
}

message("Extracting woodfuel EJ from all BE x GHG runs...")
all_rows <- list()
ix <- 0
for (be in be_values) {
  be_str <- sprintf("%02d", be)
  bio_label <- paste0("BIO", be_str)
  for (ghg in ghg_values) {
    ghg_str <- sprintf("%04d", ghg)
    ghg_label <- paste0("GHG", formatC(ghg, width = 3, flag = "0"))

    run_name <- paste0(scenario_name, "_BE", be_str, "_G", ghg_str, run_suffix_demand)
    gdx_path <- file.path(base_run_dir, run_name, "fulldata.gdx")

    wf <- extract_woodfuel_ej(gdx_path)
    wf$BIOscen <- bio_label
    wf$GHGscen <- ghg_label
    ix <- ix + 1
    all_rows[[ix]] <- wf
  }
}
wood_df <- do.call(rbind, all_rows)

wood_wide <- reshape(
  wood_df,
  idvar = c("Region", "BIOscen", "GHGscen"),
  timevar = "Year",
  direction = "wide"
)
names(wood_wide) <- sub("^Value\\.", "", names(wood_wide))
for (yc in year_cols) {
  if (!yc %in% names(wood_wide)) wood_wide[[yc]] <- 0
}
wood_wide <- wood_wide[, c("Region", "BIOscen", "GHGscen", year_cols)]

target <- mat$Variable == "Primary Energy|Biomass" & mat$SSPscen == "SSP2"
n_target <- sum(target)
if (n_target == 0) {
  stop("No rows found for Variable == 'Primary Energy|Biomass' in matrix.")
}

mat_target <- mat[target, ]
key <- paste(mat_target$Region, mat_target$BIOscen, mat_target$GHGscen, sep = "||")
wood_key <- paste(wood_wide$Region, wood_wide$BIOscen, wood_wide$GHGscen, sep = "||")
idx <- match(key, wood_key)

missing_matches <- sum(is.na(idx))
if (missing_matches > 0) {
  warning("Missing woodfuel matches for ", missing_matches, " matrix rows. These rows will remain unchanged.")
}

for (yc in year_cols) {
  add_vals <- rep(0, length(idx))
  matched <- which(!is.na(idx))
  if (length(matched) > 0) {
    add_vals[matched] <- as.numeric(wood_wide[[yc]][idx[matched]])
  }
  mat_target[[yc]] <- as.numeric(mat_target[[yc]]) + add_vals
}

mat[target, ] <- mat_target

write.csv(mat, matrix_out, row.names = FALSE, quote = c(1, 2, 3, 4, 5, 6, 7))
message("Done. Updated matrix written to: ", matrix_out)
message("Rows updated (Primary Energy|Biomass): ", n_target)
message("Missing (Region,BIO,GHG) matches: ", missing_matches)
