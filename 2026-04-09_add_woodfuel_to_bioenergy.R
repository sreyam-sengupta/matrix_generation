#!/usr/bin/env Rscript

# Post-processing script: add woodfuel energy to Primary Energy|Biomass.
#
# This script updates a pre-existing matrix (typically already containing P1/P2/P3)
# by reading woodfuel demand from demand-driven fulldata.gdx files and converting it
# from mass to energy units (EJ), then adding it to:
#   Variable == "Primary Energy|Biomass"
#
# Usage:
#   Rscript 2026-04-09_add_woodfuel_to_bioenergy.R none
#   Rscript 2026-04-09_add_woodfuel_to_bioenergy.R high
#
# Notes:
# - Conversion uses the "ge" entry from fm_attributes for woodfuel (GJ/tDM).
# - pm_demand_forestry is assumed in million tons, so:
#       EJ = (million tons) * 1e6 * (GJ/t) / 1e9
#          = (million tons) * (GJ/t) / 1000
# - The script writes a matrix with "_with_woodfuel.csv" suffix and does not alter
#   the input file.

library(magclass)
library(gdx2)

args <- commandArgs(trailingOnly = TRUE)
bd_scenario <- if (length(args) >= 1 && args[1] %in% c("none", "high")) args[1] else "none"
message("BD scenario: ", bd_scenario)

scenario_name <- paste0("SSP2_BD-", bd_scenario)

base_run_dir <- file.path(
  "/p/projects/magpie/users/sreyamse/magpie/projects/PIK_2026-03-10/magpie/output/Spatially_resolved_BII_rev2",
  scenario_name
)

matrix_dir <- "/p/projects/magpie/users/sreyamse/magpie/projects/PIK_2026-03-10/matrix_creation/output"
matrix_in <- file.path(matrix_dir, paste0("2026-03-20_magpie_input_", scenario_name, "_with_BE_prices.csv"))
matrix_out <- file.path(matrix_dir, paste0("2026-04-09_magpie_input_", scenario_name, "_with_woodfuel.csv"))

be_values <- c(0, 5, 7, 10, 15, 25, 45)
ghg_values <- c(0, 10, 20, 50, 100, 200, 400, 600, 1000, 2000, 3000, 4000)
run_suffix_demand <- "demand_rev2"

years <- c(1995, 2000, 2005, 2010, 2015, 2020, 2025,
           2030, 2035, 2040, 2045, 2050, 2055, 2060,
           2070, 2080, 2090, 2100, 2110)
year_cols <- as.character(years)

# Region mapping consistent with matrix generation script
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

if (!file.exists(matrix_in)) {
  stop("Matrix input file not found: ", matrix_in)
}
mat <- read.csv(matrix_in, stringsAsFactors = FALSE, check.names = FALSE)

# Determine energy conversion coefficient from one existing run
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

  # Keep target years only
  wf_df <- wf_df[wf_df$Year %in% years, c("Region", "Year", "Value")]

  # Convert million tons to EJ: Value * 1e6 * ge / 1e9
  wf_df$Value <- as.numeric(wf_df$Value) * woodfuel_ge / 1000

  # Map region codes to matrix names
  wf_df$Region <- unname(region_map[wf_df$Region])
  wf_df <- wf_df[!is.na(wf_df$Region), , drop = FALSE]

  # Add World as sum across mapped regions
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

# Reshape to wide format by year for matrix merge
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
