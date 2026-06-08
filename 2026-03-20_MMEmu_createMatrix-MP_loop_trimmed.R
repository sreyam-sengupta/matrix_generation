library(magclass)
library(magpie4)
library(madrat)
library(stringr)
library(magpiesets)
library(gdx)
library(piamInterfaces)
library(iamc)
library(hash)


# ===== User input (rev5 Sustainable CDR) =====
# Environment:
#   SCENARIO_VARIANT  = baseline | food | water | biodiversity | all | water-bio
#   MASPIE_OUTPUT_DIR = optional explicit path to SSP2_BD{00|78} run folders
#   DATE_PREFIX       = YYYY-MM-DD for final matrix CSV (default: today, Europe/Vienna)
#   MATRIX_OUTPUT_DIR = optional matrix output directory
#   MAP_FILE          = semicolon CSV mapping (default: <repo>/2026-06-05_MM_mapping_ds.csv)
#   MATRIX_CREATION_ROOT = repo root (default: directory containing this script)
#   BE_PRICE_FILTER, PHASES_FILTER = parallel shards (see run_matrix_pipeline.sh)
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

# SSP2_BD00: baseline, food, water; SSP2_BD78: biodiversity, all, water-bio
ssp_subdir <- if (scenario_variant %in% bd78_variants) "SSP2_BD78" else "SSP2_BD00"
scenario_name <- ssp_subdir
scenario_tag <- paste0(ssp_subdir, "_", scenario_variant, "_rev5")

magpie_out_override <- Sys.getenv("MASPIE_OUTPUT_DIR", "")
if (nzchar(magpie_out_override)) {
  base_output_dir <- magpie_out_override
} else {
  base_output_dir <- file.path(
    MAGPIE_OUTPUT_ROOT,
    paste0("Sustainable_CDR_", scenario_variant, "_rev5"),
    ssp_subdir
  )
}
if (!dir.exists(base_output_dir)) {
  stop("MAgPIE output directory not found: ", base_output_dir)
}

# Run folders: SSP2_BD00_BE{xx}_G{yyyy}_demand / SSP2_BD00_BE{xx}_G0000_price
run_suffix_demand <- "_demand"
run_suffix_price  <- "G0000_price"

# BE price values (used in folder names: SSP2_BD00_BE{xx}_G{yyyy}demand)
be_price_values_all <- c(0, 5, 7, 10, 15, 25, 45) # 0, 5, 7, 10, 15, 25, 45

# Filter BE prices if BE_PRICE_FILTER environment variable is set
# This allows parallel instances to process specific BE prices
be_price_filter <- Sys.getenv("BE_PRICE_FILTER")
if (be_price_filter != "") {
  be_price_filter_num <- as.numeric(be_price_filter)
  if (be_price_filter_num %in% be_price_values_all) {
    be_price_values <- c(be_price_filter_num)  # Keep as vector for loop compatibility
    message("Filtering to BE price: ", be_price_filter_num)
  } else {
    stop("Invalid BE_PRICE_FILTER: ", be_price_filter, ". Must be one of: ", paste(be_price_values_all, collapse = ", "))
  }
} else {
  be_price_values <- be_price_values_all
}

# GHG price values (folder names: ..._G{yyyy}_demand)
ghg_price_values_all <- c(0, 10, 20, 50, 100, 200, 400, 600, 1000, 2000, 3000, 4000)
ghg_price_filter <- Sys.getenv("GHG_PRICE_FILTER")
if (ghg_price_filter != "") {
  ghg_price_filter_num <- as.numeric(ghg_price_filter)
  if (ghg_price_filter_num %in% ghg_price_values_all) {
    ghg_price_values <- c(ghg_price_filter_num)
    message("Filtering to GHG price: ", ghg_price_filter_num)
  } else {
    stop(
      "Invalid GHG_PRICE_FILTER: ", ghg_price_filter,
      ". Must be one of: ", paste(ghg_price_values_all, collapse = ", ")
    )
  }
} else {
  ghg_price_values <- ghg_price_values_all
}

# Default matrix output directory:
#   /.../matrix_creation/output/rev5_new_mapping/{baseline,food,water,biodiversity,all,water-bio}/
matrix_output_dir <- Sys.getenv(
  "MATRIX_OUTPUT_DIR",
  file.path(MATRIX_CREATION_ROOT, "output", "rev5_new_mapping", scenario_variant)
)

# Preflight: expect 84 demand + 7 price runs with report.mif
if (Sys.getenv("SKIP_RUN_VALIDATION", "") != "true") {
  demand_dirs <- list.dirs(base_output_dir, recursive = FALSE, full.names = TRUE)
  demand_dirs <- demand_dirs[grepl(paste0(run_suffix_demand, "$"), basename(demand_dirs))]
  n_demand_mif <- sum(file.exists(file.path(demand_dirs, "report.mif")))
  price_dirs <- list.dirs(base_output_dir, recursive = FALSE, full.names = TRUE)
  price_dirs <- price_dirs[grepl(paste0(run_suffix_price, "$"), basename(price_dirs))]
  n_price_mif <- sum(file.exists(file.path(price_dirs, "report.mif")))
  message("Scenario: ", scenario_tag)
  message("MAgPIE input: ", base_output_dir)
  message("Matrix output: ", matrix_output_dir)
  message("Demand runs with report.mif: ", n_demand_mif, " / ", length(be_price_values_all) * length(ghg_price_values))
  message("Price runs with report.mif: ", n_price_mif, " / ", length(be_price_values_all))
  if (n_demand_mif < length(be_price_values_all) * length(ghg_price_values)) {
    warning("Not all demand runs have report.mif; raw phase will skip missing folders.")
  }
}

load_mapping <- function(path) {
  if (!file.exists(path)) {
    stop("Mapping file not found: ", path)
  }
  if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' required to read xlsx mapping: ", path)
    }
    mapping_xlsx <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
    csv_path <- sub("\\.xlsx$", ".csv", path, ignore.case = TRUE)
    write.table(mapping_xlsx, csv_path, sep = ";", row.names = FALSE, quote = FALSE)
    path <- csv_path
  }
  list(
    df = read.csv(path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE),
    path = path
  )
}

map_file <- Sys.getenv(
  "MAP_FILE",
  file.path(MATRIX_CREATION_ROOT, "2026-06-05_MM_mapping_ds.csv")
)
mapping_info <- load_mapping(map_file)
map_file_for_project <- mapping_info$path
mapping_df <- mapping_info$df
message("Using mapping file: ", map_file_for_project, " (", nrow(mapping_df), " rows)")

# IAMC targets to keep: derived from unique Variable column in active mapping file
vars_to_keep <- unique(mapping_df$Variable)
vars_to_keep <- vars_to_keep[!is.na(vars_to_keep) & nzchar(trimws(vars_to_keep))]
message(
  "Derived ", length(vars_to_keep), " variables to keep from mapping: ",
  map_file_for_project
)

# Processing phases
phases <- c("raw", "map", "matrix")  # valid options: "raw" (most time consumed here), "map", "matrix"
# Can be overridden by PHASES_FILTER env var (comma-separated, e.g. "map,matrix")
phases_filter <- Sys.getenv("PHASES_FILTER")
if (phases_filter != "") {
  phases <- trimws(strsplit(phases_filter, ",")[[1]])
  message("Phases overridden by PHASES_FILTER: ", paste(phases, collapse = ", "))
}

# Other settings
reduced_messages <- FALSE
loop <- FALSE  # loop over raw and map process until no new raw files have been generated
loop_wait <- 0  # wait time before restarting the loop (increased with every loop, reset to 0 if raw file is written)
check_file <- "report.mif"# "cell.bii_0.5.nc" file to check if run is finished
correct_emissions <- FALSE  # flatten emissions curves across GHG price levels
baseyear <- "y2005"  # base year for price index
# ========================================


flatten_the_curve <- function(b, c) {
    for (i in seq_along(b)) {
        if (!is.na(b[i]) && b[i] > c[i]) {
            b[i] <- c[i]
        }
    }
    return(b)
}

sum_glo <- function(b) {
    b <- add_columns(b, addnm = "GLO", dim = 1, fill = 0)
    b["GLO", , ] <- toolAggregate(b, to = "global")
    return(b)
}

normalize_mif_columns <- function(mif_data) {
  if (nrow(mif_data) == 0) {
    return(mif_data)
  }
  if ("Variable" %in% names(mif_data)) {
    mif_data$Variable <- trimws(mif_data$Variable)
  }
  if ("Region" %in% names(mif_data)) {
    mif_data$Region <- trimws(mif_data$Region)
  }
  if ("Unit" %in% names(mif_data)) {
    mif_data$Unit <- trimws(mif_data$Unit)
  }
  mif_data
}

drop_mif_na_unit_duplicates <- function(mif_data) {
  if (nrow(mif_data) == 0 || !"Unit" %in% names(mif_data)) {
    return(mif_data)
  }
  is_na_unit <- mif_data$Unit %in% c("N/A", "NA", "")
  if (!any(is_na_unit)) {
    return(mif_data)
  }
  vars_with_real <- unique(mif_data$Variable[!is_na_unit])
  mif_data[!is_na_unit | !(mif_data$Variable %in% vars_with_real), , drop = FALSE]
}

fill_magpie_from_mif <- function(mif_rows, a_regions, a_years, var_names = NULL) {
  if (nrow(mif_rows) == 0) {
    return(NULL)
  }
  year_cols <- grep("^X?[0-9]{4}$", names(mif_rows), value = TRUE)
  year_names <- gsub("^X", "", year_cols)
  if (is.null(var_names)) {
    var_names <- unique(mif_rows$Variable)
  }
  out <- new.magpie(
    cells_and_regions = a_regions,
    years = a_years,
    names = var_names,
    fill = 0
  )
  for (i in seq_len(nrow(mif_rows))) {
    var <- mif_rows$Variable[i]
    region <- mif_rows$Region[i]
    if (!region %in% a_regions || !var %in% var_names) {
      next
    }
    values <- suppressWarnings(as.numeric(mif_rows[i, year_cols]))
    values[is.na(values)] <- 0
    year_indices <- match(paste0("y", year_names), a_years)
    valid_years <- !is.na(year_indices)
    if (any(valid_years)) {
      out[region, a_years[year_indices[valid_years]], var] <- values[valid_years]
    }
  }
  out
}

getReportMESSAGE <- function(
  gdx, file = NULL, detail = TRUE, baseyear = "y2005", bii_path = ".", food_only = FALSE, ...) {

  tryReport <- function(report, width, gdx) {
    regs  <- c(readGDX(gdx, "i"), "GLO")
    years <- readGDX(gdx, "t")
    message("   ", format(report, width = width), appendLF = FALSE)
    x <- try(
      eval(parse(text = paste0("suppressMessages(", report, ")"))
      ), silent = TRUE)
    if (is(x, "try-error")) {
      message("ERROR")
      x <- NULL
    } else if (is.null(x)) {
      message("no return value")
      x <- NULL
    } else if (!is.magpie(x)) {
      message("ERROR - no magpie object")
      x <- NULL
    } else if (!setequal(getYears(x), years)) {
      message("ERROR - wrong years")
      x <- NULL
    } else if (!setequal(getRegions(x), regs)) {
      message("ERROR - wrong regions")
      x <- NULL
    } else if (any(grepl(".", getNames(x), fixed = TRUE))) {
      message("ERROR - data names contain dots (.)")
      x <- NULL
    } else {
      message("success")
    }
    return(x)
  }

  tryList <- function(..., gdx) {
    width <- max(nchar(c(...))) + 1
    return(lapply(unique(list(...)), tryReport, width, gdx))
  }

  message("Start getReportMESSAGE(gdx)...")

  if (!food_only) {

    output <- tryList(
                      "reportPopulation(gdx)",
 #                    "reportIncome(gdx)",
 #                    "reportProducerPriceIndex(gdx)",
 #                    "reportPriceGHG(gdx)",
 #                    "reportFoodExpenditure(gdx)",
                      "reportKcal(gdx,detail=detail)",
 #                    "reportIntakeDetailed(gdx,detail=detail)",
 #                    "reportLivestockShare(gdx)",
 #                    "reportLivestockDemStructure(gdx)",
 #                    "reportVegfruitShare(gdx)",
 #                    "reportHunger(gdx)",
 #                    "reportPriceShock(gdx)",
 #                    "reportPriceElasticities(gdx)",
                      "reportBII(gdx)",
                      "reportProduction(gdx,detail=detail,agmip=TRUE)",
                      "reportDemand(gdx,detail=detail,agmip=TRUE)",
                      "reportDemandBioenergy(gdx,detail=detail)",
 #                    "reportFeed(gdx,detail=detail)",
 #                    "reportTrade(gdx,detail=detail)",
                      "reportLandUse(gdx)",
 #                    "reportLandUseChange(gdx)",
 #                    "reportProtectedArea(gdx)",  # Function no longer exists
                      "reportCroparea(gdx,detail=detail)",
                      "reportNitrogenBudgetCropland(gdx)",
 #                     "reportNitrogenBudgetPasture(gdx)",
 #                    "reportManure(gdx)",
                      "reportYields(gdx,detail=detail)",
                      "reportTau(gdx)",
 #                    "reportTc(gdx)",
                      "reportCostTC(gdx)",
 #                    "reportYieldShifter(gdx)",
                      "reportEmissions(gdx)",
 #                    "reportEmisAerosols(gdx)",
 #                    "reportEmissionsBeforeTechnicalMitigation(gdx)",
 #                    "reportEmisPhosphorus(gdx)",
 #                    "reportCosts(gdx)",
 #                    "reportCostsPresolve(gdx)",
                      "reportPriceFoodIndex(gdx, baseyear = baseyear)",
 #                    "reportPriceAgriculture(gdx)",
                     "reportPriceBioenergy(gdx)",
 #                    "reportPriceLand(gdx)",
                      "reportPriceWater(gdx)",
 #                    "reportValueTrade(gdx)",
 #                    "reportValueConsumption(gdx)",
 #                    "reportProcessing(gdx, indicator='primary_to_process')",
 #                    "reportProcessing(gdx, indicator='secondary_from_primary')",
 #                    "reportAEI(gdx)",
                      "reportWaterUsage(gdx)",
 #                    "reportAAI(gdx)",
 #                    "reportSOM(gdx)",
 #                    "reportGrowingStock(gdx)",
 #                    "reportSDG1(gdx)",
                      "reportSDG2(gdx)",
 #                    "reportSDG3(gdx)",
 #                    "reportSDG6(gdx)",
 #                    "reportSDG12(gdx)",
 #                    "reportSDG15(gdx)",
 #                    "reportForestYield(gdx)",
                      "reportharvested_area_timber(gdx)",
 #                    "reportPlantationEstablishment(gdx)",
 #                    "reportRotationLength(gdx)",
                      "reportTimber(gdx)",
 #                    "reportPBbiosphere(gdx, dir=bii_path)",
                      gdx = gdx)
  }  else {
    output <- tryList(
                      "reportDemand(gdx,detail=detail,agmip=TRUE)",
                      gdx = gdx)
  }


  if (!is.null(file)) write.report2(output, file = file, ...)
  else return(output)
}

# Create output directory if it doesn't exist
if (!dir.exists(matrix_output_dir)) {
  dir.create(matrix_output_dir, recursive = TRUE)
}

setwd(base_output_dir)
message("SWITCHING FOLDER: ", base_output_dir)

time <- format(Sys.time(), "%y%m%d-%H%M%S")

# Single premap and matrix files (outside loops - will contain all scenarios)
ofile <- file.path(matrix_output_dir, paste0("magpie_input-premap_", scenario_tag, "_trimmed_", time, ".csv"))
matrix_file <- file.path(
  matrix_output_dir,
  paste0(date_prefix, "_magpie_input_", scenario_tag, ".csv")
)

# Remove existing premap file if starting fresh (only if map or matrix phase will run)
if (("map" %in% phases || "matrix" %in% phases) && file.exists(ofile)) {
  file.remove(ofile)
  message("Removed existing premap file to start fresh: ", ofile)
}

# Function to strip units from variable names for comparison
# Handles nested parentheses like "Index (2005 = 1)" by removing all parenthetical content
strip_units <- function(var_name) {
  # Vectorized function - apply to each element
  sapply(trimws(var_name), function(v) {
    result <- v
    # Keep removing parentheses until no more are found
    while(grepl("\\(", result)) {
      old_result <- result
      result <- gsub("\\s*\\([^()]*\\)", "", result)  # Remove innermost parentheses first
      # If no change, try removing nested parentheses (everything from first ( to last )
      if(result == old_result) {
        result <- gsub("\\s*\\(.*\\)", "", result)
        break
      }
    }
    # Clean up any remaining double spaces or trailing characters
    result <- gsub("\\s+", " ", result)
    result <- gsub("\\s*$", "", result)
    result <- gsub("^\\s*", "", result)
    return(result)
  }, USE.NAMES = FALSE)
}

# Loop over BE prices
for (be_idx in seq_along(be_price_values)) {
  be <- be_price_values[be_idx]
  be_str <- str_pad(be, 2, pad = "0")
  
  # MESSAGE naming convention for bioenergy
  be_o <- paste0("BIO", be_str)
  
  # Loop over GHG prices
  for (ghg_idx in seq_along(ghg_price_values)) {
    ghg <- ghg_price_values[ghg_idx]
    ghg_str <- str_pad(ghg, 4, pad = "0")
    
    # MESSAGE naming convention for GHG
    ghg_o <- paste0("GHG", str_pad(ghg, 3, pad = "0"))
    
    # Construct run folder name: SSP2_BD00_BE{xx}_G{yyyy}_demand
    run_folder <- paste0(scenario_name, "_BE", be_str, "_G", ghg_str, run_suffix_demand)
    
    # GDX file path
    gdx <- file.path(base_output_dir, run_folder, "fulldata.gdx")
    bii_path <- file.path(base_output_dir, run_folder)
    gdx_check <- file.path(base_output_dir, run_folder, check_file)
    lock_file <- file.path(base_output_dir, run_folder, ".lock")
    
    # Output file names (individual raw and map files per scenario)
    model <- "MAgPIE"
    ssp <- "SSP2"
    scen <- paste0(ssp, be_o, ghg_o)
    
    # Raw files are in scenario-specific subfolder (e.g. SSP2_BD-none/)
    of_raw <- file.path(matrix_output_dir, paste0(scenario_name, "_BE", be_str, "_GHG", str_pad(ghg, 3, pad = "0"), "raw.csv"))
    of_map <- file.path(matrix_output_dir, paste0(scenario_name, "_BE", be_str, "_GHG", str_pad(ghg, 3, pad = "0"), "map.csv"))
    
    ### Read run data (raw phase)
    if ("raw" %in% phases) {
      # Check if raw file already has the emissions fix (has Emissions|GHG|AFOLU variable)
      raw_file_needs_regeneration <- TRUE
      if (file.exists(of_raw)) {
        tryCatch({
          a_check <- read.report(of_raw, as.list = FALSE)
          check_vars <- getNames(a_check)
          if ("Emissions|GHG|AFOLU (Mt CO2e/yr)" %in% check_vars) {
            # Check if it has data
            ghg_data <- a_check[, , "Emissions|GHG|AFOLU (Mt CO2e/yr)"]
            sample_values <- as.vector(ghg_data)[1:min(5, length(ghg_data))]
            if (!all(sample_values == 0, na.rm = TRUE)) {
              raw_file_needs_regeneration <- FALSE
              if (!reduced_messages) {
                message(run_folder, ": raw file already has fixed emissions. Skipped")
              }
            }
          }
        }, error = function(e) {
          # If we can't read it, assume it needs regeneration
          raw_file_needs_regeneration <<- TRUE
        })
      }
      
      if (
        raw_file_needs_regeneration && file.exists(gdx_check) &&
        file.exists(gdx) && file.size(gdx_check) > 3000000 &&
        !file.exists(lock_file)) {

        file.create(lock_file)
        loop_wait <- 0

        message("Start report gdx= ", run_folder, "...")
        a <- mbind(getReportMESSAGE(gdx, bii_path = bii_path, baseyear = baseyear))
        
        # Read emissions from report.mif instead of using reportEmissions() output
        # (reportEmissions() returns zeros due to lowpass filter issues)
        message("Reading emissions from report.mif file...")
        report_mif <- file.path(base_output_dir, run_folder, "report.mif")
        if (file.exists(report_mif)) {
          # Read report.mif as CSV to get actual values (read.report() has issues)
          mif_data <- normalize_mif_columns(
            read.csv(report_mif, sep = ";", stringsAsFactors = FALSE)
          )
          
          # Get list of emissions variables from mapping file
          mapping <- mapping_df
          emissions_vars_with_units <- unique(mapping$piam_variable[grepl("^Emissions", mapping$piam_variable, ignore.case = TRUE)])
          emissions_vars_with_units <- emissions_vars_with_units[!grepl("^ZERO", emissions_vars_with_units, ignore.case = TRUE)]
          
          # Strip units from mapping variable names for matching (report.mif has units in separate column)
          strip_units_from_var <- function(var_name) {
            gsub("\\s*\\([^)]*\\)$", "", var_name)
          }
          emissions_vars <- strip_units_from_var(emissions_vars_with_units)
          
          # Extract emissions data from report.mif (match variable names without units)
          emissions_data <- mif_data[mif_data$Variable %in% emissions_vars, ]
          
          if (nrow(emissions_data) > 0) {
            # Get year columns
            year_cols <- grep("^X?[0-9]{4}$", names(emissions_data), value = TRUE)
            year_names <- gsub("^X", "", year_cols)
            
            # Create magpie object using as.magpie (more efficient)
            # First, reshape data: Region x Variable x Year
            regions_mif <- unique(emissions_data$Region)
            # Convert "World" to "GLO" to match getReportMESSAGE() output
            emissions_data$Region[emissions_data$Region == "World"] <- "GLO"
            regions <- unique(emissions_data$Region)
            
            # Add units to variable names to match format expected by mapping
            vars_with_units <- paste0(emissions_data$Variable, " (", emissions_data$Unit, ")")
            unique_vars <- unique(vars_with_units)
            
            # Get regions and years from existing object 'a' to ensure compatibility
            a_regions <- getRegions(a)
            a_years <- getYears(a)
            
            # Create empty magpie object with same structure as 'a'
            emissions_magpie <- new.magpie(
              cells_and_regions = a_regions,
              years = a_years,
              names = unique_vars,
              fill = 0
            )
            
            # Fill in values (only for regions that exist in emissions_data)
            for (i in 1:nrow(emissions_data)) {
              var_with_unit <- vars_with_units[i]
              region <- emissions_data$Region[i]
              if (region %in% a_regions) {
                values <- as.numeric(emissions_data[i, year_cols])
                # Handle N/A values
                values[is.na(values)] <- 0
                # Match years
                year_indices <- match(paste0("y", year_names), a_years)
                valid_years <- !is.na(year_indices)
                if (any(valid_years)) {
                  emissions_magpie[region, a_years[year_indices[valid_years]], var_with_unit] <- values[valid_years]
                }
              }
            }
            
            # Remove emissions variables from getReportMESSAGE() output
            # Try to get variable names - handle different magpie object structures
            tryCatch({
              a_vars <- getNames(a, dim = "variable")
            }, error = function(e) {
              # If that fails, try getting all names
              a_vars <<- getNames(a)
            })
            emissions_in_a <- a_vars[grepl("^Emissions", a_vars, ignore.case = TRUE)]
            if (length(emissions_in_a) > 0) {
              a <- a[, , emissions_in_a, invert = TRUE]
              message("Removed ", length(emissions_in_a), " emissions variables from getReportMESSAGE() output")
            }
            
            # Add emissions from report.mif
            a <- mbind(a, emissions_magpie)
            message("Added ", length(unique_vars), " emissions variables from report.mif")
            
            # Create aggregated Emissions|GHG|AFOLU variable
            # Get base emissions variables
            co2_var <- "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
            ch4_var <- "Emissions|CH4|Land (Mt CH4/yr)"
            n2o_var <- "Emissions|N2O|Land (Mt N2O/yr)"
            
            if (co2_var %in% getNames(emissions_magpie) && 
                ch4_var %in% getNames(emissions_magpie) && 
                n2o_var %in% getNames(emissions_magpie)) {
              
              co2_data <- emissions_magpie[, , co2_var] * 1      # factor = 1
              ch4_data <- emissions_magpie[, , ch4_var] * 25     # factor = 25
              n2o_data <- emissions_magpie[, , n2o_var] * 0.285  # factor = 0.285
              
              # Aggregate: CO2 + CH4 + N2O (all in Mt CO2e/yr)
              ghg_afolu <- co2_data + ch4_data + n2o_data
              getNames(ghg_afolu) <- "Emissions|GHG|AFOLU (Mt CO2e/yr)"
              
              # Add to output
              a <- mbind(a, ghg_afolu)
              message("Created aggregated Emissions|GHG|AFOLU variable")
            } else {
              missing <- c()
              if (!co2_var %in% getNames(emissions_magpie)) missing <- c(missing, "CO2")
              if (!ch4_var %in% getNames(emissions_magpie)) missing <- c(missing, "CH4")
              if (!n2o_var %in% getNames(emissions_magpie)) missing <- c(missing, "N2O")
              warning("Could not create Emissions|GHG|AFOLU: missing variables: ", paste(missing, collapse=", "))
            }
          } else {
            warning("No emissions data found in report.mif for variables in mapping file")
          }
        } else {
          warning("report.mif file not found, using emissions from getReportMESSAGE() (may be zeros)")
        }

        if (correct_emissions) {
          ### get emissions pre correction
          # Note: In new format, units are in separate column, so variable names don't include units
          lu_ch4 <- a[, , "Emissions|CH4|Land"]
          lu_co2 <- a[, , "Emissions|CO2|Land"]
          lu_n2o <- a[, , "Emissions|N2O|Land"]

          ### correct emissions trajectories on regional level

          lu_ch4_cor <- lu_ch4["GLO", , , invert = TRUE]
          lu_co2_cor <- lu_co2["GLO", , , invert = TRUE]
          lu_n2o_cor <- lu_n2o["GLO", , , invert = TRUE]

          if (ghg_idx > 1) {
              lu_ch4_cor <- flatten_the_curve(lu_ch4_cor, lu_ch4_up)
              lu_co2_cor <- flatten_the_curve(lu_co2_cor, lu_co2_up)
              lu_n2o_cor <- flatten_the_curve(lu_n2o_cor, lu_n2o_up)
          }
          # upper bounds for next GHG price category
          lu_ch4_up <- lu_ch4_cor
          lu_co2_up <- lu_co2_cor
          lu_n2o_up <- lu_n2o_cor

          ### re-calculate global level
          lu_ch4_cor <- sum_glo(lu_ch4_cor)
          lu_co2_cor <- sum_glo(lu_co2_cor)
          lu_n2o_cor <- sum_glo(lu_n2o_cor)

          ### adjust subcategories
          n <- getNames(a[, , "Emissions|CH4|Land|", pmatch = TRUE])
          lu_ch4_sub <- a[, , n] * lu_ch4_cor / lu_ch4
          lu_ch4_sub <- setNames(collapseDim(lu_ch4_sub), n)

          n <- getNames(
              a[, , "Emissions|CO2|Land|",
              pmatch = TRUE
              ]
          )
          lu_co2_sub <- a[, , n] * lu_co2_cor / lu_co2
          lu_co2_sub <- setNames(collapseDim(lu_co2_sub), n)

          n <- getNames(a[, , "Emissions|N2O|Land|", pmatch = TRUE])
          lu_n2o_sub <- a[, , n] * lu_n2o_cor / lu_n2o
          lu_n2o_sub <- setNames(collapseDim(lu_n2o_sub), n)

          lu_emis_cor <- mbind(
              lu_ch4_cor,
              lu_co2_cor,
              lu_n2o_cor,
              lu_ch4_sub,
              lu_co2_sub,
              lu_n2o_sub
          )

          a <- a[, , getNames(lu_emis_cor), invert = TRUE]
          a <- mbind(a, lu_emis_cor)
        }

        ### Price
        regions <- getRegions(a["GLO", , , invert = TRUE])
        years <- getYears(a)

        # bioenergy prices from price-driven run
        if (be != 0) {
          price_run_folder <- paste0(scenario_name, "_BE", be_str, "_", run_suffix_price)
          price_gdx <- file.path(base_output_dir, price_run_folder, "fulldata.gdx")
          
          price_bio <- readGDX(
              price_gdx,
              "i60_2ndgen_bioenergy_subsidy", react = "silent"
          )[, years, ]
          price_bio <- add_columns(
              price_bio,
              addnm = regions,
              dim = 1,
              fill = NA
          )
          price_bio[, , ] <- price_bio["GLO", , ]
          # Updated unit: US$2017 instead of US$05
          getNames(price_bio) <- "Prices|Bioenergy"
        }

        # emissions prices
        price_emis_co2 <- readGDX(
          gdx, "p56_pollutant_prices_input",
          react = "silent")[, years, "co2_c.peatland"] / 44 * 12
        price_emis_co2 <- add_columns(
          price_emis_co2, addnm = "GLO", dim = 1, fill = NA
        )
        price_emis_co2[, , ] <- price_emis_co2["LAM", , ]
        # Updated unit: US$2017 instead of US$2005
        getNames(price_emis_co2) <- "Prices|GHG Emission|CO2"

        if (be != 0) { prices <- mbind(price_bio, price_emis_co2) }
        else { prices <- price_emis_co2 }

        a <- a[, , getNames(prices), invert = TRUE]
        a <- mbind(a, prices)


        ### MP split (if needed - adjust based on your requirements)
        # Note: This section may need adjustment based on your new structure
        # For now, keeping the structure but you may need to adapt it
        # Load a reduced reporting (food demand only) from a baseline run
        # Baseline run folder (assuming BE=0, same GHG price)
        baseline_run_folder <- paste0(scenario_name, "_BE00_G", ghg_str, run_suffix_demand)
        baseline_gdx <- file.path(base_output_dir, baseline_run_folder, "fulldata.gdx")
        
        if (file.exists(baseline_gdx)) {
          f <- mbind(getReportMESSAGE(baseline_gdx, food_only = TRUE, baseyear = baseyear))

          # Get Delta between baseline and current for beef and dairy
          # Updated variable names: units removed
          d <- "Demand|Food|Livestock products|+|Ruminant meat"
          if (d %in% getNames(a, fulldim = TRUE)$variable) {
            beef_base <- f[,,d]
            beef <- a[,,d]
            beef_delta <- beef_base - beef
            getNames(beef_base) <- "Demand|Food|Livestock products|Ruminant meat|Baseline"
            getNames(beef_delta) <- "Demand|Food|Livestock products|Ruminant meat|Replaced"

            d <- "Demand|Food|Livestock products|+|Dairy"
            dairy_base <- f[,,d]
            dairy <- a[,,d]
            dairy_delta <- dairy_base - dairy
            getNames(dairy_base) <- "Demand|Food|Livestock products|Dairy|Baseline"
            getNames(dairy_delta) <- "Demand|Food|Livestock products|Dairy|Replaced"

            # Get protein content of beef, dairy, MP
            prot_beef <- readGDX(gdx, "f15_nutrition_attributes",
              react = "silent")[, years, "livst_rum.protein"]
            prot_dairy <- readGDX(gdx, "f15_nutrition_attributes",
              react = "silent")[, years, "livst_milk.protein"]
            prot_MP <- readGDX(gdx, "f15_nutrition_attributes",
              react = "silent")[, years, "scp.protein"]

            # Multiply by delta and MP tonnage respectively
            # Get share of delta protein of total MP Protein
            # Share (beef, dairy protein) * MP tonnage = tonnage MP (beef, dairy)
            beef_mp  <- beef_delta  * prot_beef  / prot_MP 
            dairy_mp <- dairy_delta * prot_dairy / prot_MP

            # Rename magpie objects (units removed)
            getNames(beef_mp) <- "Demand|Food|Secondary products|Microbial protein|+|Ruminant meat"
            getNames(dairy_mp) <- "Demand|Food|Secondary products|Microbial protein|+|Dairy"

            a <- mbind(a, beef_base, dairy_base, beef_delta, dairy_delta, beef_mp, dairy_mp)
          }
        }

        # Read missing MAgPIE variables from report.mif (extending emissions logic to all variables)
        # Check which MAgPIE variables from mapping file are missing after getReportMESSAGE() and emissions handling
        message("Checking for missing MAgPIE variables from mapping file...")
        if (file.exists(report_mif)) {
          # Read mapping file to get all MAgPIE-side variables
          mapping <- mapping_df
          # Get all unique MAgPIE variables from mapping (excluding ZERO entries)
          mapping_magpie_vars_with_units <- unique(mapping$piam_variable)
          mapping_magpie_vars_with_units <- mapping_magpie_vars_with_units[!grepl("^ZERO", mapping_magpie_vars_with_units, ignore.case = TRUE)]
          
          # Get variables currently in 'a' (after getReportMESSAGE and emissions handling)
          # Handle different magpie object structures
          tryCatch({
            a_vars <- getNames(a, dim = "variable")
          }, error = function(e) {
            # If that fails, try getting all names and extract variable dimension
            all_names <- getNames(a, fulldim = TRUE)
            if ("variable" %in% names(all_names)) {
              a_vars <- all_names$variable
            } else {
              # Last resort: get all names and assume they're variable names
              a_vars <- getNames(a)
            }
          })
          
          # Strip units for comparison
          strip_units_from_var <- function(var_name) {
            gsub("\\s*\\([^)]*\\)$", "", var_name)
          }
          mapping_vars_no_units <- strip_units_from_var(mapping_magpie_vars_with_units)
          a_vars_no_units <- strip_units_from_var(a_vars)
          
          # Find missing variables (in mapping but not in 'a')
          missing_vars_no_units <- mapping_vars_no_units[!mapping_vars_no_units %in% a_vars_no_units]
          
          if (length(missing_vars_no_units) > 0) {
            message("Found ", length(missing_vars_no_units), " missing MAgPIE variables. Reading from report.mif...")
            
            # Read report.mif if not already read (for emissions)
            if (!exists("mif_data") || is.null(mif_data)) {
              mif_data <- normalize_mif_columns(
                read.csv(report_mif, sep = ";", stringsAsFactors = FALSE)
              )
            } else {
              mif_data <- normalize_mif_columns(mif_data)
            }
            
            # Extract missing variables from report.mif (match variable names without units)
            missing_data <- mif_data[mif_data$Variable %in% missing_vars_no_units, ]
            missing_data <- drop_mif_na_unit_duplicates(missing_data)
            
            if (nrow(missing_data) > 0) {
              missing_data$Region[missing_data$Region == "World"] <- "GLO"
              
              # Get regions and years from existing object 'a' to ensure compatibility
              a_regions <- getRegions(a)
              a_years <- getYears(a)
              
              # Use bare variable names (no units in name) to match getReportMESSAGE() output
              unique_vars <- unique(missing_data$Variable)
              present_stripped <- strip_units(a_vars)
              unique_vars <- unique_vars[
                !strip_units(unique_vars) %in% present_stripped
              ]
              
              if (length(unique_vars) > 0) {
                missing_magpie <- fill_magpie_from_mif(
                  missing_data[missing_data$Variable %in% unique_vars, , drop = FALSE],
                  a_regions,
                  a_years,
                  unique_vars
                )
                
                # Add missing variables to output
                a <- mbind(a, missing_magpie)
                message("Added ", length(unique_vars), " missing variables from report.mif")
              } else {
                message("All missing variables already present after name normalization")
              }
            } else {
              message("No data found in report.mif for ", length(missing_vars_no_units), " missing variables")
            }
          } else {
            message("All MAgPIE variables from mapping file are present in raw data")
          }
        } else {
          warning("report.mif file not found, cannot read missing variables")
        }

        ### Add Filler Zero object for mapping
        z <- new.magpie(
          getRegions(a), getYears(a), names = "ZERO", fill = 0
        )
        a <- mbind(a, z)

        # Write raw files for mapping
        print(paste0("writing to ", of_raw))
        write.report(
            a,
            file = of_raw,
            model = model,
            scenario = scen,
            ndigit = 10,
            append = FALSE,
            skipempty = FALSE
        )
        print("raw written")
        file.remove(lock_file)
      } else if (file.exists(of_raw) && !file.exists(lock_file)) {
        # Check if raw file needs missing variables from report.mif
        # Use lock file to prevent parallel processing of same scenario
        file.create(lock_file)
        message("Raw file exists. Checking for missing variables from mapping file...")
        report_mif <- file.path(base_output_dir, run_folder, "report.mif")
        
        if (file.exists(report_mif)) {
          # Read existing raw file
          a_existing <- read.report(of_raw, as.list = FALSE)
          a_existing_vars <- getNames(a_existing, dim = "variable")
          
          # Read mapping file to get all MAgPIE-side variables
          mapping <- mapping_df
          mapping_magpie_vars_with_units <- unique(mapping$piam_variable)
          mapping_magpie_vars_with_units <- mapping_magpie_vars_with_units[!grepl("^ZERO", mapping_magpie_vars_with_units, ignore.case = TRUE)]
          
          # Strip units for comparison
          strip_units_from_var <- function(var_name) {
            gsub("\\s*\\([^)]*\\)$", "", var_name)
          }
          mapping_vars_no_units <- strip_units_from_var(mapping_magpie_vars_with_units)
          existing_vars_no_units <- strip_units_from_var(a_existing_vars)
          
          # Find missing variables
          missing_vars_no_units <- mapping_vars_no_units[!mapping_vars_no_units %in% existing_vars_no_units]
          
          if (length(missing_vars_no_units) > 0) {
            message("Found ", length(missing_vars_no_units), " missing variables. Reading from report.mif...")
            
            # Read report.mif
            mif_data <- normalize_mif_columns(
              read.csv(report_mif, sep = ";", stringsAsFactors = FALSE)
            )
            
            # Extract missing variables from report.mif
            missing_data <- mif_data[mif_data$Variable %in% missing_vars_no_units, ]
            missing_data <- drop_mif_na_unit_duplicates(missing_data)
            
            if (nrow(missing_data) > 0) {
              missing_data$Region[missing_data$Region == "World"] <- "GLO"
              
              # Get regions and years from existing object
              a_regions <- getRegions(a_existing)
              a_years <- getYears(a_existing)
              
              unique_vars <- unique(missing_data$Variable)
              present_stripped <- strip_units(a_existing_vars)
              unique_vars <- unique_vars[
                !strip_units(unique_vars) %in% present_stripped
              ]
              
              if (length(unique_vars) > 0) {
                missing_magpie <- fill_magpie_from_mif(
                  missing_data[missing_data$Variable %in% unique_vars, , drop = FALSE],
                  a_regions,
                  a_years,
                  unique_vars
                )
                
                # Append missing variables to existing raw file
                a_existing <- mbind(a_existing, missing_magpie)
                
                # Rewrite raw file with missing variables
                write.report(
                  a_existing,
                  file = of_raw,
                  model = model,
                  scenario = scen,
                  ndigit = 10,
                  append = FALSE,
                  skipempty = FALSE
                )
                message("Appended ", length(unique_vars), " missing variables to existing raw file")
              } else {
                message("All missing variables already present after name normalization")
              }
            } else {
              message("No data found in report.mif for missing variables")
            }
          } else {
            if (!reduced_messages) {
              message(run_folder, ": raw file already exists with all required variables. Skipped")
            }
          }
        } else {
          if (!reduced_messages) {
            message(run_folder, ": raw file already exists. Skipped (report.mif not found for missing variable check)")
          }
        }
        # Remove lock file after processing
        if (file.exists(lock_file)) {
          file.remove(lock_file)
        }
      } else if (file.exists(lock_file)) {
          message(run_folder, ": folder locked by other script. Skipped")
      } else {
        message(
          run_folder, ": run not started, reporting not finished yet, or infeasible, check log. Skipped."
        )
        message("Fulldata exists: ", file.exists(gdx),
        "; reporting finished: ", file.exists(gdx_check))
        if (file.exists(gdx_check)) {
          message(gdx_check, " size: ", file.size(gdx_check) / 1024, " kb")
        }
      }
    } else { # end raw phase
      if ("raw" %in% phases) {
        message("raw phase skipped")
      }
    }

    ### Read raw files and remap them
    if (("map" %in% phases || "matrix" %in% phases) && !loop) {
      if (file.exists(of_raw)) {
        message(run_folder, ": remap and merge into ", ofile)

        a_raw <- read.report(file = of_raw, as.list = FALSE)
        # Check if mapping file exists and has content
        # Try to write mapped report
        a_mapped <- write.reportProject(a_raw, mapping = map_file_for_project, file = of_map)
        if (!file.exists(of_map)) {
          # If map file wasn't created, check if the mapped object has data
          if (is.null(a_mapped) || length(a_mapped) == 0) {
            stop("Map file was not created: ", of_map, ". Mapping resulted in empty object. Check mapping file and variable names.")
          }
          # If object exists but file wasn't written, try to write it manually
          write.report(a_mapped, file = of_map)
        }
        a <- read.report(file = of_map, as.list = FALSE)
        
        # Preserve unmapped variables that exist in raw files and are in old matrix
        # Get variable names from raw and mapped data
        raw_vars <- getNames(a_raw, dim = "variable")
        mapped_vars <- getNames(a, dim = "variable")
        
        # Read mapping file to check which raw variables were mapped
        mapping <- mapping_df
        mapped_raw_vars <- unique(mapping$piam_variable)
        
        # Strip units from raw variable names for comparison (read.report adds units)
        raw_vars_stripped <- strip_units(raw_vars)
        mapped_raw_vars_stripped <- strip_units(mapped_raw_vars)
        
        # Find truly unmapped variables (not in mapping file at all)
        unmapped_vars <- raw_vars[!raw_vars_stripped %in% mapped_raw_vars_stripped]
        
        # Check if unmapped variables match old matrix variables (strip units for comparison)
        vars_to_keep_stripped <- strip_units(vars_to_keep)
        unmapped_vars_stripped <- strip_units(unmapped_vars)
        
        # Keep only unmapped variables that are in old matrix
        unmapped_to_keep <- unmapped_vars[unmapped_vars_stripped %in% vars_to_keep_stripped]
        
        if (length(unmapped_to_keep) > 0) {
          message("Preserving ", length(unmapped_to_keep), " unmapped variable(s) from old matrix: ", paste(head(unmapped_to_keep, 5), collapse = ", "), if(length(unmapped_to_keep) > 5) "..." else "")
          # Extract unmapped variables from raw data
          a_unmapped <- a_raw[, , unmapped_to_keep]
          # Combine mapped and unmapped variables
          a <- mbind(a, a_unmapped)
        }
        
        # Filter to only keep variables from mapping-derived vars_to_keep list
        # Get variable names from mapped data (with units from read.report)
        all_vars <- getNames(a, dim = "variable")
        all_vars_stripped <- strip_units(all_vars)
        vars_to_keep_stripped <- strip_units(vars_to_keep)
        
        # Find variables that match our keep list
        vars_to_keep_indices <- all_vars_stripped %in% vars_to_keep_stripped
        vars_kept <- all_vars[vars_to_keep_indices]
        vars_removed <- all_vars[!vars_to_keep_indices]
        
        if (length(vars_removed) > 0) {
          message("Filtering: keeping ", length(vars_kept), " variables, removing ", length(vars_removed), " variables")
        }
        
        # Filter the magpie object to only keep desired variables
        if (length(vars_kept) > 0) {
          a <- a[, , vars_kept]
        } else {
          warning("No variables matched vars_to_keep list for scenario ", scen)
          next
        }
        
        # Note: Missing variables that don't exist in raw files may be added in matrix phase
        # (ZERO piam entries in mapping still appear in vars_to_keep as IAMC targets)

        # Add additional scenario info
        a <- add_dimension(a, dim = 3.1, add = "SSPscen", nm = ssp)
        a <- add_dimension(a, dim = 3.1, add = "GHGscen", nm = ghg_o)
        a <- add_dimension(a, dim = 3.1, add = "BIOscen", nm = be_o)
        a <- add_dimension(
          a,
          dim = 3.1,
          add = "SDGscen",
          nm = "noSDG_rcpref"
        )

        write.report(
            a,
            file = ofile,
            model = model,
            scenario = scen,
            ndigit = 10,
            append = TRUE,
            skipempty = FALSE,
            extracols = c("SSPscen", "GHGscen", "BIOscen", "SDGscen")
        )

      } else {
        message(of_raw, " does not exist. Skipped")
      }
    } #end map phase

  } # close GHG loop
} # close BE loop

# Matrix phase (final formatting) - process single premap file after all loops
if ("matrix" %in% phases && !loop) {
  if (file.exists(ofile)) {
    ### Edit final input file
    a <- read.csv(file = ofile, sep = ";")

    #Rename regions
    rename <- hash()
    i <- c(
    "AFR", "CHA", "CPA", "EEU", "FSU", "LAM", "MEA",
    "NAM", "PAO", "PAS", "SAS", "WEU", "GLO"
    )
    j <- c(
    "SubSaharanAfrica", "ChinaReg", "PlannedAsiaChina", "CentralEastEurope",
    "FormerSovietUnion", "LatinAmericaCarib", "MidEastNorthAfrica",
    "NorthAmerica", "PacificOECD", "OtherPacificAsia",
    "SouthAsia", "WesternEurope", "World"
    )

    for (k in seq_along(i)) {
    rename[[i[k]]] <- j[k]
    }

    for (i in keys(rename)){
    a[a == i] <- rename[[i]]
    }

    # Remove unnecessary columns (Model, Scenario, empty last column)
    a <- a[, 3:(length(a) - 1)]

    # Fix read-in issue with numeric columns
    names(a) <- gsub("X", "", names(a))

    ### Re-order named Columns
    c <- c(
    "Region", "Variable", "Unit",
    "SSPscen", "GHGscen", "BIOscen", "SDGscen"
    )
    # Get annual columns
    n <- names(a)
    n <- n[!(n %in% c)]
    c <- append(c, n)

    # Apply new order
    a <- a[, c]
    
    # Add missing mapping targets as zero-filled rows (IAMC Variable not produced by map step)
    old_vars <- vars_to_keep
    new_vars <- unique(a$Variable)
    missing_vars <- old_vars[!old_vars %in% new_vars]
    
    if (length(missing_vars) > 0) {
      message("Adding ", length(missing_vars), " missing variables as zero-filled rows: ", paste(head(missing_vars, 3), collapse = ", "), if(length(missing_vars) > 3) "..." else "")
      
      # Get template row structure (use first row as template)
      template_row <- a[1, ]
      
      # Get all unique scenario combinations
      scenario_cols <- c("SSPscen", "GHGscen", "BIOscen", "SDGscen")
      scenario_combos <- unique(a[, scenario_cols, drop = FALSE])
      
      # Get all regions
      regions <- unique(a$Region)
      
      # Get year columns (numeric columns)
      year_cols <- names(a)[!names(a) %in% c("Region", "Variable", "Unit", scenario_cols)]
      
      # Create zero-filled rows for each missing variable
      zero_rows <- list()
      for (var_name in missing_vars) {
        # Extract unit from var_name if present
        # All variables in vars_to_keep should have units in parentheses (from mapping Variable column)
        if (grepl("\\(", var_name)) {
          var_base <- gsub("\\s*\\([^)]*\\)", "", var_name)
          var_unit <- gsub(".*\\(([^)]+)\\).*", "\\1", var_name)
          # Skip if unit is "N/A" - these shouldn't be added
          if (var_unit == "N/A") {
            next
          }
        } else {
          # If no unit found, skip this variable (shouldn't happen if vars_to_keep is correct)
          warning("Variable ", var_name, " has no unit in vars_to_keep list, skipping zero-filled row creation")
          next
        }
        
        # Create rows for each region-scenario combination
        for (r in 1:nrow(scenario_combos)) {
          for (reg in regions) {
            new_row <- template_row[1, , drop = FALSE]
            new_row$Region <- reg
            new_row$Variable <- var_base
            new_row$Unit <- var_unit
            new_row[, scenario_cols] <- scenario_combos[r, ]
            new_row[, year_cols] <- 0
            zero_rows[[length(zero_rows) + 1]] <- new_row
          }
        }
      }
      
      # Combine zero rows with existing data
      if (length(zero_rows) > 0) {
        zero_df <- do.call(rbind, zero_rows)
        a <- rbind(a, zero_df)
        message("Added ", nrow(zero_df), " zero-filled rows for missing variables")
      }
    }
    
    # Remove rows with "N/A" units (duplicates with proper units should be kept)
    na_rows_before <- sum(a$Unit == "N/A")
    if (na_rows_before > 0) {
      message("Removing ", na_rows_before, " rows with N/A units")
      a <- a[a$Unit != "N/A", ]
    }
    
    # Remove variables that are all zero across all scenarios, regions, and years
    year_cols <- names(a)[grepl("^X?[0-9]+$", names(a))]
    if (length(year_cols) > 0) {
      vars <- unique(a$Variable)
      all_zero_vars <- character(0)
      for (var in vars) {
        var_data <- a[a$Variable == var, year_cols, drop = FALSE]
        var_data_num <- as.matrix(var_data)
        mode(var_data_num) <- "numeric"
        all_zero <- all(var_data_num == 0 | is.na(var_data_num), na.rm = TRUE)
        if (all_zero) {
          all_zero_vars <- c(all_zero_vars, var)
        }
      }
      if (length(all_zero_vars) > 0) {
        message("Removing ", length(all_zero_vars), " all-zero variables: ", paste(head(all_zero_vars, 5), collapse = ", "), if(length(all_zero_vars) > 5) "..." else "")
        a <- a[!a$Variable %in% all_zero_vars, ]
      }
    }

    # Final check: remove any remaining rows with "N/A" units (safety check)
    na_rows_after <- sum(a$Unit == "N/A")
    if (na_rows_after > 0) {
      message("WARNING: Found ", na_rows_after, " rows with N/A units after cleanup, removing them")
      a <- a[a$Unit != "N/A", ]
    }

    # And write!
    message(ofile, ": written into Matrix file ", matrix_file)

    write.csv(
        a,
        file = matrix_file,
        row.names = FALSE,
        quote = c(1, 2, 3, 4, 5, 6, 7)
    )
  } else {
    message("Premap file ", ofile, " does not exist. Matrix phase skipped.")
  }
} # end matrix phase

if (loop && (loop_wait < 30)) {
  message("Loop ended without new raw file. Waiting ", loop_wait, " minutes")
  Sys.sleep(loop_wait * 60)
  loop_wait <- loop_wait + 3 
} else if (loop) {
  message("Loop ended without new raw file and maximum wait time reached!")
  loop_wait <- loop_wait + 5
} else {
  message("Nothing to loop here. All done for now")
}

