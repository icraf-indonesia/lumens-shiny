# ------------------------------------------------------------------------------
# PRE-QUES Land Change Analysis Script
# ------------------------------------------------------------------------------
# This script performs a pre-questionnaire (PRE-QUES) land change analysis 
# between two time points (1990 and 2000) for the Bungo region. It calculates:
# - Land cover changes
# - Trajectories of loss/recovery
# - Zonal statistics for administrative regions
# Outputs include raster maps, transition matrices, and interactive reports.
# ------------------------------------------------------------------------------

# Load Helper Functions & Check Packages --------------------------------------
# Source custom functions for PRE-QUES analysis
source("03_preques/rscript/functions_ques_pre.R")

# Define required packages
required_packages <- c(
  "terra", "dplyr", "ggplot2", "sf",  # Core spatial/data analysis
  "rmarkdown", "bslib",               # Reporting
  "readr", "tidyr", "networkD3",      # Data wrangling & visualization
  "stringr", "scales", "purrr"        
)

# Install missing packages quietly
check_and_install_packages(required_packages)

# Input Data Setup ------------------------------------------------------------
# Simulate Shiny file inputs structure for compatibility with preques functions
# Note: These paths would typically come from user uploads in Shiny apps

# Land cover maps
lc_t1_input <- list(
  datapath = "data/raster/bungo_landcover_1990r.tif",  # Time 1 land cover
  name = "bungo_landcover_1990r"
)

lc_t2_input <- list(
  datapath = "data/raster/bungo_landcover_2000r.tif",  # Time 2 land cover
  name = "bungo_landcover_2000r"
)

# Administrative zones
admin_z_input <- list(
  datapath = "data/raster/bungo_zone.tif",  # Zoning raster
  name = "bungo_zone"
)

# Lookup tables
zone_lookup_input <- list(
  datapath = "data/table/landuse_table_bungo.csv"  # Zone-class crosswalk
)

trajectory_lookup_input <- list(
  datapath = "data/table/trajectory_loss_recovery.csv"  # Trajectory definitions
)

lc_lookup_input <- list(
  datapath = "data/table/landuse_table_bungo.csv"  # Land cover class metadata
)

# Output Configuration --------------------------------------------------------
outputPath <- "output/ques_pre_1990_2000"  # All outputs will be saved here
dir.create(outputPath, showWarnings = FALSE)  # Create folder if missing

# Run Analysis ----------------------------------------------------------------
pre_ques_results <- run_preques_analysis(
  # Core inputs
  lc_t1_input = lc_t1_input,          # Initial land cover map
  lc_t2_input = lc_t2_input,          # Final land cover map
  admin_z_input = admin_z_input,      # Administrative zones raster
  lc_lookup_input = lc_lookup_input,  # Land cover class definitions
  
  # Zone metadata (if using raster zones)
  zone_lookup_input = zone_lookup_input,
  
  # Trajectory classification rules
  trajectory_lookup_input = trajectory_lookup_input,
  
  # Time points for analysis
  time_points = list(t1 = 1990, t2 = 2000),
  
  # Output location
  output_dir = outputPath
)
