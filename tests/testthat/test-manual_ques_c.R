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
source("./04_quesc/rscript/function_ques_c.R")


# Define required packages
required_packages <- c(
  "terra", "dplyr", "ggplot2", "sf", # Core spatial/data analysis
  "rmarkdown", "bslib", # Reporting
  "readr", "tidyr", "networkD3", # Data wrangling & visualization
  "stringr", "scales", "purrr", "LUMENSR"
)

# Install missing packages quietly
check_and_install_packages(required_packages)

# Input Data Setup ------------------------------------------------------------
# Simulate Shiny file inputs structure for compatibility with preques functions
# Note: These paths would typically come from user uploads in Shiny apps

# Land cover maps
lc_t1_input <- list(
  datapath = "data/raster/bungo_landcover_1990r.tif", # Time 1 land cover
  name = "bungo_landcover_1990r"
)

t1 <- 1990

lc_t2_input <- list(
  datapath = "data/raster/bungo_landcover_2000r.tif", # Time 2 land cover
  name = "bungo_landcover_2000r"
)

t2 <- 2000

# Administrative zones
admin_z_input <- list(
  datapath = "data/raster/bungo_zone.tif", # Zoning raster
  name = "bungo_zone"
)

# Lookup tables
zone_lookup_input <- list(
  datapath = "./data/table/zone_table_bungo.csv" # Zone-class crosswalk
)



c_lookup_input <- list(
  datapath = "./data/table/carbon_bungo.csv" # Land cover class definitions
)


# Output Configuration --------------------------------------------------------
outputPath <- "output/ques_c_1990_2000" %>% normalizePath(mustWork = FALSE) # All outputs will be saved here
dir.create(outputPath, showWarnings = FALSE) # Create folder if missing

# Run Analysis ----------------------------------------------------------------
ques_c_results <- run_quesc_analysis(
  lc_t1_path = lc_t1_input$datapath,
  lc_t2_path = lc_t2_input$datapath,
  admin_z_path = admin_z_input$datapath,
  c_lookup_path = c_lookup_input$datapath,
  zone_lookup_path = zone_lookup_input$datapath,
  time_points = list(t1 = t1, t2 = t2),
  output_dir = outputPath
)
