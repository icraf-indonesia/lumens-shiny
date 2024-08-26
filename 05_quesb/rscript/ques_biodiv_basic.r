# Biodiversity Analysis Script
# This script performs land cover analysis and TECI (Total Edge Contrast Index) calculation

# Load required packages ====
library(dplyr)    # For data manipulation
library(terra)    # For raster operations
library(sf)       # For vector data handling
library(DBI)      # For database interface
library(RSQLite)  # For SQLite database operations

# Source custom functions
source("05_quesb/rscript/ques_biodiv_functions.r")

# Specify paths to input files ====
lc_t1_path <- "data/quesb_test/lc_2005.tif"  # Land cover raster for time 1
t1 <- 2005  # Year of the land cover data
lulc_lut_path <- "data/quesb_test/habitat_lookup.csv"  # Land use/land cover lookup table
contab_path <- "data/quesb_test/contrast_euc.fsq"  # Contrast table
output_dir <- "data/quesb_test/"  # Directory for output files

# Load and prepare data ====
# Load land use/land cover lookup table
lulc_lut <- read.csv(lulc_lut_path)

# Set NoData value for rasters
raster.nodata <- 0

# Load and prepare land cover map
lc_t1 <- prepare_lc_data(lc_t1_path,
                         year = t1,
                         lookup_table = lulc_lut)
NAflag(lc_t1) <- raster.nodata  # Set NoData flag

# Generate sampling grid ====
# Create a polygon grid for sampling
sampling_grid <- generate_sampling_grid(lc_t1, n=1000)

# Perform TECI analysis ====
teci_lc_t1 <- teci_analysis(
  landuse = lc_t1,
  output_dir = output_dir,
  classdesc = "data/quesb_test/descriptors.fcd",  # Class descriptor file
  cont_fsq = "data/quesb_test/contrast_euc.fsq",  # Contrast file
  fca = "data/quesb_test/teciuf.fca",  # Fragstats model file
  adjacent_only = 1,  # Consider only adjacent cells
  gridres = 10000,  # Grid resolution
  windowsize = 1000,  # Moving window size
  window.shape = 1,  # Window shape (1 for square)
  raster.nodata = 0,  # NoData value
  fragstats_path = "C:/Program Files/Fragstats 4.3/"  # Path to Fragstats software
)

# Next steps:
# - Analyze the TECI results
# - Visualize the results (e.g., plot(teci_lc_t1))
# - Perform additional biodiversity analyses as needed
