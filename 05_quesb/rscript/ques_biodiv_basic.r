# QuES-B module
# This script perform TECI (Total Edge Contrast Index) calculation and
# calculates DIFA Index

# Load required packages ====
library(dplyr)    # For data manipulation
library(terra)    # For raster operations
library(sf)       # For vector data handling
library(DBI)      # For database interface
library(RSQLite)  # For SQLite database operations
library(ggplot2)

# Source custom functions
source("05_quesb/rscript/ques_biodiv_functions.r")

# Define output directory
output_dir <- "output/quesb"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# run ques-b for lc 1990
qb_1990 <- quesb_single_period(
  lc_t1_path = "data/raster/tutupan_lahan_Bungo_1990r.tif",
  t1 = 1990,  # Year of the land cover data for lc_t1
  raster.nodata = 0, # Set NoData value for rasters
  lulc_lut_path = "data/table/Tabel_focal_area_Bungo.csv",
  contab_path = "data/table/Tabel_edge_contrast_euc.fsq",
  output_dir = output_dir,
  sampling_points = 1000, # n points
  window_size = 1000, # in meters
  window.shape = 1, # circle, if 0 is square
  fca_path = NULL,#path to fragstats fca file( optional)
  fragstats_path = NULL  # Path to Fragstats software (optional)
)

# run ques-b for lc 2000
qb_2000 <- quesb_single_period(
  lc_t1_path = "data/raster/tutupan_lahan_Bungo_2000r.tif",
  t1 = 2000,  # Year of the land cover data for lc_t1
  raster.nodata = 0, # Set NoData value for rasters
  lulc_lut_path = "data/table/Tabel_focal_area_Bungo.csv",
  contab_path = "data/table/Tabel_edge_contrast_euc.fsq",
  output_dir = output_dir,
  sampling_points = 1000, # n points
  window_size = 1000, # in meters
  window.shape = 1, # circle, if 0 is square
  fca_path = NULL,#path to fragstats fca file( optional)
  fragstats_path = NULL  # Path to Fragstats software (optional)
)


# run ques-b for lc 2005
qb_2005 <- quesb_single_period(
  lc_t1_path = "data/raster/tutupan_lahan_Bungo_2005r.tif",
  t1 = 2005,  # Year of the land cover data for lc_t1
  raster.nodata = 0, # Set NoData value for rasters
  lulc_lut_path = "data/table/Tabel_focal_area_Bungo.csv",
  contab_path = "data/table/Tabel_edge_contrast_euc.fsq",
  output_dir = output_dir,
  sampling_points = 1000, # n points
  window_size = 1000, # in meters
  window.shape = 1, # circle, if 0 is square
  fca_path = NULL,#path to fragstats fca file( optional)
  fragstats_path = NULL  # Path to Fragstats software (optional)
)

# run ques-b for lc 2010
qb_2010 <- quesb_single_period(
  lc_t1_path = "data/raster/tutupan_lahan_Bungo_2010r.tif",
  t1 = 2010,  # Year of the land cover data for lc_t1
  raster.nodata = 0, # Set NoData value for rasters
  lulc_lut_path = "data/table/Tabel_focal_area_Bungo.csv",
  contab_path = "data/table/Tabel_edge_contrast_euc.fsq",
  output_dir = output_dir,
  sampling_points = 1000, # n points
  window_size = 1000, # in meters
  window.shape = 1, # circle, if 0 is square
  fca_path = NULL,#path to fragstats fca file( optional)
  fragstats_path = NULL  # Path to Fragstats software (optional)
)

