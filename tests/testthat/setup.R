# Setup for LaSEM tests
# Load fixtures and helper functions

library(terra)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)

# Source LaSEM functions
source(testthat::test_path("../../12_lasem/rscript/functions_analysis.R"))
source(testthat::test_path("../../12_lasem/rscript/functions_io.R"))
source(testthat::test_path("../../12_lasem/rscript/functions_validation.R"))

fixture_dir <- testthat::test_path("fixtures")
raster_dir <- file.path(fixture_dir, "rasters")

# Helper to load test fixtures
load_test_fixtures <- function() {
  list(
    raster_inputs = readr::read_csv(file.path(fixture_dir, "raster_inputs.csv")),
    crop_suitability = readr::read_csv(file.path(fixture_dir, "crop_suitability.csv")),
    intervention = readr::read_csv(file.path(fixture_dir, "intervention.csv"))
  )
}

# Helper to create a test SpatRaster from fixture paths
load_test_rasters <- function(raster_inputs) {
  paths <- file.path(raster_dir, basename(raster_inputs$raster_path))
  rasters <- lapply(paths, terra::rast)
  stacked <- terra::rast(rasters)
  names(stacked) <- raster_inputs$name_parameter
  stacked
}
