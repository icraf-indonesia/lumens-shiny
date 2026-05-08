# Generate Synthetic Test Fixtures for LaSEM
# Run this script to create test fixtures in tests/testthat/fixtures/

library(terra)
library(readr)

fixture_dir <- "tests/testthat/fixtures"
raster_dir <- file.path(fixture_dir, "rasters")

create_synthetic_raster <- function(name, values, ncols = 10, nrows = 10) {
  r <- rast(nrows = nrows, ncols = ncols, nlyrs = 1)
  values(r) <- values
  names(r) <- name
  writeRaster(r, file.path(raster_dir, paste0(name, ".tif")), overwrite = TRUE)
  r
}

# Create 10x10 rasters with known value patterns

# soil_ph: values 4.0 to 8.0
# Pattern: top-left 5x5 = 6.0 (S1), top-right 5x5 = 7.5 (S3),
#          bottom-left 5x5 = 5.0 (S2), bottom-right 5x5 = 4.0 (N)
soil_ph_values <- c(
  rep(c(rep(6.0, 5), rep(7.5, 5)), 5),
  rep(c(rep(5.0, 5), rep(4.0, 5)), 5)
)
create_synthetic_raster("soil_ph", soil_ph_values)

# clim_temperature_avg: values 14 to 26
# Pattern: left half = 18 (S1), right half = 25 (S3)
clim_temp_values <- c(
  rep(c(rep(18, 5), rep(25, 5)), 10)
)
create_synthetic_raster("clim_temperature_avg", clim_temp_values)

# soil_depth: values 0 to 200 cm
# Pattern: top half = 150 (S1), bottom half = 30 (S3)
soil_depth_values <- c(
  rep(150, 50),
  rep(30, 50)
)
create_synthetic_raster("soil_depth", soil_depth_values)

# Create raster inputs CSV
raster_inputs <- tibble::tibble(
  ID = 1:3,
  parameter = c("pH tanah", "Suhu rata-rata", "Kedalaman tanah"),
  name_parameter = c("soil_ph", "clim_temperature_avg", "soil_depth"),
  availability = c("Yes", "Yes", "Yes"),
  raster_path = c(
    "tests/testthat/fixtures/rasters/soil_ph.tif",
    "tests/testthat/fixtures/rasters/clim_temperature_avg.tif",
    "tests/testthat/fixtures/rasters/soil_depth.tif"
  )
)
write_csv(raster_inputs, file.path(fixture_dir, "raster_inputs.csv"))

# Create crop suitability parameters CSV (for a generic test crop)
crop_suitability <- tibble::tibble(
  name_common = c(rep("Test Crop", 11)),
  name_sp = c(rep("Testus cropus", 11)),
  class = c("S1", "S2", "S2", "S3", "S3", "N",
            "S1", "S2", "S2", "S3", "N"),
  name_parameter = c(rep("soil_ph", 6), rep("clim_temperature_avg", 5)),
  value = c("5.5-6.5", "5.0-5.5", "6.5-7.0", "4.5-5.0", "7.0-7.5", "<4.5",
            "16-20", "15-16", "20-22", "22-24", ">24"),
  unit = c(rep("pH", 6), rep("°C", 5))
)
write_csv(crop_suitability, file.path(fixture_dir, "crop_suitability.csv"))

# Create intervention lookup CSV
intervention <- tibble::tibble(
  no = 1:2,
  karakteristik_lahan = c("pH tanah", "Suhu rata-rata"),
  name_parameter = c("soil_ph", "clim_temperature_avg"),
  intervention = c(TRUE, FALSE),
  low = c(NA, NA),
  med = c("+", NA),
  high = c("++", NA)
)
write_csv(intervention, file.path(fixture_dir, "intervention.csv"))

message("Test fixtures created successfully in ", fixture_dir)
