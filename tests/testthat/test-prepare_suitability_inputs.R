test_that("prepare_suitability_inputs creates correct lookup and subsets rasters", {
  fixtures <- load_test_fixtures()
  stacked <- load_test_rasters(fixtures$raster_inputs)

  result <- prepare_suitability_inputs(
    suitability_factors = stacked,
    crop_suitability = fixtures$crop_suitability
  )

  # Returns a list with prepared rasters and lookup
  expect_type(result, "list")
  expect_named(result, c("suitability_factors", "lookup_suitability_factors"))

  # Lookup should have IDs 1:n and matching names
  lookup <- result$lookup_suitability_factors
  expect_s3_class(lookup, "data.frame")
  expect_named(lookup, c("ID", "names"))
  expect_equal(nrow(lookup), 2) # soil_ph and clim_temperature_avg match
  expect_equal(lookup$names, c("soil_ph", "clim_temperature_avg"))

  # Raster should be subset to matching layers only
  rasters <- result$suitability_factors
  expect_s4_class(rasters, "SpatRaster")
  expect_equal(terra::nlyr(rasters), 2)
  expect_equal(names(rasters), c("soil_ph", "clim_temperature_avg"))
})

test_that("prepare_suitability_inputs handles rasters with no matching parameters", {
  fixtures <- load_test_fixtures()

  # Create a raster with layers that don't match any crop parameters
  r <- terra::rast(nrows = 10, ncols = 10, nlyrs = 1)
  terra::values(r) <- 1:100
  names(r) <- "unknown_parameter"

  crop_params <- fixtures$crop_suitability |>
    filter(name_parameter == "soil_ph")

  expect_error(
    prepare_suitability_inputs(r, crop_params),
    "No matching parameters found"
  )
})
