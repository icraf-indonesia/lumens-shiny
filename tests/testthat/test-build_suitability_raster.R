test_that("build_suitability_raster classifies and concatenates factors", {
  fixtures <- load_test_fixtures()
  stacked <- load_test_rasters(fixtures$raster_inputs)

  prepared <- prepare_suitability_inputs(stacked, fixtures$crop_suitability)

  result <- build_suitability_raster(
    suitability_factors = prepared$suitability_factors,
    crop_suitability = fixtures$crop_suitability
  )

  expect_type(result, "list")
  expect_named(result, c("suitability_raster", "suitability_raster_freq", "suitability_by_factors"))

  # The concatenated raster should have categories
  expect_s4_class(result$suitability_raster, "SpatRaster")
  expect_true(terra::is.factor(result$suitability_raster))

  # Frequency table should have counts for each category
  freq <- result$suitability_raster_freq
  expect_s3_class(freq, "data.frame")
  expect_named(freq, c("categories", "count"))
  expect_gt(nrow(freq), 0)

  # Individual factor rasters should match input layers
  expect_s4_class(result$suitability_by_factors, "SpatRaster")
  expect_equal(terra::nlyr(result$suitability_by_factors), 2)
})
