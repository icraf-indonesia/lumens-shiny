test_that("build_suitability_attributes creates correct attribute table", {
  fixtures <- load_test_fixtures()
  stacked <- load_test_rasters(fixtures$raster_inputs)

  prepared <- prepare_suitability_inputs(stacked, fixtures$crop_suitability)
  validated <- validate_parameter_coverage(
    prepared$suitability_factors,
    fixtures$crop_suitability
  )
  built <- build_suitability_raster(
    prepared$suitability_factors,
    fixtures$crop_suitability
  )

  result <- build_suitability_attributes(
    suitability_raster = built$suitability_raster,
    freq = built$suitability_raster_freq,
    lookup = prepared$lookup_suitability_factors
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("ID", "categories", "suitability", "count",
                         "limiting_factor_actual", "limiting_factor_potential"))

  # Each unique combination of classes should have one row
  expect_gt(nrow(result), 0)

  # Suitability should be one of S1, S2, S3, N
  expect_true(all(result$suitability %in% c("S1", "S2", "S3", "N")))

  # limiting_factor_actual should be a list column
  expect_type(result$limiting_factor_actual, "list")
})
