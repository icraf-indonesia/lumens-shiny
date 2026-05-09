test_that("format_suitability_outputs creates correct return list", {
  fixtures <- load_test_fixtures()
  stacked <- load_test_rasters(fixtures$raster_inputs)

  prepared <- prepare_suitability_inputs(stacked, fixtures$crop_suitability)
  built_raster <- build_suitability_raster(
    prepared$suitability_factors,
    fixtures$crop_suitability
  )
  attr <- build_suitability_attributes(
    built_raster$suitability_raster,
    built_raster$suitability_raster_freq,
    prepared$lookup_suitability_factors
  )

  result <- format_suitability_outputs(
    suitability_raster = built_raster$suitability_raster,
    suitability_attr = attr,
    by_factors = built_raster$suitability_by_factors,
    lookup = prepared$lookup_suitability_factors
  )

  expect_type(result, "list")
  expect_named(result, c("suitability_raster", "suitability_polygon",
                         "suitability_attr", "suitability_by_factors",
                         "lookup_suitability_factors"))

  expect_s4_class(result$suitability_raster, "SpatRaster")
  expect_s3_class(result$suitability_polygon, "sf")
  expect_s3_class(result$suitability_attr, "data.frame")
  expect_s4_class(result$suitability_by_factors, "SpatRaster")
})
