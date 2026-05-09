test_that("validate_parameter_coverage identifies matching parameters", {
  fixtures <- load_test_fixtures()
  stacked <- load_test_rasters(fixtures$raster_inputs)

  prepared <- prepare_suitability_inputs(stacked, fixtures$crop_suitability)

  expect_message(
    result <- validate_parameter_coverage(
      prepared$suitability_factors,
      fixtures$crop_suitability
    ),
    "Number of objects: 2"
  )

  expect_equal(result, c("soil_ph", "clim_temperature_avg"))
})

test_that("validate_parameter_coverage errors on zero overlap", {
  fixtures <- load_test_fixtures()

  r <- terra::rast(nrows = 10, ncols = 10, nlyrs = 1)
  terra::values(r) <- 1:100
  names(r) <- "unknown_param"

  crop_params <- fixtures$crop_suitability |>
    dplyr::filter(name_parameter == "soil_ph")

  expect_error(
    validate_parameter_coverage(r, crop_params),
    "No matching parameters"
  )
})
