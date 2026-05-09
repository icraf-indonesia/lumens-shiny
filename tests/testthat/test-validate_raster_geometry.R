test_that("validate_raster_geometry passes for consistent rasters", {
  fixtures <- load_test_fixtures()
  stacked <- load_test_rasters(fixtures$raster_inputs)

  # Split back into list
  raster_list <- lapply(seq_len(terra::nlyr(stacked)), function(i) stacked[[i]])

  result <- validate_raster_geometry(raster_list)

  expect_true(result$valid)
  expect_equal(nrow(result$errors), 0)
  expect_s3_class(result$details, "data.frame")
})

test_that("validate_raster_geometry fails for different extents", {
  r1 <- terra::rast(nrows = 10, ncols = 10, nlyrs = 1)
  terra::values(r1) <- 1:100
  names(r1) <- "r1"

  r2 <- terra::rast(nrows = 10, ncols = 10, nlyrs = 1, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(r2) <- 1:100
  names(r2) <- "r2"

  result <- validate_raster_geometry(list(r1, r2))

  expect_false(result$valid)
  expect_gt(nrow(result$errors), 0)
})
