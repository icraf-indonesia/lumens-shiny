library(dplyr)
library(terra)
library(testthat)
source('../functions.R', encoding = 'UTF-8')

make_inputs <- function() {
  output_dir <- tempfile()
  dir.create(output_dir)
  raster <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 200, ymin = 0, ymax = 100, crs = 'EPSG:32748')
  t1 <- terra::setValues(raster, c(1, 2))
  t2 <- terra::setValues(raster, c(2, 1))
  pu <- terra::setValues(raster, c(1, 1))
  paths <- file.path(output_dir, c('t1.tif', 't2.tif', 'pu.tif', 'pu.csv', 'eae.csv', 'carbon.csv'))
  terra::writeRaster(t1, paths[1], overwrite = TRUE)
  terra::writeRaster(t2, paths[2], overwrite = TRUE)
  terra::writeRaster(pu, paths[3], overwrite = TRUE)
  write.csv(data.frame(ID = 1, PU = 'Area'), paths[4], row.names = FALSE)
  write.csv(data.frame(ID = c(1, 2), LC = c('Forest', 'Crop'), EAE = c(10, 20)), paths[5], row.names = FALSE)
  write.csv(data.frame(ID = 1, LC = 'Forest', Carbon = 10), paths[6], row.names = FALSE)
  paths
}

run_preprocess <- function(paths, carbon) {
  preprocess_data(paths[1], paths[2], paths[3], paths[4], paths[5], carbon, 2000, 2010)
}

test_that('an incomplete supplied carbon table identifies the missing class', {
  paths <- make_inputs()
  on.exit(unlink(dirname(paths[1]), recursive = TRUE))
  expect_error(run_preprocess(paths, paths[6]), 'Crop')
})

test_that('a supplied zero is valid and omitting carbon remains supported', {
  paths <- make_inputs()
  on.exit(unlink(dirname(paths[1]), recursive = TRUE))
  write.csv(data.frame(ID = c(1, 2), LC = c('Forest', 'Crop'), Carbon = c(10, 0)), paths[6], row.names = FALSE)
  complete <- run_preprocess(paths, paths[6])
  expect_true(all(is.finite(complete$combinedRasterTable$C_T1)))
  expect_true(any(complete$combinedRasterTable$C_T1 == 0))
  no_carbon <- run_preprocess(paths, NULL)
  expect_true(all(no_carbon$combinedRasterTable$C_T1 == 0))
})

test_that('non-numeric carbon values identify the affected class', {
  paths <- make_inputs()
  on.exit(unlink(dirname(paths[1]), recursive = TRUE))
  write.csv(data.frame(ID = c(1, 2), LC = c('Forest', 'Crop'), Carbon = c('10', 'invalid')), paths[6], row.names = FALSE)
  expect_error(run_preprocess(paths, paths[6]), 'Crop')
})


test_that('analysis requires a valid forward time interval', {
  paths <- make_inputs()
  on.exit(unlink(dirname(paths[1]), recursive = TRUE))
  run_years <- function(t1, t2) preprocess_data(paths[1], paths[2], paths[3], paths[4], paths[5], NULL, t1, t2)
  expect_error(run_years(2010, 2010), 'T2 must be later')
  expect_error(run_years(2010, 2005), 'T2 must be later')
  expect_error(run_years('invalid', 2010), 'valid years')
  expect_equal(run_years(2005, 2010)$period, 5)
  expect_error(build_opcost_table(data.frame(), 0, 1), 'positive number')
})
