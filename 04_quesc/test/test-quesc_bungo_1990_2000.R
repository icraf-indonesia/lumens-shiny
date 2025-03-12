# tests/testthat/test-quesc.R

# Test file for quesc module using testthat package
library(testthat)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(raster)
library(tidyterra)
library(networkD3)
library(scales)

# Make sure the functions are available for testing
# If your package is properly set up, you shouldn't need this
# as testthat will load your package automatically

# Source helper file
source("04_quesc/rscript/function_ques_c.R")
source("04_quesc/test/helper_tests_quesc.R")
source("03_preques/rscript/functions_ques_pre.R")

# Setup test data paths - consider moving to a helper function


# Test for package dependencies
test_that("Required packages are installed", {
  required_packages <- c(
    "terra", "raster",
    "splitstackshape", "ggplot2",
    "foreign", "reshape2",
    "dplyr", "reshape",
    "purrr", "plotly",
    "sf", "remotes",
    "rmarkdown", "bslib", 
    "tidyterra", "networkD3",
    "scales"
  )
  
  for (package in required_packages) {
    expect_true(package %in% rownames(installed.packages()),
                paste0(package, " is not installed"))
  }
})

# Test for file path validation
test_that("Input files exist", {
  test_data <- setup_test_data()
  
  expect_true(file.exists(test_data$lc_t1_path$datapath),
              "Land cover 1990 raster not found")
  expect_true(file.exists(test_data$lc_t2_path$datapath),
              "Land cover 2000 raster not found")
  expect_true(file.exists(test_data$admin_z_path$datapath),
              "Administrative zones raster not found")
  expect_true(file.exists(test_data$c_lookup_path$datapath),
              "Carbon table for land cover not found")
})

# Test for output directory creation
test_that("Output directory can be created", {
  test_output <- setup_output_dir()
  expect_true(dir.exists(test_output),
              "Output directory not created")
})

# Test the main analysis function
test_that("run_quesc_analysis returns expected output structure", {
  # Skip this test if it takes too long during routine testing
  skip_on_cran()
  test_data <- setup_test_data()
  test_output <- setup_output_dir()
  
  quesc_results <- run_quesc_analysis(
    lc_t1_path = test_data$lc_t1_path$datapath,
    lc_t2_path = test_data$lc_t2_path$datapath,
    admin_z_path = test_data$admin_z_path$datapath,
    c_lookup_path = test_data$c_lookup_path$datapath,
    time_points = list(t1 = 1990, t2 = 2000),
    output_dir = test_output,
    # progress_callback = function(value, detail) {
    #   setProgress(value = value, message = detail)
    # }
  )
  
  # Check overall structure
  expect_type(quesc_results, "list")
  expect_length(quesc_results, 11)
  
  # Check output_quesc (input path for report)
  output_quesc <- quesc_results$inputs
  expect_type(output_quesc, "list")
  expect_length(output_quesc, 5)
  
})
