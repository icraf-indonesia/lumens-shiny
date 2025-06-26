# tests/testthat/test-pre_ques.R

# Test file for pre_ques module using testthat package
library(testthat)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

# Make sure the functions are available for testing
# If your package is properly set up, you shouldn't need this
# as testthat will load your package automatically

# Source helper file
source("../../03_preques/rscript/functions_ques_pre.R")
source("helper_tests.R")

# Setup test data paths - consider moving to a helper function


# Test for package dependencies
test_that("Required packages are installed", {
  required_packages <- c(
    "terra", "dplyr", "ggplot2", "sf",
    "rmarkdown", "bslib",
    "readr", "tidyr", "networkD3",
    "stringr", "scales", "purrr"
  )
  
  for (package in required_packages) {
    expect_true(package %in% rownames(installed.packages()),
                paste0(package, " is not installed"))
  }
})

# Test for file path validation
test_that("Input files exist", {
  test_data <- setup_test_data()
  
  expect_true(file.exists(test_data$lc_t1_input$datapath),
              "Land cover 1990 raster not found")
  expect_true(file.exists(test_data$lc_t2_input$datapath),
              "Land cover 2000 raster not found")
  expect_true(file.exists(test_data$admin_z_input$datapath),
              "Administrative zones raster not found")
  expect_true(file.exists(test_data$zone_lookup_input$datapath),
              "Land use table for zones not found")
  expect_true(file.exists(test_data$trajectory_lookup_input$datapath),
              "Trajectory definitions not found")
})

# Test for output directory creation
test_that("Output directory can be created", {
  test_output <- setup_output_dir()
  expect_true(dir.exists(test_output),
              "Output directory not created")
})

# Test the main analysis function
test_that("run_preques_analysis returns expected output structure", {
  # Skip this test if it takes too long during routine testing
  skip_on_cran()
  test_data <- setup_test_data()
  test_output <- setup_output_dir()
  
  pre_ques_results <- run_preques_analysis(
    lc_t1_input = test_data$lc_t1_input,
    lc_t2_input = test_data$lc_t2_input,
    admin_z_input = test_data$admin_z_input,
    zone_lookup_input = test_data$zone_lookup_input,
    trajectory_lookup_input = test_data$trajectory_lookup_input,
    lc_lookup_input = test_data$lc_lookup_input,
    time_points = list(t1 = 1990, t2 = 2000),
    output_dir = test_output
  )
  
  # Check overall structure
  expect_type(pre_ques_results, "list")
  expect_length(pre_ques_results, 2)
  
  # Test output_pre_ques (first list element)
  output_pre_ques <- pre_ques_results$output_pre_ques
  expect_type(output_pre_ques, "list")
  expect_length(output_pre_ques, 3)
  
  # Check input_dataviz element
  expect_type(output_pre_ques$input_dataviz, "list")
  expect_s3_class(output_pre_ques$input_dataviz$tbl_lookup_lc_t1, "data.frame")
  expect_s3_class(output_pre_ques$input_dataviz$tbl_lookup_lc_t2, "data.frame")
  expect_s3_class(output_pre_ques$input_dataviz$tbl_lookup_admin, "data.frame")
  
  # Check landscape_level element
  expect_type(output_pre_ques$landscape_level, "list")
  expect_s3_class(output_pre_ques$landscape_level$luc_top_10_tbl, "data.frame")
  expect_s3_class(output_pre_ques$landscape_level$crosstab_landscape, "data.frame")
  expect_s3_class(output_pre_ques$landscape_level$crosstab_long, "data.frame")
  
  # Check pu_level element
  expect_type(output_pre_ques$pu_level, "list")
  zone_count <- nrow(read.csv(test_data$zone_lookup_input$datapath))
  expect_lte(length(output_pre_ques$pu_level), zone_count)
  
  # Test output_pre_ques_traj (second list element)
  output_pre_ques_traj <- pre_ques_results$output_pre_ques_traj
  expect_type(output_pre_ques_traj, "list")
  expect_length(output_pre_ques_traj, 2)
  
  # Check landscape_level in trajectory output
  expect_type(output_pre_ques_traj$landscape_level, "list")
  expect_s3_class(output_pre_ques_traj$landscape_level$table_traj_area, "data.frame")
  
  # Check pu_level in trajectory output
  expect_type(output_pre_ques_traj$pu_level, "list")
  expect_lte(length(output_pre_ques_traj$pu_level), zone_count)
})

# Test for expected output files
test_that("Expected output files are created", {
  # Skip this if the previous test is skipped
  skip_on_cran()
  
  test_output <- setup_output_dir()
  
  # Expected output files
  expected_outputs <- c(
    "PreQuES_ChangeTrajectory_lookup.csv",
    "PreQuES_ChangeTrajectory_map.tif",
    "PreQuES_luc_change_transition_table.csv",
    "PreQuES_report.html"
  )
  
  for(file in expected_outputs) {
    file_path <- file.path(test_output, file)
    expect_true(
      file.exists(file_path),
      paste0(file, " not found in output directory")
    )
  }
})