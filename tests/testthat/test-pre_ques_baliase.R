# ------------------------------------------------------------------------------
# TEST SCRIPT FOR: Pre-QuES Analysis (Baliase Dataset)
# ------------------------------------------------------------------------------
# PURPOSE:
# This script tests the 'run_preques_analysis' using the Baliase dataset
#
# HOW IT WORKS:
# 1. Sets up the testing environment and loads necessary functions.
# 2. Uses a helper function ('setup_baliase_test_data') to prepare all inputs.
# 3. Executes the main 'run_preques_analysis' function.
# 4. Validates that the output list and its nested elements have the correct
#    types, lengths, and names, ensuring the function ran successfully and
#    produced the expected data structure.
# ------------------------------------------------------------------------------

# Load necessary libraries for testing and data manipulation
library(testthat) # The core testing framework
library(terra)    # For spatial raster data operations
library(sf)       # For spatial vector data (shapefiles)
library(dplyr)    # For data manipulation and pipelines



# Load required functions into the testing environment
source("helper_tests.R") # Contains functions to set up test data
source("../../03_preques/rscript/functions_ques_pre.R") # Contains the main analysis functions to be tested

# Define a test unit using test_that()
test_that("run_preques_analysis for Baliase returns expected output structure", {

  skip_on_cran()
  
  # PHASE 1: SETUP
  # --------------------------------------------------------------------------
  # Call the helper function to get a list of all required input data for the Baliase test case.
  test_data_baliase <- setup_baliase_test_data()
  # Create a dedicated output directory for this test run.
  test_output_baliase <- setup_output_dir_baliase()
  
  # PHASE 2: EXECUTION
  # --------------------------------------------------------------------------
  # Run the main function with the prepared test data.
  
  pre_ques_results <- run_preques_analysis(
    lc_t1_input = test_data_baliase$lc_t1_input,
    lc_t2_input = test_data_baliase$lc_t2_input,
    admin_z_input = test_data_baliase$admin_z_input,
    lc_lookup_input = test_data_baliase$lc_lookup_input,
    zone_lookup_input = NULL, # This test case does not use a separate zone lookup table
    trajectory_lookup_input = test_data_baliase$trajectory_lookup_input,
    time_points = list(t1 = 2018, t2 = 2023),
    output_dir = test_output_baliase
  )
  
  # PHASE 3: VALIDATION
  # --------------------------------------------------------------------------
  # Check if the output from the function meets expectations.
  
  # Check the top-level structure of the results.
  # 'expect_type' verifies that the output is a list.
  expect_type(pre_ques_results, "list")
  # 'expect_length' verifies that the list contains exactly two main components.
  expect_length(pre_ques_results, 2)
  # 'expect_named' verifies that those components have the correct names.
  expect_named(pre_ques_results, c("output_pre_ques", "output_pre_ques_traj"))
  
  # Dive deeper and check the structure of the first element (land cover change results).
  output_pre_ques <- pre_ques_results$output_pre_ques
  expect_type(output_pre_ques, "list")
  expect_named(output_pre_ques, c("input_dataviz", "landscape_level", "pu_level"))
  
  # Check the structure of the second element (trajectory analysis results).
  output_pre_ques_traj <- pre_ques_results$output_pre_ques_traj
  expect_type(output_pre_ques_traj, "list")
  expect_named(output_pre_ques_traj, c("landscape_level", "pu_level"))
})