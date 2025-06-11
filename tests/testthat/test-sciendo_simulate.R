# tests/testthat/test-sciendo_simulate

#setwd("tests/testthat/")
# Test file for pre_ques module using testthat package
library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(ggthemes)
library(rlang)


# Source helper file
source("../../11_sciendo-simulate/rscript/function_sciendo_simulate.R")
source("helper_tests.R")


#-------------------------------------------------------------------------------
# Test Cases for plot_interactive_stacked_area
#-------------------------------------------------------------------------------
# These tests validate the core functionality of the plotting function:
# 1. Correct Output: Ensures the function produces a 'plotly' object.
# 2. Input Validation: Checks that the function fails gracefully with bad inputs.
# 3. Column Name Flexibility: Confirms the function works with custom column names.
# 4. Data Cleaning: Verifies that the function handles non-numeric data correctly.
#-------------------------------------------------------------------------------

luc_proj_table <- tibble::tribble(
  ~ID, ~LC, ~`T+1`, ~`T+2`,
  3, "Hutan rawa primer", 9784, 9373,
  4, "Hutan rawa sekunder/bekas tebangan", 31398, 32464,
  8, "Perkebunan/Kebun", 124368, 129544
)


test_that("Function returns a plotly object with valid data", {
  # Call the function with the standard test data
  plot_output <- plot_interactive_stacked_area(luc_proj_table)
  
  # Check if the output is of the class "plotly"
  expect_s3_class(plot_output, "plotly")
})

test_that("Function stops with an error for invalid column names", {
  # Create a data frame with incorrect column names
  bad_data <- luc_proj_table %>%
    rename(Category = LC, Identifier = ID)
  
  # Expect the function to throw an error because it can't find "LC" and "ID"
  expect_error(
    plot_interactive_stacked_area(bad_data),
    "The provided data frame must contain the columns: LC, ID"
  )
})

test_that("Function handles non-default column names correctly", {
  # Create a data frame with different column names
  custom_data <- luc_proj_table %>%
    rename(Category = LC, Identifier = ID)
  
  # Call the function, specifying the custom column names
  # This should run without error
  expect_no_error(
    plot_interactive_stacked_area(custom_data, class_col = "Category", id_col = "Identifier")
  )
  
  # Also check if the output is a plotly object
  plot_output <- plot_interactive_stacked_area(custom_data, class_col = "Category", id_col = "Identifier")
  expect_s3_class(plot_output, "plotly")
})

test_that("Function correctly cleans character-based numeric data", {
  # Create a data frame where numeric values are stored as characters
  char_data <- tibble::tribble(
    ~ID, ~LC, ~`T+1`, ~`T+2`,
    3, "Hutan rawa primer", "9,784", "9,373", # Using comma as thousands separator
    8, "Perkebunan/Kebun", "124368", "129544"
  )
  
  # The function should parse these strings into numbers and run without error
  expect_no_error(
    plot_interactive_stacked_area(char_data)
  )
  
  # Check that the result is a valid plotly object
  plot_output <- plot_interactive_stacked_area(char_data)
  expect_s3_class(plot_output, "plotly")
})
