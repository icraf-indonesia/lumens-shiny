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

#-------------------------------------------------------------------------------
# Test Cases for DINAMICA EGO Temporary Folder Helpers
#-------------------------------------------------------------------------------

test_that("resolve_dinamica_temp_dir maps options correctly", {
  expect_null(resolve_dinamica_temp_dir(NULL, "D:/out"))
  expect_null(resolve_dinamica_temp_dir("", "D:/out"))
  expect_null(resolve_dinamica_temp_dir("default", "D:/out"))
  expect_null(resolve_dinamica_temp_dir("output_dir", NULL))
  expect_equal(resolve_dinamica_temp_dir("output_dir", "D:/out"), "D:/out/TEMP_SCIENDO")
  expect_equal(resolve_dinamica_temp_dir("D:/TEMP_SCIENDO", "D:/out"), "D:/TEMP_SCIENDO")
})

test_that("detect_dinamica_version extracts major version", {
  expect_equal(detect_dinamica_version("C:/Program Files/Dinamica EGO 7", "DinamicaConsole7.exe"), "7")
  expect_equal(detect_dinamica_version("C:/Program Files/Dinamica EGO 6", NULL), "6")
  expect_null(detect_dinamica_version("C:/Program Files/Dinamica", NULL))
})

test_that("detect_writable_non_c_drives excludes C: and returns valid paths", {
  drives <- detect_writable_non_c_drives()
  expect_type(drives, "character")
  expect_false(any(grepl("^C:", drives, ignore.case = TRUE)))
  if (length(drives) > 0) {
    expect_true(all(grepl("/TEMP_SCIENDO$", drives)))
  }
})

test_that("with_dinamica_temp_dir creates, isolates, and cleans up temp folder on success and error", {
  base_dir <- file.path(tempdir(), paste0("TEMP_SCIENDO_TEST_", Sys.getpid()))
  unlink(base_dir, recursive = TRUE, force = TRUE)

  old_temp <- Sys.getenv("TEMP", unset = NA)
  old_tmp  <- Sys.getenv("TMP", unset = NA)

  # Success case
  res <- with_dinamica_temp_dir(base_dir, NULL, NULL, {
    expect_true(dir.exists(Sys.getenv("TEMP")))
    expect_true(startsWith(normalizePath(Sys.getenv("TEMP"), winslash = "/", mustWork = FALSE),
                           normalizePath(base_dir, winslash = "/", mustWork = FALSE)))
    "ok"
  })
  expect_equal(res, "ok")
  expect_false(dir.exists(base_dir))
  expect_equal(Sys.getenv("TEMP", unset = NA), old_temp)
  expect_equal(Sys.getenv("TMP", unset = NA), old_tmp)

  # Error case: still cleans up and restores env
  expect_error(
    with_dinamica_temp_dir(base_dir, NULL, NULL, {
      stop("simulated crash")
    }),
    "simulated crash"
  )
  expect_false(dir.exists(base_dir))
  expect_equal(Sys.getenv("TEMP", unset = NA), old_temp)
  expect_equal(Sys.getenv("TMP", unset = NA), old_tmp)
})

test_that("is_ego_temp_read_error detects Issue #221 ERMapper read-mode errors", {
  expect_true(is_ego_temp_read_error(c(
    '[@2 ERROR] "ForEachCategory":',
    '  The header file of ERMapper map "C:/Users/u/AppData/Local/Temp/EGO123.tmp_TEMP/EGOTmp_1_.ers" can not be open in read mode.'
  )))
  expect_false(is_ego_temp_read_error("Some other DINAMICA EGO error"))
  expect_false(is_ego_temp_read_error(character(0)))
})

test_that("multiple_lc_freq_combined handles single simulation period (1 landscape file) without initial_raster", {
  tmp_dir <- file.path(tempdir(), paste0("sciendo_1period_", Sys.getpid()))
  unlink(tmp_dir, recursive = TRUE, force = TRUE)
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 400, ymin = 0, ymax = 400, crs = "EPSG:32748")
  terra::values(r) <- rep(c(1, 2), each = 8)
  terra::writeRaster(r, file.path(tmp_dir, "landscape2025.tif"), overwrite = TRUE)

  lc_lookup <- data.frame(ID = c(1, 2), LC = c("Forest", "Agriculture"), stringsAsFactors = FALSE)

  res <- multiple_lc_freq_combined(tmp_dir, lc_lookup, PU = "NO", initial_raster = NULL)
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("Landcover", "2025"))
  expect_equal(nrow(res), 2)
})

test_that("plot_categorical_raster handles >50 classes (e.g. 60 planning units)", {
  r <- terra::rast(nrows = 12, ncols = 10, xmin = 0, xmax = 1200, ymin = 0, ymax = 1200, crs = "EPSG:32748")
  terra::values(r) <- rep(1:60, each = 2)
  pu_lookup <- data.frame(ID = 1:60, PU = paste0("PU_", sprintf("%02d", 1:60)), stringsAsFactors = FALSE)
  r_cat <- add_legend_to_categorical_raster(r, pu_lookup)

  p <- plot_categorical_raster(r_cat)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("add_legend_to_categorical_raster handles character IDs with unused lookup rows and 8-bit class 128", {
  f <- tempfile(fileext = ".tif")
  on.exit(unlink(f), add = TRUE)
  r <- terra::rast(nrows = 6, ncols = 5, xmin = 0, xmax = 500, ymin = 0, ymax = 600, crs = "EPSG:32748")
  terra::values(r) <- c(1:29, 128)
  terra::writeRaster(r, f, datatype = "INT1U", overwrite = TRUE)
  r8 <- terra::rast(f)

  lt <- data.frame(
    ID = as.character(c(1:29, 128, 999)),
    LC = paste0("Class_", c(1:29, 128, 999)),
    stringsAsFactors = FALSE
  )
  r_out <- add_legend_to_categorical_raster(r8, lt)
  lvls <- terra::levels(r_out)[[1]]
  expect_equal(nrow(lvls), 30)
  expect_true(128 %in% lvls[[1]])
})

test_that("matrix_to_tpm handles partial unmatched classes safely and reports clear mismatch message when wrong lookup table is given", {
  skip_if_not_installed("openxlsx2")
  tmp_dir <- file.path(tempdir(), paste0("mat_tpm_guard_", Sys.getpid()))
  unlink(tmp_dir, recursive = TRUE, force = TRUE)
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  xl_path <- file.path(tmp_dir, "sciendo_tpm_scenario.xlsm")
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet("Dashboard")
  wb$add_worksheet("PU_1_Lindung")
  wb$add_data("PU_1_Lindung", x = "Unit Perencanaan 1: Lindung", start_col = 2, start_row = 1)
  hdr <- data.frame(
    Luas = "Luas 2020 (ha)", Dari = "Dari / Ke",
    NoData = "NoData", Forest = "Forest", Agri = "Agri", Total = "Total Baris",
    stringsAsFactors = FALSE
  )
  wb$add_data("PU_1_Lindung", x = hdr, start_col = 1, start_row = 6, col_names = FALSE)
  rows_df <- data.frame(
    Area = c(0, 100, 50),
    Class = c("NoData", "Forest", "Agri"),
    c1 = c(1, 0, 0),
    c2 = c(0, 0.8, 0.1),
    c3 = c(0, 0.2, 0.9),
    tot = c(1, 1, 1),
    stringsAsFactors = FALSE
  )
  wb$add_data("PU_1_Lindung", x = rows_df, start_col = 1, start_row = 7, col_names = FALSE)
  wb$save(xl_path)

  # Partial mismatch: lookup omits NoData (ID 0), should drop NoData safely without 'undefined columns selected'
  lc_partial <- data.frame(ID = c(1, 2), LC = c("Forest", "Agri"), stringsAsFactors = FALSE)
  res <- matrix_to_tpm(xl_path, lc_partial, tmp_dir)
  expect_length(res, 1)
  out_tbl <- read.csv(res[1], check.names = FALSE)
  expect_equal(nrow(out_tbl), 2)

  # Full mismatch (e.g., Planning Unit table passed as Land Cover lookup): clear diagnostic message, no undefined columns error
  pu_wrong <- data.frame(ID = c(1, 2), LC = c("Lindung", "Produksi"), stringsAsFactors = FALSE)
  out_dir2 <- file.path(tmp_dir, "wrong_lc")
  dir.create(out_dir2)
  expect_message(
    matrix_to_tpm(xl_path, pu_wrong, out_dir2),
    "None of the matrix classes matched the Land Cover Lookup Table"
  )
})
