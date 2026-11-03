# tests/testthat/test-sciendo_tpm_macro_bridge.R
# Test suite for Consolidated SCIENDO TPM Macro Workbook and Simulate Bridge

library(testthat)
library(openxlsx2)
library(readxl)
library(dplyr)
library(tidyr)

source("../../11_sciendo-train/rscript/function_sciendo_train.R")
source("../../11_sciendo-simulate/rscript/function_sciendo_simulate.R")

test_dir <- tempfile("test_tpm_bridge_")
dir.create(test_dir, recursive = TRUE)

input_tpm_dir <- "../../output/sciendo_train_test/baseline_tpm"
lc_tbl_file   <- "../../data/table/landuse_table_bungo.csv"
zone_tbl_file <- "../../data/table/zone_table_bungo.csv"
template_file <- "../../11_sciendo-train/macro_template/macro_template.xlsm"

test_that("tpm_to_matrix_conversion generates consolidated workbook with Dashboard and PU sheets", {
  skip_if_not(dir.exists(input_tpm_dir), "Sample TPM directory not available")
  skip_if_not(file.exists(lc_tbl_file), "LC lookup table not available")
  skip_if_not(file.exists(template_file), "Macro template not available")
  
  lc_lookup <- read.csv(lc_tbl_file)
  zone_lookup <- read.csv(zone_tbl_file)
  
  out_xlsm <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = test_dir,
    template_path  = template_file,
    pu_lookup      = zone_lookup
  )
  
  expect_true(file.exists(out_xlsm))
  expect_equal(basename(out_xlsm), "sciendo_tpm_scenario.xlsm")
  
  # Inspect workbook sheets
  wb <- openxlsx2::wb_load(out_xlsm)
  sheet_names <- wb$get_sheet_names()
  
  # 1. Tab 1 is Dashboard
  expect_equal(unname(sheet_names[1]), "Dashboard")
  
  # 2. Contains PU sheets for each planning unit
  pu_sheets <- sheet_names[grepl("^PU_", sheet_names)]
  expect_equal(length(pu_sheets), 8)
  expect_true(any(grepl("Protected_Forest", pu_sheets)))
  expect_true(all(nchar(pu_sheets) <= 25))
  
  # 3. Contains hidden baseline sheets
  base_sheets <- sheet_names[grepl("^_Base_PU_", sheet_names)]
  expect_equal(length(base_sheets), 8)
  expect_true(all(nchar(base_sheets) <= 31))
  
  # 4. VBA project is preserved
  zip_files <- unzip(out_xlsm, list = TRUE)
  expect_true(any(grepl("vbaProject.bin", zip_files$Name)))
})

test_that("PU sheets have correct matrix structure, row sums = 1, and intervention panel", {
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  # Read PU 1 sheet
  raw_pu1 <- readxl::read_excel(out_xlsm, sheet = "PU_1_Protected_Forest", col_names = FALSE) %>% as.data.frame()
  
  # Header row is row 4
  header_row <- as.character(raw_pu1[4, ])
  expect_true(any(grepl("From \\\\ To", header_row)))
  expect_true(any(grepl("Grand Total", header_row)))
  expect_true(any(grepl("From Class", header_row)))
  expect_true(any(grepl("% Adjustment", header_row)))
  expect_true(any(grepl("Scenario Rate", header_row)))
  expect_true(any(grepl("Lock Status", header_row)))
  
  # Locate Grand Total column and verify row sums
  gt_col <- which(grepl("Grand Total", header_row))
  data_rows <- 5:nrow(raw_pu1)
  
  # Filter to matrix rows (where column B is non-empty)
  mat_row_indices <- data_rows[!is.na(raw_pu1[data_rows, 2]) & raw_pu1[data_rows, 2] != ""]
  expect_true(length(mat_row_indices) > 0)
  
  for (r in mat_row_indices) {
    row_sum_val <- as.numeric(raw_pu1[r, gt_col])
    expect_equal(row_sum_val, 1.0, tolerance = 1e-4)
  }
  
  # Verify active transitions in panel
  panel_col <- which(grepl("From Class", header_row))
  panel_rows <- data_rows[!is.na(raw_pu1[data_rows, panel_col]) & raw_pu1[data_rows, panel_col] != ""]
  expect_true(length(panel_rows) > 0)
  
  # Initial % Adjustment should be 0 and Lock Status should be Unlocked
  adj_col <- which(grepl("% Adjustment", header_row))
  lock_col <- which(grepl("Lock Status", header_row))
  expect_equal(as.numeric(raw_pu1[panel_rows[1], adj_col]), 0.0)
  expect_equal(as.character(raw_pu1[panel_rows[1], lock_col]), "Unlocked")
})

test_that("matrix_to_tpm extracts scenario_tpm CSVs seamlessly from consolidated workbook", {
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  lc_lookup <- read.csv(lc_tbl_file)
  sim_out_dir <- file.path(test_dir, "sim_output")
  
  # 1. Test passing directory containing sciendo_tpm_scenario.xlsm
  macro_folder <- dirname(out_xlsm)
  extracted <- matrix_to_tpm(
    input_folder_path = macro_folder,
    lc_lookup         = lc_lookup,
    output_dir        = sim_out_dir
  )
  
  expect_equal(length(extracted), 8)
  
  # Check generated files
  for (i in 1:8) {
    csv_file <- file.path(sim_out_dir, "scenario_tpm", sprintf("single_step%06d.csv", i))
    expect_true(file.exists(csv_file))
    
    # Read CSV and check format
    df <- read.csv(csv_file)
    expect_true(all(c("From.", "To.", "Rate") %in% colnames(df)) ||
                all(c("From*", "To*", "Rate") %in% colnames(df)))
    expect_true(nrow(df) > 0)
    expect_true(all(df$Rate > 0))
    expect_true(is.numeric(df[[1]]))
    expect_true(is.numeric(df[[2]]))
    expect_true(all(df[[1]] != df[[2]])) # Off-diagonal only
  }
  
  # 2. Test resilience against Excel lock files (~$...)
  lock_file <- file.path(macro_folder, "~$sciendo_tpm_scenario.xlsm")
  writeBin(as.raw(c(0x01, 0x02, 0x03)), lock_file)
  sim_out_lock <- file.path(test_dir, "sim_output_lock")
  extracted_lock <- matrix_to_tpm(
    input_folder_path = macro_folder,
    lc_lookup         = lc_lookup,
    output_dir        = sim_out_lock
  )
  expect_equal(length(extracted_lock), 8)
  unlink(lock_file)
  
  # 3. Compare PU 1 extracted rates against original baseline TPM
  orig_pu1 <- read.csv(file.path(input_tpm_dir, "single_step000001.csv"))
  colnames(orig_pu1) <- gsub("[*.]", "", colnames(orig_pu1))
  orig_pu1 <- orig_pu1[!is.na(orig_pu1$Rate) & orig_pu1$Rate > 0, ]
  
  gen_pu1 <- read.csv(file.path(sim_out_dir, "scenario_tpm", "single_step000001.csv"))
  colnames(gen_pu1) <- gsub("[*.]", "", colnames(gen_pu1))
  
  # Merge by From and To to compare rates
  merged <- merge(orig_pu1, gen_pu1, by = c("From", "To"), suffixes = c("_orig", "_gen"))
  expect_equal(nrow(merged), nrow(orig_pu1))
  expect_equal(merged$Rate_gen, merged$Rate_orig, tolerance = 1e-4)
})

test_that("matrix_to_tpm handles direct file path and legacy individual .xlsm files", {
  lc_lookup <- read.csv(lc_tbl_file)
  sim_out_dir2 <- file.path(test_dir, "sim_output2")
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  
  # Test direct file path
  extracted <- matrix_to_tpm(
    input_folder_path = out_xlsm,
    lc_lookup         = lc_lookup,
    output_dir        = sim_out_dir2
  )
  expect_equal(length(extracted), 8)
  
  # Test legacy backward compatibility if legacy files exist
  legacy_dir <- "../../output/sciendo_train_test/baseline_tpm_macro"
  if (dir.exists(legacy_dir) && length(list.files(legacy_dir, pattern = "\\.xlsm$")) > 0) {
    sim_out_legacy <- file.path(test_dir, "sim_output_legacy")
    legacy_extracted <- matrix_to_tpm(
      input_folder_path = legacy_dir,
      lc_lookup         = lc_lookup,
      output_dir        = sim_out_legacy
    )
    expect_true(length(legacy_extracted) > 0)
    expect_true(file.exists(file.path(sim_out_legacy, "scenario_tpm", "single_step000001.csv")))
  }
})

test_that("tpm_to_matrix_conversion works without pu_lookup (fallback to Planning Unit ID)", {
  out_xlsm_no_pu <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = file.path(test_dir, "no_pu"),
    template_path  = template_file,
    pu_lookup      = NULL
  )
  
  expect_true(file.exists(out_xlsm_no_pu))
  wb <- openxlsx2::wb_load(out_xlsm_no_pu)
  sheets <- wb$get_sheet_names()
  expect_true("Dashboard" %in% sheets)
  expect_true("PU_1_Planning_Unit_1" %in% sheets || "PU_1" %in% sheets)
})

# Cleanup temp files
unlink(test_dir, recursive = TRUE)
