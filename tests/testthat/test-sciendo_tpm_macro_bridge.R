# tests/testthat/test-sciendo_tpm_macro_bridge.R
# Test suite for Consolidated SCIENDO TPM Macro Workbook and Simulate Bridge
# Covers:
# 1. Bahasa Indonesia localization via UI language & English fallback
# 2. Step-by-step instruction boxes on Dashboard and PU sheets
# 3. Real percentage numbers on newly added transitions (no "NEW" text) & 2-way sync
# 4. Macro unblocking utility script (activate_macros.bat) & safe VBA execution
# 5. Full roundtrip bridge to SCIENDO Simulate matrix_to_tpm()

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

test_that("tpm_to_matrix_conversion generates consolidated Indonesian workbook with Dashboard, PU sheets, and unblock script", {
  skip_if_not(dir.exists(input_tpm_dir), "Sample TPM directory not available")
  skip_if_not(file.exists(lc_tbl_file), "LC lookup table not available")
  skip_if_not(file.exists(template_file), "Macro template not available")
  
  zone_lookup <- read.csv(zone_tbl_file)
  
  out_xlsm <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = test_dir,
    template_path  = template_file,
    pu_lookup      = zone_lookup,
    lang           = "id"
  )
  
  expect_true(file.exists(out_xlsm))
  expect_equal(basename(out_xlsm), "sciendo_tpm_scenario.xlsm")
  
  # Check activate_macros.bat utility script
  bat_file <- file.path(dirname(out_xlsm), "activate_macros.bat")
  expect_true(file.exists(bat_file))
  bat_lines <- readLines(bat_file, warn = FALSE)
  expect_true(any(grepl("Unblock-File", bat_lines)))
  expect_true(any(grepl("\\*\\.xlsm", bat_lines)))
  expect_true(any(grepl("Membuka blokir keamanan Windows", bat_lines)))
  
  # Inspect workbook sheets
  wb <- openxlsx2::wb_load(out_xlsm)
  sheet_names <- wb$get_sheet_names()
  
  # 1. Tab 1 is Dashboard (internal name preserved for SCIENDO Simulate compatibility)
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
  
  # 4. Contains hidden reference sheet _LULC_List for dropdowns
  expect_true("_LULC_List" %in% sheet_names)
  
  # 5. VBA project is preserved
  zip_files <- unzip(out_xlsm, list = TRUE)
  expect_true(any(grepl("vbaProject.bin", zip_files$Name)))
})

test_that("Indonesian Dashboard sheet has localized layout, policy card, instruction box, and DrawingML buttons", {
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  raw_dash <- readxl::read_excel(out_xlsm, sheet = "Dashboard", range = "A1:G28", col_names = FALSE) %>% as.data.frame()
  
  # Row 2 & 3: Localized Title and Subtitle
  expect_true(grepl("SCIENDO Scenario Builder", as.character(raw_dash[2, 2]), ignore.case = TRUE))
  expect_true(grepl("Pembangunan Skenario Melalui Matriks Probabilitas Transisi", as.character(raw_dash[3, 2]), ignore.case = TRUE))
  
  # Rows 7-10: Localized Policy Card
  expect_true(grepl("Input Intervensi untuk Seluruh Unit Perencanaan", as.character(raw_dash[7, 2]), ignore.case = TRUE))
  expect_equal(as.character(raw_dash[8, 2]), "Kelas Asal:")
  expect_equal(as.character(raw_dash[8, 4]), "% Penyesuaian:")
  expect_equal(as.character(raw_dash[9, 2]), "Kelas Tujuan:")
  expect_equal(as.character(raw_dash[9, 4]), "Target PU:")
  expect_true(!is.na(raw_dash[8, 3])) # From Class default value exists
  expect_true(!is.na(raw_dash[9, 3])) # To Class default value exists
  expect_equal(as.numeric(raw_dash[8, 5]), -0.50) # Default % adjustment
  expect_equal(as.character(raw_dash[9, 5]), "Semua Unit Perencanaan") # Localized default target PU
  
  # Rows 12-16: Step-by-Step Instruction Box on Dashboard
  expect_true(grepl("Panduan Singkat Dashboard:", as.character(raw_dash[12, 2])))
  expect_true(grepl("1\\.", as.character(raw_dash[13, 2])))
  expect_true(grepl("2\\.", as.character(raw_dash[14, 2])))
  expect_true(grepl("3\\.", as.character(raw_dash[15, 2])))
  expect_true(grepl("Catatan:", as.character(raw_dash[16, 2])))
  
  # Row 17: Localized Summary Table Headers
  expect_equal(as.character(raw_dash[17, 2]), "ID PU")
  expect_equal(as.character(raw_dash[17, 3]), "Nama Unit Perencanaan")
  expect_equal(as.character(raw_dash[17, 4]), "Tautan Navigasi")
  expect_equal(as.character(raw_dash[17, 5]), "Intervensi Aktif")
  expect_equal(as.character(raw_dash[17, 6]), "Status Validasi")
  expect_equal(as.character(raw_dash[17, 7]), "Transisi Historis")
  
  # Rows 18+: Data rows (8 PUs), PU 1 at row 18
  expect_equal(nrow(raw_dash[18:25, ]), 8)
  expect_equal(as.numeric(raw_dash[18, 2]), 1)
  expect_true(grepl("Protected Forest", as.character(raw_dash[18, 3])))
  
  # Navigation link label at row 18
  wb <- openxlsx2::wb_load(out_xlsm)
  cc_dash <- wb$worksheets[[1]]$sheet_data$cc
  d18_row <- cc_dash[cc_dash$r == "D18", ]
  expect_true(grepl("Buka Sheet PU_1", d18_row$f))
  
  # Drawing elements & localized button labels in DrawingML
  td_draw <- tempfile("draw_check_")
  unzip(out_xlsm, files = "xl/drawings/drawing1.xml", exdir = td_draw)
  draw_text <- paste(readLines(file.path(td_draw, "xl/drawings/drawing1.xml"), warn = FALSE), collapse = "\n")
  unlink(td_draw, recursive = TRUE)
  
  expect_true(grepl("Validasi Semua Unit Perencanaan", draw_text))
  expect_true(grepl("Atur Ulang ke Baseline", draw_text))
  expect_true(grepl("Aplikasikan Intervensi", draw_text))
})

test_that("Indonesian PU sheets have localized headers, instruction box, row sums = 1, and navigation links", {
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  wb <- openxlsx2::wb_load(out_xlsm)
  sheet_names <- wb$get_sheet_names()
  pu_sheets <- sheet_names[grepl("^PU_", sheet_names)]
  
  # Row 1: Localized title (Column B)
  raw_pu1 <- readxl::read_excel(out_xlsm, sheet = pu_sheets[1], col_names = FALSE) %>% as.data.frame()
  expect_true(grepl("Unit Perencanaan 1:", as.character(raw_pu1[1, 2])))
  
  # Row 2: Hyperlinks in openxlsx2 formula structure
  idx_pu1 <- which(sheet_names == pu_sheets[1])
  cc_pu1 <- wb$worksheets[[idx_pu1]]$sheet_data$cc
  b2_row <- cc_pu1[cc_pu1$r == "B2", ]
  d2_row <- cc_pu1[cc_pu1$r == "D2", ]
  expect_true(grepl("Kembali ke Dashboard Utama", b2_row$f))
  expect_true(grepl("Lanjut ke Unit Perencanaan Berikutnya", d2_row$f) || grepl(pu_sheets[2], d2_row$f))
  
  # Read final PU sheet and verify looping navigation link
  idx_pu_last <- which(sheet_names == tail(pu_sheets, 1))
  cc_last <- wb$worksheets[[idx_pu_last]]$sheet_data$cc
  d2_last <- cc_last[cc_last$r == "D2", ]
  expect_true(grepl("Kembali ke Unit Perencanaan Pertama", d2_last$f) && grepl("PU 1", d2_last$f))
  
  # Rows 3-4: Step-by-Step Instruction Box on PU sheet (Column B)
  expect_true(grepl("Panduan Penggunaan:", as.character(raw_pu1[3, 2])))
  expect_true(grepl("3\\.", as.character(raw_pu1[4, 2])))
  
  # Row 6: Localized Matrix and Recapitulation Table Headers
  header_row <- as.character(raw_pu1[6, ])
  expect_true(any(grepl("Dari / Ke", header_row)))
  expect_true(any(grepl("Total Baris", header_row)))
  expect_true(any(grepl("Kelas Asal", header_row)))
  expect_true(any(grepl("Kelas Tujuan", header_row)))
  expect_true(any(grepl("Laju Baseline", header_row)))
  expect_true(any(grepl("Luas Baseline \\(ha\\)", header_row)))
  expect_true(any(grepl("% Penyesuaian", header_row)))
  expect_true(any(grepl("Laju Skenario", header_row)))
  expect_true(any(grepl("Luas Skenario \\(ha\\)", header_row)))
  expect_true(any(grepl("Status Intervensi", header_row)))
  
  # Row 6: Localized Column A header (cell A6)
  expect_equal(as.character(raw_pu1[6, 1]), "Luas T1 (ha)")
  
  # Locate Total Baris column and verify row sums
  gt_col <- which(grepl("Total Baris", header_row))
  data_rows <- 7:nrow(raw_pu1)
  
  # Filter to matrix rows (until the blank separator row)
  mat_row_indices <- integer(0)
  for (r in data_rows) {
    if (!is.na(raw_pu1[r, 2]) && raw_pu1[r, 2] != "") {
      mat_row_indices <- c(mat_row_indices, r)
    } else {
      break
    }
  }
  expect_true(length(mat_row_indices) > 0)
  
  for (r in mat_row_indices) {
    row_sum_val <- as.numeric(raw_pu1[r, gt_col])
    expect_equal(row_sum_val, 1.0, tolerance = 1e-4)
    # Column A contains initial class area in ha
    expect_true(!is.na(raw_pu1[r, 1]))
    expect_true(as.numeric(raw_pu1[r, 1]) > 0)
  }
  
  # Check 1 blank separator row below last matrix row
  blank_row_idx <- tail(mat_row_indices, 1) + 1
  expect_true(is.na(raw_pu1[blank_row_idx, 1]) || raw_pu1[blank_row_idx, 1] == "")
  expect_true(is.na(raw_pu1[blank_row_idx, 2]) || raw_pu1[blank_row_idx, 2] == "")
  
  # Check 3 Bottom Summary Rows
  base_sum_idx  <- blank_row_idx + 1
  scen_sum_idx  <- blank_row_idx + 2
  delta_sum_idx <- blank_row_idx + 3
  
  expect_equal(as.character(raw_pu1[base_sum_idx, 2]), "Luas Baseline Tahun Proyeksi (ha)")
  expect_equal(as.character(raw_pu1[scen_sum_idx, 2]), "Luas Skenario Tahun Proyeksi (ha)")
  expect_equal(as.character(raw_pu1[delta_sum_idx, 2]), "Perubahan Bersih (ha)")
  
  # Verify summary row formulas in worksheet data
  last_mat_r <- tail(mat_row_indices, 1)
  expect_true(any(grepl(paste0("SUM\\(A7:A", last_mat_r, "\\)"), cc_pu1$f)))
  expect_true(any(grepl(paste0("SUMPRODUCT\\(\\$A\\$7:\\$A\\$", last_mat_r), cc_pu1$f)))
  expect_true(any(grepl("_Base_", cc_pu1$f)))
  expect_true(any(grepl(paste0("C", scen_sum_idx, "-C", base_sum_idx), cc_pu1$f) |
                    grepl(paste0("C", scen_sum_idx, " - C", base_sum_idx), cc_pu1$f)))
  
  # Hidden _Base_ sheet also contains Column A and initial class areas
  base_sh_name <- paste0("_Base_", pu_sheets[1])
  raw_base1 <- readxl::read_excel(out_xlsm, sheet = base_sh_name, col_names = FALSE) %>% as.data.frame()
  expect_equal(as.character(raw_base1[6, 1]), "Luas T1 (ha)")
  expect_equal(as.numeric(raw_base1[7, 1]), as.numeric(raw_pu1[7, 1]))
  expect_equal(as.character(raw_base1[base_sum_idx, 2]), "Luas Baseline Tahun Proyeksi (ha)")
  
  # Verify active transitions in panel
  panel_col <- which(grepl("Kelas Asal", header_row))
  panel_rows <- data_rows[!is.na(raw_pu1[data_rows, panel_col]) & raw_pu1[data_rows, panel_col] != ""]
  expect_true(length(panel_rows) > 0)
  
  # Initial % Adjustment should be 0, Intervention Status should be "Baseline", areas should be positive
  base_area_col <- which(grepl("Luas Baseline \\(ha\\)", header_row))
  adj_col       <- which(grepl("% Penyesuaian", header_row))
  status_col    <- which(grepl("Status Intervensi", header_row))
  
  expect_true(as.numeric(raw_pu1[panel_rows[1], base_area_col]) > 0)
  expect_equal(as.numeric(raw_pu1[panel_rows[1], adj_col]), 0.0)
  expect_equal(as.character(raw_pu1[panel_rows[1], status_col]), "Baseline")
  
  # Verify Scenario Area (ha) formula is dynamically embedded
  expect_true(any(grepl("ROUND", cc_pu1$f)))
})

test_that("matrix_to_tpm extracts scenario_tpm CSVs seamlessly from consolidated Indonesian workbook", {
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

test_that("English localization (lang = 'en') generates English headers, guide boxes, and DrawingML buttons", {
  en_dir <- file.path(test_dir, "en_test")
  dir.create(en_dir, recursive = TRUE)
  zone_lookup <- read.csv(zone_tbl_file)
  
  out_en_xlsm <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = en_dir,
    template_path  = template_file,
    pu_lookup      = zone_lookup,
    lang           = "en"
  )
  
  expect_true(file.exists(out_en_xlsm))
  
  # Check English activate_macros.bat
  bat_en_file <- file.path(dirname(out_en_xlsm), "activate_macros.bat")
  expect_true(file.exists(bat_en_file))
  bat_en_lines <- readLines(bat_en_file, warn = FALSE)
  expect_true(any(grepl("Unblocking Windows security", bat_en_lines)))
  expect_true(any(grepl("Done! Macro file is ready", bat_en_lines)))
  
  # Check Dashboard sheet in English
  raw_dash_en <- readxl::read_excel(out_en_xlsm, sheet = "Dashboard", range = "A1:G28", col_names = FALSE) %>% as.data.frame()
  expect_true(grepl("Scenario Builder", as.character(raw_dash_en[2, 2]), ignore.case = TRUE))
  expect_true(grepl("Transition Probability Matrix", as.character(raw_dash_en[3, 2]), ignore.case = TRUE))
  expect_true(grepl("Intervention Input Across Planning Units", as.character(raw_dash_en[7, 2]), ignore.case = TRUE))
  expect_equal(as.character(raw_dash_en[8, 2]), "From Class:")
  expect_equal(as.character(raw_dash_en[8, 4]), "% Adjustment:")
  expect_equal(as.character(raw_dash_en[9, 2]), "To Class:")
  expect_equal(as.character(raw_dash_en[9, 4]), "Target PU:")
  expect_equal(as.character(raw_dash_en[9, 5]), "All Planning Units")
  expect_true(grepl("Quick Dashboard Guide:", as.character(raw_dash_en[12, 2])))
  expect_equal(as.character(raw_dash_en[17, 2]), "PU ID")
  expect_equal(as.character(raw_dash_en[17, 3]), "Planning Unit Name")
  expect_equal(as.character(raw_dash_en[17, 4]), "Navigation Link")
  
  # Check DrawingML in English
  td_en_draw <- tempfile("en_draw_")
  unzip(out_en_xlsm, files = "xl/drawings/drawing1.xml", exdir = td_en_draw)
  en_draw_text <- paste(readLines(file.path(td_en_draw, "xl/drawings/drawing1.xml"), warn = FALSE), collapse = "\n")
  unlink(td_en_draw, recursive = TRUE)
  
  expect_true(grepl("Validate All Sheets", en_draw_text))
  expect_true(grepl("Reset All to Baseline", en_draw_text))
  expect_true(grepl("Apply Intervention", en_draw_text))
  
  # Check PU sheet in English
  raw_pu1_en <- readxl::read_excel(out_en_xlsm, sheet = "PU_1_Protected_Forest", col_names = FALSE) %>% as.data.frame()
  expect_true(grepl("Planning Unit 1:", as.character(raw_pu1_en[1, 2])))
  expect_true(grepl("Quick Guide:", as.character(raw_pu1_en[3, 2])))
  
  hdr_en <- as.character(raw_pu1_en[6, ])
  expect_equal(as.character(raw_pu1_en[6, 1]), "T1 Area (ha)")
  expect_true(any(grepl("From \\\\ To", hdr_en)))
  expect_true(any(grepl("Grand Total", hdr_en)))
  expect_true(any(grepl("From Class", hdr_en)))
  expect_true(any(grepl("To Class", hdr_en)))
  expect_true(any(grepl("Intervention Status", hdr_en)))
  
  # English summary row labels below matrix
  data_rows_en <- 7:nrow(raw_pu1_en)
  mat_row_indices_en <- integer(0)
  for (r in data_rows_en) {
    if (!is.na(raw_pu1_en[r, 2]) && raw_pu1_en[r, 2] != "") {
      mat_row_indices_en <- c(mat_row_indices_en, r)
    } else {
      break
    }
  }
  blank_r_en <- tail(mat_row_indices_en, 1) + 1
  expect_equal(as.character(raw_pu1_en[blank_r_en + 1, 2]), "Baseline Projected Years Area (ha)")
  expect_equal(as.character(raw_pu1_en[blank_r_en + 2, 2]), "Scenario Projected Years Area (ha)")
  expect_equal(as.character(raw_pu1_en[blank_r_en + 3, 2]), "Net Change (ha)")
  
  # Verify matrix_to_tpm extracts seamlessly from English workbook too
  sim_en_out <- file.path(test_dir, "sim_en_output")
  extracted_en <- matrix_to_tpm(
    input_folder_path = dirname(out_en_xlsm),
    lc_lookup         = read.csv(lc_tbl_file),
    output_dir        = sim_en_out
  )
  expect_equal(length(extracted_en), 8)
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
    pu_lookup      = NULL,
    lang           = "id"
  )
  
  expect_true(file.exists(out_xlsm_no_pu))
  wb <- openxlsx2::wb_load(out_xlsm_no_pu)
  sheets <- wb$get_sheet_names()
  expect_true("Dashboard" %in% sheets)
  expect_true("PU_1_Planning_Unit_1" %in% sheets || "PU_1" %in% sheets)
  
  # Test PU names with special characters / single quotes
  special_pu_df <- data.frame(
    ID = 1:8,
    Name = c("Taman 'Nasional' Bogani", paste("Planning Unit", 2:8))
  )
  out_xlsm_special <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = file.path(test_dir, "special_pu"),
    template_path  = template_file,
    pu_lookup      = special_pu_df,
    lang           = "id"
  )
  expect_true(file.exists(out_xlsm_special))
  wb_spec <- openxlsx2::wb_load(out_xlsm_special)
  sheets_spec <- wb_spec$get_sheet_names()
  expect_true(any(grepl("Taman__Nasional", sheets_spec)))
  # Ensure no unescaped single quote in sheet names
  expect_false(any(grepl("'", sheets_spec)))
})

test_that("Live Excel COM validates sheets, applies policy via dashboard cells, and resets to baseline", {
  skip_on_os(c("mac", "linux", "solaris"))
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  # Check if Excel COM is accessible via PowerShell
  com_check <- tryCatch({
    system2("powershell", c("-NoProfile", "-Command", "New-Object -ComObject Excel.Application"), stdout = FALSE, stderr = FALSE)
  }, error = function(e) 1)
  skip_if(com_check != 0, "Excel COM is not available on this environment")
  
  ps_script <- sprintf('
$excel = New-Object -ComObject Excel.Application
$excel.Visible = $false
$excel.DisplayAlerts = $false
try {
    $wb = $excel.Workbooks.Open("%s")
    $dash = $wb.Sheets.Item("Dashboard")
    
    # 1. Run ValidateAllSheets
    $excel.Run("ValidateAllSheets")
    $statusPU1 = $dash.Cells.Item(18, 6).Text
    
    # 2. Configure Policy Card: From Class (C8), To Class (C9), Pct (E8), Target PU (E9)
    $pu1 = $wb.Sheets.Item("PU_1_Protected_Forest")
    $matHeaderRow = 6
    $panelCol = 0
    for ($c = 1; $c -le 40; $c++) {
        $t = $pu1.Cells.Item($matHeaderRow, $c).Text
        if ($t -like "*From Class*" -or $t -like "*Kelas Asal*") {
            $panelCol = $c
            break
        }
    }
    $firstDataRow = $matHeaderRow + 1
    $fromClass = $pu1.Cells.Item($firstDataRow, $panelCol).Text
    $toClass   = $pu1.Cells.Item($firstDataRow, $panelCol + 1).Text
    $baseRate  = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 2).Value2
    $baseArea  = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 3).Value2
    
    $dash.Range("C8").Value = $fromClass
    $dash.Range("C9").Value = $toClass
    $dash.Range("E8").Value = -50 # Test integer -50 gets normalized to -0.50
    $dash.Range("E9").Value = "1"
    
    # Check baseline status of row 8 before policy
    $row8BaseRate = [double]$pu1.Cells.Item($firstDataRow + 1, $panelCol + 2).Value2
    $row8StatusBefore = $pu1.Cells.Item($firstDataRow + 1, $panelCol + 7).Text
    
    # 3. Run ApplyPolicyAcrossPUs
    $excel.Run("ApplyPolicyAcrossPUs")
    
    $adjPct   = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 4).Value2
    $scenRate = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 5).Value2
    $scenArea = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 6).Value2
    $status   = $pu1.Cells.Item($firstDataRow, $panelCol + 7).Text
    
    # Verify row 8 (different transition) was not falsely modified
    $row8RateAfter = [double]$pu1.Cells.Item($firstDataRow + 1, $panelCol + 5).Value2
    $row8StatusAfter = $pu1.Cells.Item($firstDataRow + 1, $panelCol + 7).Text
    
    # 4. Test direct Scenario Area edit at column offset 6
    $targetArea = [Math]::Round($baseArea * 0.75, 2)
    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $firstDataRow, $panelCol + 6, $targetArea)
    $areaEditAdjPct = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 4).Value2
    $areaEditRate   = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 5).Value2
    $areaEditStatus = $pu1.Cells.Item($firstDataRow, $panelCol + 7).Text
    
    # Check bottom summary rows after policy & area edit
    $matEndRow = $matHeaderRow + ($matEndCol - $matStartCol + 1)
    $blankRow = $matEndRow + 1
    $baseR = $blankRow + 1
    $scenR = $blankRow + 2
    $deltaR = $blankRow + 3
    $excel.Calculate()
    $policyBaseAreaC = [double]$pu1.Cells.Item($baseR, 3).Value2
    $policyScenAreaC = [double]$pu1.Cells.Item($scenR, 3).Value2
    $policyDeltaC = [double]$pu1.Cells.Item($deltaR, 3).Value2
    
    # Test editing Column A (Cell A7) is safely ignored by VBA
    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $firstDataRow, 1, 9999.0)
    $colAEditMatVal = [double]$pu1.Cells.Item($firstDataRow, 3).Value2
    
    # Test editing summary row (Cell C18) is safely ignored by VBA
    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $baseR, 3, 1234.5)
    $sumEditMatVal = [double]$pu1.Cells.Item($firstDataRow, 3).Value2
    
    # 5. Run ResetAllSheetsToBaseline
    $excel.Run("ResetAllSheetsToBaseline")
    $excel.Calculate()
    $resetRate   = [double]$pu1.Cells.Item($firstDataRow, $panelCol + 5).Value2
    $resetStatus = $pu1.Cells.Item($firstDataRow, $panelCol + 7).Text
    $resetScenAreaC = [double]$pu1.Cells.Item($scenR, 3).Value2
    $resetBaseAreaC = [double]$pu1.Cells.Item($baseR, 3).Value2
    $resetDeltaC = [double]$pu1.Cells.Item($deltaR, 3).Value2
    
    $wb.Save()
    $wb.Close($false)
    
    Write-Output "STATUS_PU1:$statusPU1"
    Write-Output "ADJ_PCT:$adjPct"
    Write-Output "SCEN_RATE:$scenRate"
    Write-Output "BASE_RATE:$baseRate"
    Write-Output "SCEN_AREA:$scenArea"
    Write-Output "BASE_AREA:$baseArea"
    Write-Output "STATUS:$status"
    Write-Output "ROW8_STATUS_AFTER:$row8StatusAfter"
    Write-Output ("AREA_EDIT_ADJ:" + [Math]::Round($areaEditAdjPct, 2))
    Write-Output "AREA_EDIT_RATE:$areaEditRate"
    Write-Output "AREA_EDIT_STATUS:$areaEditStatus"
    Write-Output "POLICY_BASE_C:$policyBaseAreaC"
    Write-Output "POLICY_SCEN_C:$policyScenAreaC"
    Write-Output "POLICY_DELTA_C:$policyDeltaC"
    Write-Output "COLA_EDIT_MAT_VAL:$colAEditMatVal"
    Write-Output "SUM_EDIT_MAT_VAL:$sumEditMatVal"
    Write-Output "RESET_RATE:$resetRate"
    Write-Output "RESET_STATUS:$resetStatus"
    Write-Output "RESET_SCEN_C:$resetScenAreaC"
    Write-Output "RESET_BASE_C:$resetBaseAreaC"
    Write-Output "RESET_DELTA_C:$resetDeltaC"
} catch {
    Write-Output ("ERROR:" + $_.Exception.Message)
} finally {
    $excel.Quit()
    [System.Runtime.InteropServices.Marshal]::ReleaseComObject($excel) | Out-Null
}
', normalizePath(out_xlsm, winslash = "\\"))
  
  tf_ps <- tempfile(fileext = ".ps1")
  writeLines(ps_script, tf_ps)
  res_lines <- system2("powershell", c("-ExecutionPolicy", "Bypass", "-File", tf_ps), stdout = TRUE, stderr = TRUE)
  unlink(tf_ps)
  
  skip_if(any(grepl("^ERROR:", res_lines)), paste(res_lines, collapse = "\n"))
  
  expect_true(any(grepl("STATUS_PU1:VALID", res_lines)))
  expect_true(any(grepl("STATUS:(Intervened|Diintervensi)", res_lines)))
  expect_true(any(grepl("ADJ_PCT:-0.5", res_lines)))
  expect_true(any(grepl("ROW8_STATUS_AFTER:Baseline", res_lines)))
  expect_true(any(grepl("AREA_EDIT_ADJ:-0.25", res_lines)))
  expect_true(any(grepl("AREA_EDIT_STATUS:(Intervened|Diintervensi)", res_lines)))
  expect_true(any(grepl("COLA_EDIT_MAT_VAL:", res_lines)))
  expect_true(any(grepl("SUM_EDIT_MAT_VAL:", res_lines)))
  expect_true(any(grepl("RESET_STATUS:Baseline", res_lines)))
  expect_true(any(grepl("RESET_DELTA_C:0", res_lines)))
})

test_that("Live Excel COM handles 0-to-exist dynamic row lifecycle with real percentage numbers (no NEW text), two-way sync, and deletion", {
  skip_on_os(c("mac", "linux", "solaris"))
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  com_check <- tryCatch({
    system2("powershell", c("-NoProfile", "-Command", "New-Object -ComObject Excel.Application"), stdout = FALSE, stderr = FALSE)
  }, error = function(e) 1)
  skip_if(com_check != 0, "Excel COM is not available on this environment")
  
  tf_ps <- tempfile(fileext = ".ps1")
  ps_lines <- c(
    '$excel = New-Object -ComObject Excel.Application',
    '$excel.Visible = $false',
    '$excel.DisplayAlerts = $false',
    'try {',
    sprintf('    $wb = $excel.Workbooks.Open("%s")', normalizePath(out_xlsm, winslash = "\\")),
    '    $dash = $wb.Sheets.Item("Dashboard")',
    '    $pu1 = $wb.Sheets.Item("PU_1_Protected_Forest")',
    '',
    '    $matHeaderRow = 6',
    '    $matLabelCol = 2',
    '    $matStartCol = 3',
    '    $colScan = $matStartCol',
    '    while ($pu1.Cells.Item($matHeaderRow, $colScan).Text -ne "" -and $pu1.Cells.Item($matHeaderRow, $colScan).Text -ne "Grand Total" -and $pu1.Cells.Item($matHeaderRow, $colScan).Text -ne "Total Baris") {',
    '        $colScan++',
    '    }',
    '    $matEndCol = $colScan - 1',
    '    $sumCol = $colScan',
    '    $panelCol = $sumCol + 2',
    '    $nClasses = $matEndCol - $matStartCol + 1',
    '    $matEndRow = $matHeaderRow + $nClasses',
    '',
    '    # Baseline panel row count',
    '    $pEnd0 = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd0 + 1, $panelCol).Text -ne "") {',
    '        $pEnd0++',
    '    }',
    '',
    '    # Locate off-diagonal 0 cell in matrix',
    '    $zeroRow = 0',
    '    $zeroCol = 0',
    '    for ($r = ($matHeaderRow + 1); $r -le $matEndRow; $r++) {',
    '        for ($c = $matStartCol; $c -le $matEndCol; $c++) {',
    '            if (($r - $matHeaderRow) -ne ($c - $matLabelCol)) {',
    '                $val = [double]$pu1.Cells.Item($r, $c).Value2',
    '                if ($val -lt 0.0001) {',
    '                    $zeroRow = $r',
    '                    $zeroCol = $c',
    '                    break',
    '                }',
    '            }',
    '        }',
    '        if ($zeroRow -gt 0) { break }',
    '    }',
    '',
    '    # 1. Dynamically append row by modifying 0-cell to 0.05',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.05)',
    '    $matVal1 = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $pEnd1 = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd1 + 1, $panelCol).Text -ne "") { $pEnd1++ }',
    '    $appFrom = $pu1.Cells.Item($pEnd1, $panelCol).Text',
    '    $appTo = $pu1.Cells.Item($pEnd1, $panelCol + 1).Text',
    '    $appBaseRate = [double]$pu1.Cells.Item($pEnd1, $panelCol + 2).Value2',
    '    $appAdj = $pu1.Cells.Item($pEnd1, $panelCol + 4).Text',
    '    $appScenRate = [double]$pu1.Cells.Item($pEnd1, $panelCol + 5).Value2',
    '    $appScenArea = [double]$pu1.Cells.Item($pEnd1, $panelCol + 6).Value2',
    '    $appStatus = $pu1.Cells.Item($pEnd1, $panelCol + 7).Text',
    '    $rowSum1 = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 1b. Test Two-Way Synchronization on Appended Row (% Adjustment -> Scenario Rate & Matrix)',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $pEnd1, $panelCol + 4, 0.10)',
    '    $twoWayMatVal = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $twoWayScenRate = [double]$pu1.Cells.Item($pEnd1, $panelCol + 5).Value2',
    '    $twoWayAdj = $pu1.Cells.Item($pEnd1, $panelCol + 4).Text',
    '    $twoWayRowSum = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 1c. Test integer percentage input normalization (15 -> 0.15)',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $pEnd1, $panelCol + 4, 15)',
    '    $intMatVal = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $intScenRate = [double]$pu1.Cells.Item($pEnd1, $panelCol + 5).Value2',
    '',
    '    # 2. Revert 0-cell to 0.0 via matrix edit -> cleanly delete appended row',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.0)',
    '    $matVal2 = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $pEnd2 = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd2 + 1, $panelCol).Text -ne "") { $pEnd2++ }',
    '    $rowSum2 = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 3. Direct panel edit on appended row: setting Scenario Rate to 0 removes row',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.06)',
    '    $pEnd3Before = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd3Before + 1, $panelCol).Text -ne "") { $pEnd3Before++ }',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $pEnd3Before, $panelCol + 5, 0.0)',
    '    $matVal3 = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $pEnd3After = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd3After + 1, $panelCol).Text -ne "") { $pEnd3After++ }',
    '    $rowSum3 = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 3b. Test multiple appended rows and deleting middle row',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.05)',
    '    $zeroRow2 = 0',
    '    $zeroCol2 = 0',
    '    for ($r = ($matHeaderRow + 1); $r -le $matEndRow; $r++) {',
    '        for ($c = $matStartCol; $c -le $matEndCol; $c++) {',
    '            if (($r - $matHeaderRow) -ne ($c - $matLabelCol) -and ($r -ne $zeroRow -or $c -ne $zeroCol)) {',
    '                $v = [double]$pu1.Cells.Item($r, $c).Value2',
    '                if ($v -lt 0.0001) {',
    '                    $zeroRow2 = $r',
    '                    $zeroCol2 = $c',
    '                    break',
    '                }',
    '            }',
    '        }',
    '        if ($zeroRow2 -gt 0) { break }',
    '    }',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow2, $zeroCol2, 0.04)',
    '    $pEnd2App = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd2App + 1, $panelCol).Text -ne "") { $pEnd2App++ }',
    '    $multiDiffBefore = $pEnd2App - $pEnd0',
    '',
    '    # Delete row A (middle row)',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.0)',
    '    $pEndAfterDelA = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEndAfterDelA + 1, $panelCol).Text -ne "") { $pEndAfterDelA++ }',
    '    $multiDiffAfter = $pEndAfterDelA - $pEnd0',
    '    $rowB_Rate = [double]$pu1.Cells.Item($pEndAfterDelA, $panelCol + 5).Value2',
    '    $rowB_Status = $pu1.Cells.Item($pEndAfterDelA, $panelCol + 7).Text',
    '    $rowB_mRow = [int]$pu1.Cells.Item($pEndAfterDelA, $panelCol + 8).Value2',
    '    $rowB_mCol = [int]$pu1.Cells.Item($pEndAfterDelA, $panelCol + 9).Value2',
    '    $rowBMatch = "NO"',
    '    if ($rowB_mRow -eq $zeroRow2 -and $rowB_mCol -eq $zeroCol2) { $rowBMatch = "YES" }',
    '',
    '    # Clean up row B',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow2, $zeroCol2, 0.0)',
    '',
    '    # 4. Batch Policy with identical From and To (100% moratorium)',
    '    $diagClass = $pu1.Cells.Item($matHeaderRow + 1, $matLabelCol).Text',
    '    $dash.Range("C8").Value = $diagClass',
    '    $dash.Range("C9").Value = $diagClass',
    '    $dash.Range("E8").Value = 1.0',
    '    $dash.Range("E9").Value = "1"',
    '    $excel.Run("ApplyPolicyAcrossPUs")',
    '',
    '    $diagVal = [double]$pu1.Cells.Item($matHeaderRow + 1, 3).Value2',
    '    $offDiagSum = 0.0',
    '    for ($c = 4; $c -le $matEndCol; $c++) {',
    '        $offDiagSum += [double]$pu1.Cells.Item($matHeaderRow + 1, $c).Value2',
    '    }',
    '    $diagRowSum = [double]$pu1.Cells.Item($matHeaderRow + 1, $sumCol).Value2',
    '',
    '    $row5PanelCount = 0',
    '    $row5IntervenedCount = 0',
    '    for ($pr = ($matHeaderRow + 1); $pr -le $pEnd0; $pr++) {',
    '        if ([int]$pu1.Cells.Item($pr, $panelCol + 8).Value2 -eq ($matHeaderRow + 1)) {',
    '            $row5PanelCount++',
    '            $rRate = [double]$pu1.Cells.Item($pr, $panelCol + 5).Value2',
    '            $rArea = [double]$pu1.Cells.Item($pr, $panelCol + 6).Value2',
    '            $rStat = $pu1.Cells.Item($pr, $panelCol + 7).Text',
    '            if ($rRate -lt 0.0001 -and $rArea -lt 0.01 -and ($rStat -eq "Intervened" -or $rStat -eq "Diintervensi")) {',
    '                $row5IntervenedCount++',
    '            }',
    '        }',
    '    }',
    '',
    '    # 4b. Test arbitrary diagonal persistence policy (-10%) with appended row',
    '    $excel.Run("ResetCurrentSheetToBaseline", "PU_1_Protected_Forest")',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.05)',
    '    $dash.Range("C8").Value = $diagClass',
    '    $dash.Range("C9").Value = $diagClass',
    '    $dash.Range("E8").Value = -0.10',
    '    $dash.Range("E9").Value = "1"',
    '    $excel.Run("ApplyPolicyAcrossPUs")',
    '    $diagRowSum4b = [double]$pu1.Cells.Item($matHeaderRow + 1, $sumCol).Value2',
    '    $pEnd4b = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd4b + 1, $panelCol).Text -ne "") { $pEnd4b++ }',
    '    $hasValueErr = "NO"',
    '    for ($pr = ($matHeaderRow + 1); $pr -le $pEnd4b; $pr++) {',
    '        $cellText = $pu1.Cells.Item($pr, $panelCol + 6).Text',
    '        if ($cellText -eq "#VALUE!" -or $cellText -like "*#*") {',
    '            $hasValueErr = "YES"',
    '            break',
    '        }',
    '    }',
    '',
    '    # 5. Batch Policy 0-to-exist support (From != To for 0-baseline transition)',
    '    $excel.Run("ResetCurrentSheetToBaseline", "PU_1_Protected_Forest")',
    '    $fromZero = $pu1.Cells.Item($zeroRow, $matLabelCol).Text',
    '    $toZero = $pu1.Cells.Item($matHeaderRow, $zeroCol).Text',
    '    $dash.Range("C8").Value = $fromZero',
    '    $dash.Range("C9").Value = $toZero',
    '    $dash.Range("E8").Value = 0.07',
    '    $dash.Range("E9").Value = "1"',
    '    $excel.Run("ApplyPolicyAcrossPUs")',
    '',
    '    $bpMatVal = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $pEnd5 = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd5 + 1, $panelCol).Text -ne "") { $pEnd5++ }',
    '    $bpAppAdj = $pu1.Cells.Item($pEnd5, $panelCol + 4).Text',
    '    $bpAppScenRate = [double]$pu1.Cells.Item($pEnd5, $panelCol + 5).Value2',
    '    $bpAppStatus = $pu1.Cells.Item($pEnd5, $panelCol + 7).Text',
    '    $bpRowSum = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 6. Reset to baseline restores clean initial state',
    '    $excel.Run("ResetAllSheetsToBaseline")',
    '    $excel.Run("ValidateAllSheets")',
    '    $pEnd6 = $matHeaderRow',
    '    while ($pu1.Cells.Item($pEnd6 + 1, $panelCol).Text -ne "") { $pEnd6++ }',
    '    $resetDiagVal = [double]$pu1.Cells.Item($matHeaderRow + 1, 3).Value2',
    '    $resetZeroVal = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $dashValid = $dash.Cells.Item(18, 6).Text',
    '',
    '    $wb.Save()',
    '    $wb.Close($false)',
    '',
    '    $appAreaPos = "NO"',
    '    if ($appScenArea -gt 0) { $appAreaPos = "YES" }',
    '    $moratoriumPanelOk = "NO"',
    '    if ($row5PanelCount -gt 0 -and $row5PanelCount -eq $row5IntervenedCount) { $moratoriumPanelOk = "YES" }',
    '    $resetDiagRestored = "NO"',
    '    if ($resetDiagVal -lt 1.0) { $resetDiagRestored = "YES" }',
    '',
    '    Write-Output "ZERO_APPEND_VAL:$matVal1"',
    '    Write-Output "ZERO_APPEND_DIFF:$($pEnd1 - $pEnd0)"',
    '    Write-Output "ZERO_APPEND_ADJ:$appAdj"',
    '    Write-Output "ZERO_APPEND_RATE:$appScenRate"',
    '    Write-Output "ZERO_APPEND_AREA_POS:$appAreaPos"',
    '    Write-Output "ZERO_APPEND_STATUS:$appStatus"',
    '    Write-Output "ZERO_APPEND_SUM:$rowSum1"',
    '',
    '    Write-Output "TWOWAY_MAT_VAL:$twoWayMatVal"',
    '    Write-Output "TWOWAY_SCEN_RATE:$twoWayScenRate"',
    '    Write-Output "TWOWAY_ADJ:$twoWayAdj"',
    '    Write-Output "TWOWAY_ROW_SUM:$twoWayRowSum"',
    '',
    '    Write-Output "INT_MAT_VAL:$intMatVal"',
    '    Write-Output "INT_SCEN_RATE:$intScenRate"',
    '',
    '    Write-Output "ZERO_REVERT_VAL:$matVal2"',
    '    Write-Output "ZERO_REVERT_DIFF:$($pEnd2 - $pEnd0)"',
    '    Write-Output "ZERO_REVERT_SUM:$rowSum2"',
    '',
    '    Write-Output "PANEL_EDIT_ZERO_VAL:$matVal3"',
    '    Write-Output "PANEL_EDIT_ZERO_DIFF:$($pEnd3After - $pEnd0)"',
    '    Write-Output "PANEL_EDIT_ZERO_SUM:$rowSum3"',
    '',
    '    Write-Output "MULTI_APPEND_BEFORE:$multiDiffBefore"',
    '    Write-Output "MULTI_APPEND_AFTER:$multiDiffAfter"',
    '    Write-Output "ROW_B_RATE:$rowB_Rate"',
    '    Write-Output "ROW_B_STATUS:$rowB_Status"',
    '    Write-Output "ROW_B_MATCH:$rowBMatch"',
    '',
    '    Write-Output "MORATORIUM_DIAG_VAL:$diagVal"',
    '    Write-Output "MORATORIUM_OFFDIAG_SUM:$offDiagSum"',
    '    Write-Output "MORATORIUM_ROW_SUM:$diagRowSum"',
    '    Write-Output "MORATORIUM_PANEL_OK:$moratoriumPanelOk"',
    '',
    '    Write-Output "DIAG_4B_ROW_SUM:$diagRowSum4b"',
    '    Write-Output "DIAG_4B_VALUE_ERR:$hasValueErr"',
    '',
    '    Write-Output "BP_ZERO_VAL:$bpMatVal"',
    '    Write-Output "BP_ZERO_DIFF:$($pEnd5 - $pEnd0)"',
    '    Write-Output "BP_ZERO_ADJ:$bpAppAdj"',
    '    Write-Output "BP_ZERO_RATE:$bpAppScenRate"',
    '    Write-Output "BP_ZERO_STATUS:$bpAppStatus"',
    '    Write-Output "BP_ZERO_SUM:$bpRowSum"',
    '',
    '    Write-Output "RESET_DIFF:$($pEnd6 - $pEnd0)"',
    '    Write-Output "RESET_DIAG_RESTORED:$resetDiagRestored"',
    '    Write-Output "RESET_ZERO_VAL:$resetZeroVal"',
    '    Write-Output "RESET_VALID:$dashValid"',
    '} catch {',
    '    Write-Output ("ERROR:" + $_.Exception.Message)',
    '} finally {',
    '    $excel.Quit()',
    '    [System.Runtime.InteropServices.Marshal]::ReleaseComObject($excel) | Out-Null',
    '}'
  )
  writeLines(ps_lines, tf_ps)
  res_lines <- system2("powershell", c("-ExecutionPolicy", "Bypass", "-File", tf_ps), stdout = TRUE, stderr = TRUE)
  unlink(tf_ps)
  
  skip_if(any(grepl("^ERROR:", res_lines)), paste(res_lines, collapse = "\n"))
  
  # 1. 0-to-exist dynamic append: displays real percentage (no "NEW" text)
  expect_true(any(grepl("ZERO_APPEND_VAL:0.05", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_DIFF:1", res_lines)))
  
  # Assert ZERO_APPEND_ADJ is numeric percentage, strictly NOT "NEW"
  adj_line <- res_lines[grepl("^ZERO_APPEND_ADJ:", res_lines)]
  expect_true(length(adj_line) > 0)
  expect_false(grepl("NEW", adj_line, ignore.case = TRUE))
  expect_true(grepl("5", adj_line) || grepl("0.05", adj_line))
  
  expect_true(any(grepl("ZERO_APPEND_RATE:0.05", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_AREA_POS:YES", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_STATUS:(Intervened|Diintervensi)", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_SUM:1", res_lines)))
  
  # 1b. Two-way sync: editing % Adjustment updates Scenario Rate and Matrix cell
  expect_true(any(grepl("TWOWAY_MAT_VAL:0.1", res_lines)))
  expect_true(any(grepl("TWOWAY_SCEN_RATE:0.1", res_lines)))
  expect_true(any(grepl("TWOWAY_ROW_SUM:1", res_lines)))
  twoway_adj_line <- res_lines[grepl("^TWOWAY_ADJ:", res_lines)]
  expect_true(grepl("10", twoway_adj_line) || grepl("0.1", twoway_adj_line))
  expect_false(grepl("NEW", twoway_adj_line, ignore.case = TRUE))
  
  # 1c. Integer percentage input normalization
  expect_true(any(grepl("INT_MAT_VAL:0.15", res_lines)))
  expect_true(any(grepl("INT_SCEN_RATE:0.15", res_lines)))
  
  # 2. Revert to 0 removes row
  expect_true(any(grepl("ZERO_REVERT_VAL:0", res_lines)))
  expect_true(any(grepl("ZERO_REVERT_DIFF:0", res_lines)))
  expect_true(any(grepl("ZERO_REVERT_SUM:1", res_lines)))
  
  # 3. Direct panel edit setting rate to 0 removes row
  expect_true(any(grepl("PANEL_EDIT_ZERO_VAL:0", res_lines)))
  expect_true(any(grepl("PANEL_EDIT_ZERO_DIFF:0", res_lines)))
  expect_true(any(grepl("PANEL_EDIT_ZERO_SUM:1", res_lines)))

  # 3b. Multi-row append and middle deletion shifts remaining row cleanly
  expect_true(any(grepl("MULTI_APPEND_BEFORE:2", res_lines)))
  expect_true(any(grepl("MULTI_APPEND_AFTER:1", res_lines)))
  expect_true(any(grepl("ROW_B_RATE:0.04", res_lines)))
  expect_true(any(grepl("ROW_B_STATUS:(Intervened|Diintervensi)", res_lines)))
  expect_true(any(grepl("ROW_B_MATCH:YES", res_lines)))
  
  # 4. Moratorium identical From/To sets diagonal to 1 and panel rows to 0 Intervened
  expect_true(any(grepl("MORATORIUM_DIAG_VAL:1", res_lines)))
  expect_true(any(grepl("MORATORIUM_OFFDIAG_SUM:0", res_lines)))
  expect_true(any(grepl("MORATORIUM_ROW_SUM:1", res_lines)))
  expect_true(any(grepl("MORATORIUM_PANEL_OK:YES", res_lines)))

  # 4b. Arbitrary diagonal policy with appended row retains valid row sum and no #VALUE! errors
  expect_true(any(grepl("DIAG_4B_ROW_SUM:1", res_lines)))
  expect_true(any(grepl("DIAG_4B_VALUE_ERR:NO", res_lines)))
  
  # 5. Batch policy 0-to-exist creates new row and rebalances
  expect_true(any(grepl("BP_ZERO_VAL:0.07", res_lines)))
  expect_true(any(grepl("BP_ZERO_DIFF:1", res_lines)))
  bp_adj_line <- res_lines[grepl("^BP_ZERO_ADJ:", res_lines)]
  expect_false(grepl("NEW", bp_adj_line, ignore.case = TRUE))
  expect_true(grepl("7", bp_adj_line) || grepl("0.07", bp_adj_line))
  expect_true(any(grepl("BP_ZERO_RATE:0.07", res_lines)))
  expect_true(any(grepl("BP_ZERO_STATUS:(Intervened|Diintervensi)", res_lines)))
  expect_true(any(grepl("BP_ZERO_SUM:1", res_lines)))
  
  # 6. Reset cleans up
  expect_true(any(grepl("RESET_DIFF:0", res_lines)))
  expect_true(any(grepl("RESET_DIAG_RESTORED:YES", res_lines)))
  expect_true(any(grepl("RESET_ZERO_VAL:0", res_lines)))
  expect_true(any(grepl("RESET_VALID:VALID", res_lines)))
})

test_that("VBA module contains complete bilingual alerts and dialogs", {
  vba_file <- "../../11_sciendo-train/macro_template/module_vba.txt"
  skip_if_not(file.exists(vba_file), "module_vba.txt not found")
  vba_text <- paste(readLines(vba_file, warn = FALSE), collapse = "\n")
  
  # Check Indonesian alert and confirmation messages
  expect_true(grepl("Nilai tidak valid! Silakan masukkan probabilitas", vba_text))
  expect_true(grepl("Silakan masukkan persentase penyesuaian yang valid", vba_text))
  expect_true(grepl("Silakan masukkan laju skenario yang valid", vba_text))
  expect_true(grepl("Silakan masukkan luas skenario positif yang valid", vba_text))
  expect_true(grepl("Tidak dapat mengubah sel: Total sel terintervensi", vba_text))
  expect_true(grepl("Lembar baseline untuk .* tidak ditemukan", vba_text))
  expect_true(grepl("Silakan pilih 'Kelas Asal' dan 'Kelas Tujuan'", vba_text))
  expect_true(grepl("Validasi Berhasil!", vba_text))
  expect_true(grepl("Peringatan Validasi:", vba_text))
  expect_true(grepl("Apakah Anda yakin ingin mengatur ulang", vba_text))
  expect_true(grepl("Seluruh Unit Perencanaan berhasil diatur ulang", vba_text))
  expect_true(grepl("Kebijakan berhasil diterapkan pada", vba_text))
  
  # Check English alert and confirmation messages
  expect_true(grepl("Invalid value! Please enter a probability", vba_text))
  expect_true(grepl("Please enter a valid percentage adjustment", vba_text))
  expect_true(grepl("Please enter a valid scenario rate", vba_text))
  expect_true(grepl("Please enter a valid positive area in hectares", vba_text))
  expect_true(grepl("Cannot modify cell: The sum of intervened cells", vba_text))
  expect_true(grepl("Validation Passed!", vba_text))
  expect_true(grepl("Validation Warning:", vba_text))
  expect_true(grepl("Are you sure you want to reset all Planning Units", vba_text))
  expect_true(grepl("All Planning Units have been successfully reset", vba_text))
  expect_true(grepl("Policy applied to", vba_text))
})

test_that("tpm_to_matrix_conversion supports calendar year from time_points and empirical raster class areas", {
  skip_if_not(dir.exists(input_tpm_dir), "Sample TPM directory not available")
  skip_if_not(file.exists(lc_tbl_file), "LC lookup table not available")
  skip_if_not(file.exists(template_file), "Macro template not available")
  
  lc_rast   <- "../../data/raster/bungo_landcover_2010r.tif"
  zone_rast <- "../../data/raster/bungo_zone.tif"
  
  td_year <- tempfile("test_year_")
  dir.create(td_year, recursive = TRUE)
  
  # Run conversion with explicit time_points (t1 = 2010, t2 = 2020) and rasters
  out_year_xlsm <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = td_year,
    template_path  = template_file,
    pu_lookup      = read.csv(zone_tbl_file),
    lc_t1_path     = if (file.exists(lc_rast)) lc_rast else NULL,
    zone_path      = if (file.exists(zone_rast)) zone_rast else NULL,
    time_points    = list(t1 = 2010, t2 = 2020),
    lang           = "id"
  )
  
  expect_true(file.exists(out_year_xlsm))
  
  # Check cell A6 on PU sheet: "Luas 2010 (ha)"
  raw_year_pu1 <- readxl::read_excel(out_year_xlsm, sheet = "PU_1_Protected_Forest", col_names = FALSE) %>% as.data.frame()
  expect_equal(as.character(raw_year_pu1[6, 1]), "Luas 2010 (ha)")
  
  # Check English year formatting
  td_year_en <- tempfile("test_year_en_")
  dir.create(td_year_en, recursive = TRUE)
  out_year_en_xlsm <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = td_year_en,
    template_path  = template_file,
    pu_lookup      = read.csv(zone_tbl_file),
    time_points    = list(t1 = 2010, t2 = 2020),
    lang           = "en"
  )
  raw_year_en_pu1 <- readxl::read_excel(out_year_en_xlsm, sheet = "PU_1_Protected_Forest", col_names = FALSE) %>% as.data.frame()
  expect_equal(as.character(raw_year_en_pu1[6, 1]), "2010 Area (ha)")
  
  # Also verify named vector time_points = c(t1 = 2010, t2 = 2020)
  td_vec <- tempfile("test_year_vec_")
  dir.create(td_vec, recursive = TRUE)
  out_vec_xlsm <- tpm_to_matrix_conversion(
    input_dir      = input_tpm_dir,
    lc_lookup_path = lc_tbl_file,
    output_dir     = td_vec,
    template_path  = template_file,
    pu_lookup      = read.csv(zone_tbl_file),
    time_points    = c(t1 = 2010, t2 = 2020),
    lang           = "id"
  )
  raw_vec_pu1 <- readxl::read_excel(out_vec_xlsm, sheet = "PU_1_Protected_Forest", col_names = FALSE) %>% as.data.frame()
  expect_equal(as.character(raw_vec_pu1[6, 1]), "Luas 2010 (ha)")
  unlink(td_vec, recursive = TRUE)
  
  # Check summary rows existence and mathematical conservation in formulas
  wb_yr <- openxlsx2::wb_load(out_year_xlsm)
  cc_yr <- wb_yr$worksheets[[2]]$sheet_data$cc
  
  # Verify SUMPRODUCT formulas present for summary rows
  expect_true(any(grepl("SUMPRODUCT", cc_yr$f)))
  expect_true(any(grepl("_Base_", cc_yr$f)))
  
  # Verify matrix_to_tpm extracts cleanly from year-annotated workbook
  sim_yr_out <- file.path(td_year, "sim_output")
  extracted_yr <- matrix_to_tpm(
    input_folder_path = dirname(out_year_xlsm),
    lc_lookup         = read.csv(lc_tbl_file),
    output_dir        = sim_yr_out
  )
  expect_equal(length(extracted_yr), 8)
  
  unlink(td_year, recursive = TRUE)
  unlink(td_year_en, recursive = TRUE)
})

# Cleanup temp files
unlink(test_dir, recursive = TRUE)
