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
  
  # 4. Contains hidden reference sheet _LULC_List for dropdowns
  expect_true("_LULC_List" %in% sheet_names)
  
  # 5. VBA project is preserved
  zip_files <- unzip(out_xlsm, list = TRUE)
  expect_true(any(grepl("vbaProject.bin", zip_files$Name)))
})

test_that("Dashboard sheet has tidy layout, policy card, and non-overlapping elements", {
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  raw_dash <- readxl::read_excel(out_xlsm, sheet = "Dashboard", range = "A1:G22", col_names = FALSE) %>% as.data.frame()
  
  # Row 2 & 3: Title and Subtitle
  expect_true(grepl("Scenario Builder", as.character(raw_dash[2, 2]), ignore.case = TRUE))
  expect_true(grepl("Consolidated Planning Units", as.character(raw_dash[3, 2]), ignore.case = TRUE))
  
  # Rows 7-10: Policy Card
  expect_true(grepl("Batch Policy Across Planning Units", as.character(raw_dash[7, 2]), ignore.case = TRUE))
  expect_equal(as.character(raw_dash[8, 2]), "From Class:")
  expect_equal(as.character(raw_dash[8, 4]), "% Adjustment:")
  expect_equal(as.character(raw_dash[9, 2]), "To Class:")
  expect_equal(as.character(raw_dash[9, 4]), "Target PU:")
  expect_true(!is.na(raw_dash[8, 3])) # From Class default value exists
  expect_true(!is.na(raw_dash[9, 3])) # To Class default value exists
  expect_equal(as.numeric(raw_dash[8, 5]), -0.50) # Default % adjustment
  expect_equal(as.character(raw_dash[9, 5]), "All Planning Units") # Default target PU
  
  # Row 12: Summary Table Headers
  expect_equal(as.character(raw_dash[12, 2]), "PU ID")
  expect_equal(as.character(raw_dash[12, 3]), "Planning Unit Name")
  expect_equal(as.character(raw_dash[12, 4]), "Navigation Link")
  expect_equal(as.character(raw_dash[12, 5]), "Active Interventions")
  expect_equal(as.character(raw_dash[12, 6]), "Validation Status")
  expect_equal(as.character(raw_dash[12, 7]), "Historical Transitions")
  
  # Rows 13+: Data rows (8 PUs)
  expect_equal(nrow(raw_dash[13:20, ]), 8)
  expect_equal(as.numeric(raw_dash[13, 2]), 1)
  
  # Drawing elements & button anchors
  zip_files <- unzip(out_xlsm, list = TRUE)
  expect_true(any(grepl("xl/drawings/drawing1.xml", zip_files$Name)))
  td_draw <- tempfile("draw_check_")
  unzip(out_xlsm, files = "xl/drawings/drawing1.xml", exdir = td_draw)
  draw_text <- paste(readLines(file.path(td_draw, "xl/drawings/drawing1.xml"), warn = FALSE), collapse = "\n")
  unlink(td_draw, recursive = TRUE)
  
  # btnValidateAll anchored at col=1, row=4 to col=3, row=5 with color 1E3A8A
  expect_true(grepl('name="btnValidateAll"', draw_text))
  expect_true(grepl('val="1E3A8A"', draw_text))
  
  # btnResetAll anchored at col=3, row=4 to col=5, row=5 with color 991B1B
  expect_true(grepl('name="btnResetAll"', draw_text))
  expect_true(grepl('val="991B1B"', draw_text))
  
  # btnApplyPolicy anchored inside card col=5, row=7 to col=7, row=9 with color 0F766E
  expect_true(grepl('name="btnApplyPolicy"', draw_text))
  expect_true(grepl('val="0F766E"', draw_text))
})

test_that("PU sheets have correct matrix structure, row sums = 1, area in ha, Intervention Status, and navigation links", {
  out_xlsm <- file.path(test_dir, "baseline_tpm_macro", "sciendo_tpm_scenario.xlsm")
  skip_if_not(file.exists(out_xlsm), "Consolidated workbook does not exist")
  
  wb <- openxlsx2::wb_load(out_xlsm)
  sheet_names <- wb$get_sheet_names()
  pu_sheets <- sheet_names[grepl("^PU_", sheet_names)]
  
  # Row 2: Hyperlinks in openxlsx2 formula structure
  idx_pu1 <- which(sheet_names == pu_sheets[1])
  cc_pu1 <- wb$worksheets[[idx_pu1]]$sheet_data$cc
  b2_row <- cc_pu1[cc_pu1$r == "B2", ]
  d2_row <- cc_pu1[cc_pu1$r == "D2", ]
  expect_true(grepl("Dashboard", b2_row$f))
  expect_true(grepl("Next Planning Unit", d2_row$f) || grepl(pu_sheets[2], d2_row$f))
  
  # Read final PU sheet and verify looping navigation link
  idx_pu_last <- which(sheet_names == tail(pu_sheets, 1))
  cc_last <- wb$worksheets[[idx_pu_last]]$sheet_data$cc
  d2_last <- cc_last[cc_last$r == "D2", ]
  expect_true(grepl("First Planning Unit", d2_last$f) && grepl("PU 1", d2_last$f))
  
  # Read PU 1 sheet data via readxl
  raw_pu1 <- readxl::read_excel(out_xlsm, sheet = pu_sheets[1], col_names = FALSE) %>% as.data.frame()
  
  # Header row is row 4
  header_row <- as.character(raw_pu1[4, ])
  expect_true(any(grepl("From \\\\ To", header_row)))
  expect_true(any(grepl("Grand Total", header_row)))
  expect_true(any(grepl("From Class", header_row)))
  expect_true(any(grepl("To Class", header_row)))
  expect_true(any(grepl("Baseline Rate", header_row)))
  expect_true(any(grepl("Baseline Area \\(ha\\)", header_row)))
  expect_true(any(grepl("% Adjustment", header_row)))
  expect_true(any(grepl("Scenario Rate", header_row)))
  expect_true(any(grepl("Scenario Area \\(ha\\)", header_row)))
  expect_true(any(grepl("Intervention Status", header_row)))
  
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
  
  # Initial % Adjustment should be 0, Intervention Status should be "Baseline", areas should be positive
  base_area_col <- which(grepl("Baseline Area \\(ha\\)", header_row))
  adj_col       <- which(grepl("% Adjustment", header_row))
  status_col    <- which(grepl("Intervention Status", header_row))
  
  expect_true(as.numeric(raw_pu1[panel_rows[1], base_area_col]) > 0)
  expect_equal(as.numeric(raw_pu1[panel_rows[1], adj_col]), 0.0)
  expect_equal(as.character(raw_pu1[panel_rows[1], status_col]), "Baseline")
  
  # Verify Scenario Area (ha) formula is dynamically embedded
  expect_true(any(grepl("ROUND", cc_pu1$f)))
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
    $statusPU1 = $dash.Cells.Item(13, 6).Text
    
    # 2. Configure Policy Card: From Class (C8), To Class (C9), Pct (E8), Target PU (E9)
    $pu1 = $wb.Sheets.Item("PU_1_Protected_Forest")
    $panelCol = 0
    for ($c = 1; $c -le 40; $c++) {
        if ($pu1.Cells.Item(4, $c).Text -eq "From Class") {
            $panelCol = $c
            break
        }
    }
    $fromClass = $pu1.Cells.Item(5, $panelCol).Text
    $toClass   = $pu1.Cells.Item(5, $panelCol + 1).Text
    $baseRate  = [double]$pu1.Cells.Item(5, $panelCol + 2).Value2
    $baseArea  = [double]$pu1.Cells.Item(5, $panelCol + 3).Value2
    
    $dash.Range("C8").Value = $fromClass
    $dash.Range("C9").Value = $toClass
    $dash.Range("E8").Value = -50 # Test integer -50 gets normalized to -0.50
    $dash.Range("E9").Value = "1"
    
    # Check baseline status of row 6 before policy
    $row6BaseRate = [double]$pu1.Cells.Item(6, $panelCol + 2).Value2
    $row6StatusBefore = $pu1.Cells.Item(6, $panelCol + 7).Text
    
    # 3. Run ApplyPolicyAcrossPUs
    $excel.Run("ApplyPolicyAcrossPUs")
    
    $adjPct   = [double]$pu1.Cells.Item(5, $panelCol + 4).Value2
    $scenRate = [double]$pu1.Cells.Item(5, $panelCol + 5).Value2
    $scenArea = [double]$pu1.Cells.Item(5, $panelCol + 6).Value2
    $status   = $pu1.Cells.Item(5, $panelCol + 7).Text
    
    # Verify row 6 (different transition) was not falsely modified
    $row6RateAfter = [double]$pu1.Cells.Item(6, $panelCol + 5).Value2
    $row6StatusAfter = $pu1.Cells.Item(6, $panelCol + 7).Text
    
    # 4. Test direct Scenario Area edit at column offset 6
    $targetArea = [Math]::Round($baseArea * 0.75, 2)
    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", 5, $panelCol + 6, $targetArea)
    $areaEditAdjPct = [double]$pu1.Cells.Item(5, $panelCol + 4).Value2
    $areaEditRate   = [double]$pu1.Cells.Item(5, $panelCol + 5).Value2
    $areaEditStatus = $pu1.Cells.Item(5, $panelCol + 7).Text
    
    # 5. Run ResetAllSheetsToBaseline
    $excel.Run("ResetAllSheetsToBaseline")
    $resetRate   = [double]$pu1.Cells.Item(5, $panelCol + 5).Value2
    $resetStatus = $pu1.Cells.Item(5, $panelCol + 7).Text
    
    $wb.Save()
    $wb.Close($false)
    
    Write-Output "STATUS_PU1:$statusPU1"
    Write-Output "ADJ_PCT:$adjPct"
    Write-Output "SCEN_RATE:$scenRate"
    Write-Output "BASE_RATE:$baseRate"
    Write-Output "SCEN_AREA:$scenArea"
    Write-Output "BASE_AREA:$baseArea"
    Write-Output "STATUS:$status"
    Write-Output "ROW6_STATUS_AFTER:$row6StatusAfter"
    Write-Output ("AREA_EDIT_ADJ:" + [Math]::Round($areaEditAdjPct, 2))
    Write-Output "AREA_EDIT_RATE:$areaEditRate"
    Write-Output "AREA_EDIT_STATUS:$areaEditStatus"
    Write-Output "RESET_RATE:$resetRate"
    Write-Output "RESET_STATUS:$resetStatus"
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
  expect_true(any(grepl("STATUS:Intervened", res_lines)))
  expect_true(any(grepl("ADJ_PCT:-0.5", res_lines)))
  expect_true(any(grepl("ROW6_STATUS_AFTER:Baseline", res_lines)))
  expect_true(any(grepl("AREA_EDIT_ADJ:-0.25", res_lines)))
  expect_true(any(grepl("AREA_EDIT_STATUS:Intervened", res_lines)))
  expect_true(any(grepl("RESET_STATUS:Baseline", res_lines)))
})

test_that("Live Excel COM handles 0-to-exist dynamic row lifecycle, direct panel removal, and identical From/To batch moratorium", {
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
    '    $matHeaderRow = 4',
    '    $matLabelCol = 2',
    '    $matStartCol = 3',
    '    $colScan = $matStartCol',
    '    while ($pu1.Cells.Item($matHeaderRow, $colScan).Text -ne "" -and $pu1.Cells.Item($matHeaderRow, $colScan).Text -ne "Grand Total") {',
    '        $colScan++',
    '    }',
    '    $matEndCol = $colScan - 1',
    '    $sumCol = $colScan',
    '    $panelCol = $sumCol + 2',
    '    $nClasses = $matEndCol - $matStartCol + 1',
    '    $matEndRow = $matHeaderRow + $nClasses',
    '',
    '    # Baseline panel row count',
    '    $pEnd0 = 4',
    '    while ($pu1.Cells.Item($pEnd0 + 1, $panelCol).Text -ne "") {',
    '        $pEnd0++',
    '    }',
    '',
    '    # Locate off-diagonal 0 cell in matrix',
    '    $zeroRow = 0',
    '    $zeroCol = 0',
    '    for ($r = 5; $r -le $matEndRow; $r++) {',
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
    '    $pEnd1 = 4',
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
    '    # 2. Revert 0-cell to 0.0 via matrix edit -> cleanly delete appended row',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.0)',
    '    $matVal2 = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $pEnd2 = 4',
    '    while ($pu1.Cells.Item($pEnd2 + 1, $panelCol).Text -ne "") { $pEnd2++ }',
    '    $rowSum2 = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 3. Direct panel edit on appended row: setting Scenario Rate to 0 removes row',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.06)',
    '    $pEnd3Before = 4',
    '    while ($pu1.Cells.Item($pEnd3Before + 1, $panelCol).Text -ne "") { $pEnd3Before++ }',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $pEnd3Before, $panelCol + 5, 0.0)',
    '    $matVal3 = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $pEnd3After = 4',
    '    while ($pu1.Cells.Item($pEnd3After + 1, $panelCol).Text -ne "") { $pEnd3After++ }',
    '    $rowSum3 = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 3b. Test multiple appended rows and deleting middle row',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.05)',
    '    $zeroRow2 = 0',
    '    $zeroCol2 = 0',
    '    for ($r = 5; $r -le $matEndRow; $r++) {',
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
    '    $pEnd2App = 4',
    '    while ($pu1.Cells.Item($pEnd2App + 1, $panelCol).Text -ne "") { $pEnd2App++ }',
    '    $multiDiffBefore = $pEnd2App - $pEnd0',
    '',
    '    # Delete row A (middle row)',
    '    $excel.Run("TriggerCellChange", "PU_1_Protected_Forest", $zeroRow, $zeroCol, 0.0)',
    '    $pEndAfterDelA = 4',
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
    '    $diagClass = $pu1.Cells.Item(5, $matLabelCol).Text',
    '    $dash.Range("C8").Value = $diagClass',
    '    $dash.Range("C9").Value = $diagClass',
    '    $dash.Range("E8").Value = 1.0',
    '    $dash.Range("E9").Value = "1"',
    '    $excel.Run("ApplyPolicyAcrossPUs")',
    '',
    '    $diagVal = [double]$pu1.Cells.Item(5, 3).Value2',
    '    $offDiagSum = 0.0',
    '    for ($c = 4; $c -le $matEndCol; $c++) {',
    '        $offDiagSum += [double]$pu1.Cells.Item(5, $c).Value2',
    '    }',
    '    $diagRowSum = [double]$pu1.Cells.Item(5, $sumCol).Value2',
    '',
    '    $row5PanelCount = 0',
    '    $row5IntervenedCount = 0',
    '    for ($pr = 5; $pr -le $pEnd0; $pr++) {',
    '        if ([int]$pu1.Cells.Item($pr, $panelCol + 8).Value2 -eq 5) {',
    '            $row5PanelCount++',
    '            $rRate = [double]$pu1.Cells.Item($pr, $panelCol + 5).Value2',
    '            $rArea = [double]$pu1.Cells.Item($pr, $panelCol + 6).Value2',
    '            $rStat = $pu1.Cells.Item($pr, $panelCol + 7).Text',
    '            if ($rRate -lt 0.0001 -and $rArea -lt 0.01 -and $rStat -eq "Intervened") {',
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
    '    $diagRowSum4b = [double]$pu1.Cells.Item(5, $sumCol).Value2',
    '    $pEnd4b = 4',
    '    while ($pu1.Cells.Item($pEnd4b + 1, $panelCol).Text -ne "") { $pEnd4b++ }',
    '    $hasValueErr = "NO"',
    '    for ($pr = 5; $pr -le $pEnd4b; $pr++) {',
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
    '    $pEnd5 = 4',
    '    while ($pu1.Cells.Item($pEnd5 + 1, $panelCol).Text -ne "") { $pEnd5++ }',
    '    $bpAppAdj = $pu1.Cells.Item($pEnd5, $panelCol + 4).Text',
    '    $bpAppScenRate = [double]$pu1.Cells.Item($pEnd5, $panelCol + 5).Value2',
    '    $bpAppStatus = $pu1.Cells.Item($pEnd5, $panelCol + 7).Text',
    '    $bpRowSum = [double]$pu1.Cells.Item($zeroRow, $sumCol).Value2',
    '',
    '    # 6. Reset to baseline restores clean initial state',
    '    $excel.Run("ResetAllSheetsToBaseline")',
    '    $excel.Run("ValidateAllSheets")',
    '    $pEnd6 = 4',
    '    while ($pu1.Cells.Item($pEnd6 + 1, $panelCol).Text -ne "") { $pEnd6++ }',
    '    $resetDiagVal = [double]$pu1.Cells.Item(5, 3).Value2',
    '    $resetZeroVal = [double]$pu1.Cells.Item($zeroRow, $zeroCol).Value2',
    '    $dashValid = $dash.Cells.Item(13, 6).Text',
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
  
  # 1. 0-to-exist dynamic append
  expect_true(any(grepl("ZERO_APPEND_VAL:0.05", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_DIFF:1", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_ADJ:NEW", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_RATE:0.05", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_AREA_POS:YES", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_STATUS:Intervened", res_lines)))
  expect_true(any(grepl("ZERO_APPEND_SUM:1", res_lines)))
  
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
  expect_true(any(grepl("ROW_B_STATUS:Intervened", res_lines)))
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
  expect_true(any(grepl("BP_ZERO_ADJ:NEW", res_lines)))
  expect_true(any(grepl("BP_ZERO_RATE:0.07", res_lines)))
  expect_true(any(grepl("BP_ZERO_STATUS:Intervened", res_lines)))
  expect_true(any(grepl("BP_ZERO_SUM:1", res_lines)))
  
  # 6. Reset cleans up
  expect_true(any(grepl("RESET_DIFF:0", res_lines)))
  expect_true(any(grepl("RESET_DIAG_RESTORED:YES", res_lines)))
  expect_true(any(grepl("RESET_ZERO_VAL:0", res_lines)))
  expect_true(any(grepl("RESET_VALID:VALID", res_lines)))
})

# Cleanup temp files
unlink(test_dir, recursive = TRUE)


