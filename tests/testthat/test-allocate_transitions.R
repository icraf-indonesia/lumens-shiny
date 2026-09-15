# tests/testthat/test-allocate_transitions.R

library(testthat)

# Source simulate functions
source("../../11_sciendo-simulate/rscript/function_sciendo_simulate.R")

test_that("make_alloc_trans_template creates a valid template structure", {
  lc_table <- data.frame(
    ID = 1:3,
    LC = c("Forest", "Agriculture", "Settlement"),
    stringsAsFactors = FALSE
  )
  
  tmpl <- make_alloc_trans_template(lc_table)
  
  expect_equal(nrow(tmpl), 6)
  expected_cols <- c(
    "from_id", "from_name", "to_id", "to_name",
    "percent", "exp_mean", "exp_var", "exp_iso",
    "gen_mean", "gen_var", "gen_iso"
  )
  expect_true(all(expected_cols %in% names(tmpl)))
  expect_true(all(tmpl$from_id != tmpl$to_id))
  expect_true(all(is.na(tmpl$percent)))
  expect_true(all(is.na(tmpl$exp_mean)))
})

test_that("build_allocate_transitions returns valid default port strings", {
  lc_table <- data.frame(
    ID = 1:3,
    LC = c("Forest", "Agriculture", "Settlement"),
    stringsAsFactors = FALSE
  )
  
  res <- build_allocate_transitions(lc_table)
  
  expect_type(res, "list")
  expect_named(res, c("percentOfTransitionsByExpansion", "patchExpansionParameters", "patchGenerationParameters"))
  expect_true(grepl("^\\[&#x0A;", res$percentOfTransitionsByExpansion))
  expect_true(grepl("\\]$", res$percentOfTransitionsByExpansion))
  expect_true(grepl("0\\.5", res$percentOfTransitionsByExpansion))
  expect_true(grepl("2 1 1", res$patchExpansionParameters))
  expect_true(grepl("1 1 1", res$patchGenerationParameters))
})

test_that("build_allocate_transitions handles custom uniform values correctly", {
  lc_table <- data.frame(
    ID = 1:2,
    LC = c("Forest", "Non-Forest"),
    stringsAsFactors = FALSE
  )
  
  # Setting A: Patcher preference
  res_a <- build_allocate_transitions(
    lc_table,
    percent  = 0.05,
    exp_mean = 1, exp_var = 1, exp_iso = 1,
    gen_mean = 1, gen_var = 1, gen_iso = 1
  )
  expect_true(grepl("0\\.05", res_a$percentOfTransitionsByExpansion))
  expect_true(grepl("1 1 1", res_a$patchExpansionParameters))
  expect_true(grepl("1 1 1", res_a$patchGenerationParameters))
  
  # Setting B: Expander preference
  res_b <- build_allocate_transitions(
    lc_table,
    percent  = 1,
    exp_mean = 50, exp_var = 50, exp_iso = 2,
    gen_mean = 50, gen_var = 50, gen_iso = 2
  )
  expect_true(grepl("1->2 1", res_b$percentOfTransitionsByExpansion))
  expect_true(grepl("50 50 2", res_b$patchExpansionParameters))
  expect_true(grepl("50 50 2", res_b$patchGenerationParameters))
})

test_that("build_allocate_transitions validates input ranges strictly", {
  lc_table <- data.frame(ID = 1:2, LC = c("A", "B"))
  
  expect_error(build_allocate_transitions(lc_table, percent = -0.1), "percent must be in \\[0,1\\]")
  expect_error(build_allocate_transitions(lc_table, percent = 1.1), "percent must be in \\[0,1\\]")
  
  expect_error(build_allocate_transitions(lc_table, exp_mean = 0), "exp_mean must be > 0")
  expect_error(build_allocate_transitions(lc_table, gen_mean = -5), "gen_mean must be > 0")
  
  expect_error(build_allocate_transitions(lc_table, exp_var = -1), "exp_var must be >= 0")
  expect_error(build_allocate_transitions(lc_table, gen_var = -0.5), "gen_var must be >= 0")
  
  expect_error(build_allocate_transitions(lc_table, exp_iso = -0.1), "exp_iso must be in \\[0,2\\]")
  expect_error(build_allocate_transitions(lc_table, gen_iso = 2.5), "gen_iso must be in \\[0,2\\]")
})

test_that("build_allocate_transitions merges override_df correctly", {
  lc_table <- data.frame(ID = 1:3, LC = c("A", "B", "C"))
  
  override <- data.frame(
    from_id  = 1,
    to_id    = 2,
    percent  = 0.9,
    exp_mean = 10,
    exp_var  = 2,
    exp_iso  = 2,
    gen_mean = 5,
    gen_var  = 1,
    gen_iso  = 1
  )
  
  res <- build_allocate_transitions(lc_table, percent = 0.2, override_df = override)
  
  expect_true(grepl("1->2 0\\.9", res$percentOfTransitionsByExpansion))
  expect_true(grepl("1->3 0\\.2", res$percentOfTransitionsByExpansion))
  expect_true(grepl("1->2 10 2 2", res$patchExpansionParameters))
})

test_that("build_allocate_transitions errors on invalid override_df", {
  lc_table <- data.frame(ID = 1:2, LC = c("A", "B"))
  
  bad_override <- data.frame(from_id = 1, to_id = 2, percent = 0.5)
  expect_error(build_allocate_transitions(lc_table, override_df = bad_override), "missing columns")
  
  dup_override <- data.frame(
    from_id = c(1, 1), to_id = c(2, 2),
    percent = c(0.5, 0.6), exp_mean = c(1, 1), exp_var = c(1, 1), exp_iso = c(1, 1),
    gen_mean = c(1, 1), gen_var = c(1, 1), gen_iso = c(1, 1)
  )
  expect_error(build_allocate_transitions(lc_table, override_df = dup_override), "duplicate transitions")
})
