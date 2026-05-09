library(shinytest2)

# Paths to real Bone dataset for integration testing
bone_raster_csv <- test_path("../../data/LaSEM/sample_datasets/bone_regency/lookup_tables/crop_suitability_spatial_input.csv")
bone_crop_csv <- test_path("../../data/LaSEM/crop_parameters/kesesuaian_jagung.csv")
bone_intervention_csv <- test_path("../../data/LaSEM/lookup_tables/lookup_intervention.csv")

test_that("LaSEM app loads and validates inputs with real data", {
  skip_if_not(
    file.exists("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
    "Chrome not available"
  )

  app_dir <- test_path("../../12_lasem/rscript")
  skip_if_not(dir.exists(app_dir), "App directory not found")

  skip_if_not(file.exists(bone_raster_csv), "Bone raster inputs not found")
  skip_if_not(file.exists(bone_crop_csv), "Jagung crop parameters not found")
  skip_if_not(file.exists(bone_intervention_csv), "Intervention lookup not found")

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "lasem_basic",
    variant = NULL,
    timeout = 20000
  )

  # Upload real dataset CSVs one at a time
  app$upload_file(raster_inputs_csv = bone_raster_csv)
  app$upload_file(crop_suitability_csv = bone_crop_csv)
  app$upload_file(intervention_csv = bone_intervention_csv)

  # Wait for validation to update
  app$wait_for_idle(duration = 500)

  # Check validation panel shows success
  validation_html <- app$get_html("#validation_panel")
  expect_match(validation_html, "All inputs valid", fixed = TRUE)

  # Check CSV previews rendered
  expect_true(app$get_html("#preview_raster_inputs") != "")
  expect_true(app$get_html("#preview_crop_suitability") != "")
  expect_true(app$get_html("#preview_intervention") != "")

  app$stop()
})

test_that("LaSEM app runs analysis end-to-end with real Bone data", {
  skip_if_not(
    file.exists("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
    "Chrome not available"
  )

  app_dir <- test_path("../../12_lasem/rscript")
  skip_if_not(dir.exists(app_dir), "App directory not found")

  skip_if_not(file.exists(bone_raster_csv), "Bone raster inputs not found")
  skip_if_not(file.exists(bone_crop_csv), "Jagung crop parameters not found")
  skip_if_not(file.exists(bone_intervention_csv), "Intervention lookup not found")

  # Ensure output directory exists for test mode
  out_dir <- test_path("output/shinytest2_lasem")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # Clean any previous test outputs
  unlink(list.files(out_dir, full.names = TRUE), recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "lasem_analysis",
    variant = NULL,
    timeout = 120000,
    options = list(shiny.testmode = TRUE)
  )

  # Upload real dataset CSVs one at a time
  app$upload_file(raster_inputs_csv = bone_raster_csv)
  app$upload_file(crop_suitability_csv = bone_crop_csv)
  app$upload_file(intervention_csv = bone_intervention_csv)

  # Wait for validation to pass
  app$wait_for_idle(duration = 500)

  # Verify we're in test mode by checking output directory display
  out_dir_html <- app$get_html("#print_output_dir")
  message("Output dir display: ", out_dir_html)

  # Click Run Analysis
  app$click("run_analysis")

  # Wait for analysis to complete by polling output files
  max_wait <- 120
  poll_interval <- 3
  waited <- 0
  report_exists <- FALSE

  while (waited < max_wait) {
    Sys.sleep(poll_interval)
    waited <- waited + poll_interval

    # Check if report was generated (last step of analysis)
    if (file.exists(file.path(out_dir, "LaSEM_report.html"))) {
      report_exists <- TRUE
      break
    }
  }

  # Verify report was created
  expect_true(
    file.exists(file.path(out_dir, "LaSEM_report.html")),
    info = paste(
      "Report HTML was not generated within timeout period.",
      "Files found:", paste(list.files(out_dir), collapse = ", ")
    )
  )

  # Check other output files
  expect_true(file.exists(file.path(out_dir, "land_suitability.tif")))
  expect_true(file.exists(file.path(out_dir, "land_suitability.rds")))
  expect_true(file.exists(file.path(out_dir, "suitability_factors.tif")))
  expect_true(file.exists(file.path(out_dir, "soil_climate_factors.rds")))
  expect_true(file.exists(file.path(out_dir, "suitability_lookup.csv")))

  # Give UI a moment to update after analysis completes
  Sys.sleep(1)
  app$wait_for_idle(duration = 500)

  # Switch to Results tab and verify map
  app$set_inputs(main_tabs = "results_tab")
  app$wait_for_idle(duration = 500)

  # Verify results map exists (Leaflet creates a div with id 'results_map')
  map_html <- app$get_html("#results_map")
  expect_true(nchar(map_html) > 200, info = "Map canvas should be rendered")
  # Leaflet maps contain specific classes
  expect_match(map_html, "leaflet", fixed = TRUE, info = "Map should be a Leaflet instance")

  # Verify area summary table exists and has content
  area_html <- app$get_html("#area_summary")
  expect_true(nchar(area_html) > 100, info = "Area summary table should be rendered")

  app$stop()
})

test_that("LaSEM report content is correct with real data", {
  skip_if_not(
    file.exists("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
    "Chrome not available"
  )

  app_dir <- test_path("../../12_lasem/rscript")
  skip_if_not(dir.exists(app_dir), "App directory not found")

  skip_if_not(file.exists(bone_raster_csv), "Bone raster inputs not found")
  skip_if_not(file.exists(bone_crop_csv), "Jagung crop parameters not found")
  skip_if_not(file.exists(bone_intervention_csv), "Intervention lookup not found")

  out_dir <- test_path("output/shinytest2_lasem")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(out_dir, full.names = TRUE), recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "lasem_report",
    variant = NULL,
    timeout = 120000,
    options = list(shiny.testmode = TRUE)
  )

  # Upload real dataset
  app$upload_file(raster_inputs_csv = bone_raster_csv)
  app$upload_file(crop_suitability_csv = bone_crop_csv)
  app$upload_file(intervention_csv = bone_intervention_csv)
  app$wait_for_idle(duration = 500)

  # Run analysis
  app$click("run_analysis")

  # Poll for report generation
  max_wait <- 120
  poll_interval <- 3
  waited <- 0
  while (waited < max_wait) {
    Sys.sleep(poll_interval)
    waited <- waited + poll_interval
    if (file.exists(file.path(out_dir, "LaSEM_report.html"))) break
  }

  skip_if_not(
    file.exists(file.path(out_dir, "LaSEM_report.html")),
    "Report was not generated - skipping content evaluation"
  )

  # Read and evaluate report content
  report_html <- readLines(file.path(out_dir, "LaSEM_report.html"))
  report_text <- paste(report_html, collapse = " ")
  # rmarkdown/pandoc may wrap lines, splitting phrases across newlines;
  # collapse all whitespace so we can match contiguous phrases
  report_text <- gsub("[[:space:]]+", " ", report_text)

  # Check report has expected sections
  expect_match(report_text, "Land Suitability Evaluation Module", fixed = TRUE,
               info = "Report should have LaSEM title")
  expect_match(report_text, "Analysis Log", fixed = TRUE,
               info = "Report should have Analysis Log section")
  expect_match(report_text, "Crop Environmental Requirements", fixed = TRUE,
               info = "Report should have crop requirements section")
  expect_match(report_text, "Suitability Map", fixed = TRUE,
               info = "Report should have suitability map section")
  expect_match(report_text, "Interactive Suitability Map", fixed = TRUE,
               info = "Report should have interactive map section")
  expect_match(report_text, "Data Quality", fixed = TRUE,
               info = "Report should have data quality section")

  # Check report includes validation results
  expect_match(report_text, "Validation", fixed = TRUE,
               info = "Report should mention validation")

  # Check report has methodology section
  expect_match(report_text, "Methodology", fixed = TRUE,
               info = "Report should have methodology section")

  # Verify report has the expected crop name from real data
  expect_match(report_text, "jagung", fixed = TRUE,
               info = "Report should show jagung crop name from real data")

  app$stop()
})
