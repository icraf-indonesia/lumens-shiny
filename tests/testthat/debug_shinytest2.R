#!/usr/bin/env Rscript
# Debug script for shinytest2

library(shinytest2)

fixture_dir <- normalizePath("tests/testthat/fixtures")
raster_dir <- file.path(fixture_dir, "rasters")

df <- readr::read_csv("tests/testthat/fixtures/raster_inputs.csv", show_col_types = FALSE)
df$raster_path <- file.path(raster_dir, basename(df$raster_path))
abs_raster_csv <- tempfile(fileext = ".csv")
readr::write_csv(df, abs_raster_csv)

out_dir <- "tests/testthat/output/shinytest2_lasem"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(out_dir, full.names = TRUE), recursive = TRUE)

message("Starting app in test mode...")
app <- AppDriver$new(
  app_dir = "12_lasem/rscript",
  name = "debug",
  variant = NULL,
  timeout = 60000,
  options = list(shiny.testmode = TRUE)
)

message("Uploading files...")
app$upload_file(raster_inputs_csv = abs_raster_csv)
app$upload_file(crop_suitability_csv = "tests/testthat/fixtures/crop_suitability.csv")
app$upload_file(intervention_csv = "tests/testthat/fixtures/intervention.csv")
app$wait_for_idle(duration = 500)

# Check output directory display
out_display <- app$get_html("#print_output_dir")
message("Output dir display: ", out_display)

message("Clicking Run Analysis...")
app$click("run_analysis")

# Wait and poll
message("Polling for output files...")
for (i in 1:30) {
  Sys.sleep(2)
  files <- list.files(out_dir)
  if (length(files) > 0) {
    message("Files found after ", i*2, " seconds: ", paste(files, collapse = ", "))
  }
  if ("LaSEM_report.html" %in% files) {
    message("Report generated!")
    break
  }
}

# Check error messages
err_html <- app$get_html("#error_messages")
message("Error HTML length: ", nchar(err_html))
if (nchar(err_html) > 100) {
  message("Error content: ", substr(err_html, 1, 500))
}

# Check status
status_html <- app$get_html("#status_messages")
message("Status HTML: ", substr(status_html, 1, 200))

app$stop()
message("Done.")
