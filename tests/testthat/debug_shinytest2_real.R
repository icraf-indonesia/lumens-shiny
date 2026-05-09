#!/usr/bin/env Rscript
# Debug script for shinytest2 with real Bone data

library(shinytest2)

bone_raster_csv <- "data/LaSEM/sample_datasets/bone_regency/lookup_tables/crop_suitability_spatial_input.csv"
bone_crop_csv <- "data/LaSEM/crop_parameters/kesesuaian_jagung.csv"
bone_intervention_csv <- "data/LaSEM/lookup_tables/lookup_intervention.csv"

stopifnot(file.exists(bone_raster_csv))
stopifnot(file.exists(bone_crop_csv))
stopifnot(file.exists(bone_intervention_csv))

out_dir <- "tests/testthat/output/shinytest2_lasem"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(out_dir, full.names = TRUE), recursive = TRUE)

message("Starting app in test mode...")
app <- AppDriver$new(
  app_dir = "12_lasem/rscript",
  name = "debug_real",
  variant = NULL,
  timeout = 120000,
  options = list(shiny.testmode = TRUE)
)

message("Uploading real Bone dataset...")
app$upload_file(raster_inputs_csv = bone_raster_csv)
app$upload_file(crop_suitability_csv = bone_crop_csv)
app$upload_file(intervention_csv = bone_intervention_csv)
app$wait_for_idle(duration = 500)

# Check output directory display
out_display <- app$get_html("#print_output_dir")
message("Output dir display: ", out_display)

message("Clicking Run Analysis...")
app$click("run_analysis")

# Wait and poll
message("Polling for output files...")
for (i in 1:40) {
  Sys.sleep(3)
  files <- list.files(out_dir)
  if (length(files) > 0) {
    message("Files found after ", i*3, " seconds: ", paste(files, collapse = ", "))
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
