# QuES-B module
# This script performs TECI (Total Edge Contrast Index) calculation and
# calculates DIFA Index

# Load required packages


# Source custom functions
source("05_quesb/rscript/ques_biodiv_functions.r")
check_and_install_packages(c("dplyr", "ggplot2", "caTools", "terra", "sf", "DBI", "RSQLite"))

# Input data and Parameters
lc_t1_path <- "data/raster/tutupan_lahan_Bungo_2010r.tif"
t1 <- 2010
nodata_class <- 0
lulc_lut_path <- "data/table/Tabel_focal_area_Bungo.csv"
contab_path <- "data/table/Tabel_edge_contrast_euc.fsq"
sampling_points <- 1000
window_size <- 1000
window.shape <- 0
fca_path <- NULL
fragstats_path <- NULL
output_dir <- "output/quesb_2010"

# Create output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Run ques-b for lc 1990
start_time <- Sys.time()
cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

quesb_result <- quesb_single_period(
  lc_t1_path = lc_t1_path,
  t1 = t1,
  raster.nodata = nodata_class,
  lulc_lut_path = lulc_lut_path,
  contab_path = contab_path,
  output_dir = output_dir,
  sampling_points = sampling_points,
  window_size = window_size,
  window.shape = window.shape,
  fca_path = fca_path,
  fragstats_path = fragstats_path
)

# End of the script
end_time <- Sys.time()
cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
session_log <- format_session_info_table()


report_params <- list(
  start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
  end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
  output_dir = output_dir,
  total_area = quesb_result$total_area,
  dir_teci_map = basename(quesb_result$path_teci_map),
  dir_focal_area_ = basename(quesb_result$path_focal_area),
  dir_sampling_grid = basename(quesb_result$path_sampling_grid),
  dir_difa_table = basename(quesb_result$path_difa_table),
  difa_table = quesb_result$difa_table,
  difa_score = quesb_result$difa_score,
  inputs = list(
    lc_t1_path = lc_t1_path,
    t1 = t1,
    raster.nodata = nodata_class,
    lulc_lut_path = lulc_lut_path,
    contab_path = contab_path,
    output_dir = output_dir,
    sampling_points = sampling_points,
    window_size = window_size,
    window.shape = window.shape
  ),
  session_log = session_log
)

# Prepare parameters for report rendering
report_params$inputs$fca_path <- if(is.null(fca_path)) {"05_quesb/rscript/teciuf.fca"} else {fca_path}
if (is.null(report_params$inputs$fragstats_path)) {
  program_files <- c("C:/Program Files/", "C:/Program Files (x86)/")
  fragstats_dirs <- list.files(program_files, pattern = "^Fragstats 4", full.names = TRUE)
  if (length(fragstats_dirs) == 0) {
    stop("No Fragstats 4.x installation found.")
  }
  # Sort directories to use the latest version if multiple are found
  report_params$inputs$fragstats_path <- sort(fragstats_dirs, decreasing = TRUE)[1]
}

# Render the R Markdown report
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
}

rmarkdown::render(
  input = "05_quesb/report_template/quesb_report_template.Rmd",
  output_file = "QuES_B_report.html",
  output_dir = output_dir,
  params = report_params
)
