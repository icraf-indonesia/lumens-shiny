# QuES-B module
# This script performs TECI (Total Edge Contrast Index) calculation and
# calculates DIFA Index

# Source helper functions
source("05_quesb/rscript/ques_biodiv_functions.r")
check_and_install_packages(c("dplyr", "ggplot2", "caTools", "terra", "sf", "DBI", "RSQLite"))

run_ques_b(
  lc_t1_path = "data/raster/tutupan_lahan_Bungo_2010r.tif",
  t1 = 2010,
  nodata_class = 0,
  lulc_lut_path = "data/table/Tabel_focal_area_Bungo.csv",
  contab_path = "data/table/Tabel_edge_contrast_euc.fsq",
  sampling_points = 1000,
  window_size = 1000,
  window.shape = 1,
  fca_path = NULL,
  fragstats_path = NULL,
  output_dir = "output/quesb_triggered_from_function",
  report_template_path = "05_quesb/report_template/quesb_report_template.Rmd"
)
