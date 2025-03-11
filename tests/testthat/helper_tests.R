# Define required packages
required_packages <- c(
  "terra", "dplyr", "ggplot2", "sf",  # Core spatial/data analysis
  "rmarkdown", "bslib",               # Reporting
  "readr", "tidyr", "networkD3",      # Data wrangling & visualization
  "stringr", "scales", "purrr"        
)

lapply(required_packages, require, character.only = TRUE)

setup_test_data <- function() {
  list(
    lc_t1_input = list(
      datapath = "../../data/raster/bungo_landcover_1990r.tif",
      name = "bungo_landcover_1990r"
    ),
    lc_t2_input = list(
      datapath = "../../data/raster/bungo_landcover_2000r.tif",
      name = "bungo_landcover_2000r"
    ),
    admin_z_input = list(
      datapath = "../../data/raster/bungo_zone.tif",
      name = "bungo_zone"
    ),
    zone_lookup_input = list(
      datapath = "../../data/table/zone_table_bungo.csv"
    ),
    trajectory_lookup_input = list(
      datapath = "../../data/table/trajectory_loss_recovery.csv"
    ),
    lc_lookup_input = list(
      datapath = "../../data/table/landuse_table_bungo.csv"
    )
  )
}




setup_output_dir <- function() {
  test_output <- "../../output/ques_pre_1990_2000"
  dir.create(test_output, recursive = TRUE, showWarnings = FALSE)
  return(test_output)
}

