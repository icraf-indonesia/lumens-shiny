# Define required packages
required_packages <- c(
  "terra", "raster",
  "splitstackshape", "ggplot2",
  "foreign", "reshape2",
  "dplyr", "reshape",
  "purrr", "plotly",
  "sf", "remotes",
  "rmarkdown", "bslib"      
)

lapply(required_packages, require, character.only = TRUE)

setup_test_data <- function() {
  list(
    lc_t1_path = list(
      datapath = "data/raster/bungo_landcover_1990r.tif",
      name = "bungo_landcover_1990r"
    ),
    lc_t2_path = list(
      datapath = "data/raster/bungo_landcover_2000r.tif",
      name = "bungo_landcover_2000r"
    ),
    admin_z_path = list(
      datapath = "data/vector/planning_unit_bungo.shp",
      name = "plannung_unit_bungo"
    ),
    c_lookup_path = list(
      datapath = "data/table/carbon_bungo.csv"
    )
  )
}


setup_output_dir <- function() {
  test_output <- "04_quesc/output/quesc_1990_2000"
  dir.create(test_output, recursive = TRUE, showWarnings = FALSE)
  return(test_output)
}

