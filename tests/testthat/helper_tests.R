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

setup_baliase_test_data <- function() {
  # Define file paths for shapefile components
  shp_base_path <- "data/BALIASE/LOKASI_IZIN_PS_BALIASE"
  admin_paths <- c(
    paste0(shp_base_path, ".shp"),
    paste0(shp_base_path, ".shx"),
    paste0(shp_base_path, ".sbx"),
    paste0(shp_base_path, ".sbn"),
    paste0(shp_base_path, ".dbf"),
    paste0(shp_base_path, ".cpg"),
    paste0(shp_base_path, ".prj")
  )
  
  # Read and process the shapefile to create the zone raster
  sf_object <- sf::st_read(admin_paths[1]) %>%
    dplyr::transmute(planning_unit = paste(SKEMA, LEMBAGA)) %>%
    dplyr::mutate(ID = dplyr::row_number()) %>%
    dplyr::select(ID, planning_unit) %>%
    dplyr::rename(Value = 1, planning_unit = 2)
  
  lc_t1_raster <- terra::rast("data/BALIASE/PL2018re.tif")
  lc_t1_res <- terra::res(lc_t1_raster)
  zone_raster <- rasterise_multipolygon(sf_object, raster_res = lc_t1_res, field = "Value")
  zone_raster <- terra::resample(zone_raster, lc_t1_raster, method = "near")
  
  list(
    lc_t1_input = list(
      datapath = "data/BALIASE/PL2018re.tif",
      name = "PL2018re"
    ),
    lc_t2_input = list(
      datapath = "data/BALIASE/PL2023re.tif",
      name = "PL2023re"
    ),
    admin_z_input = zone_raster, # Use the pre-processed raster
    trajectory_lookup_input = list(
      datapath = "../../data/table/trajectory_loss_recovery.csv"
    ),
    lc_lookup_input = list(
      datapath = "data/BALIASE/Tuplah_Baliase.csv"
    ),
    zone_lookup_input = NULL # Not used in this test case
  )
}





setup_output_dir <- function() {
  test_output <- "../../output/ques_pre_1990_2000"
  dir.create(test_output, recursive = TRUE, showWarnings = FALSE)
  return(test_output)
}

setup_output_dir_baliase <- function() {
  test_output <- "../../output/ques_pre_baliase"
  dir.create(test_output, recursive = TRUE, showWarnings = FALSE)
  return(test_output)
}
