# QUES-H RUSLE Script

# 0. Load functions and libraries -----------------------------------------
tryCatch({

# Load custom functions
source("06_quesh/rscript/initial/quesh_functions.R")

required_packages <- c(
  "terra", "sf", "magrittr", "dplyr", "lattice", "rasterVis", "classInt", "ggplot2", "scales"
)

check_and_install_packages(required_packages)

# Start running QUES-H
start_time <- Sys.time()

# 1. Define input parameters ----------------------------------------------

# Define file path and parameters
path <- list(
  pu_file = "data/vector/unit_perencanaan_bungo.shp", # boundary map AOI
  dem_file = "data/data_quesh/SRTM_Bungo.tif", # raster file of DEM
  rainfall_file = "data/data_quesh/rainfall_annual_bungo_wc2.1.tif", # raster file of rainfall
  sand_file = "data/raster/soil/bungo_sand_0-5cm_mean.tif", # raster file
  silt_file = "data/raster/soil/bungo_silt_0-5cm_mean.tif", 
  clay_file = "data/raster/soil/bungo_clay_0-5cm_mean.tif", 
  orgc_file = "data/raster/soil/bungo_soc_0-5cm_mean.tif",
  lc_t1_file = "data/raster/tutupan_lahan_Bungo_2005r.tif", # landcover directory that consist of time series land cover map
  lc_t2_file = "data/raster/tutupan_lahan_Bungo_2010r.tif",
  c_ref_file = "data/data_quesh/c_factor_bungo_usda1972.csv", # csv file contained cover management factor for each landcover class
  multiseries = 1, # 1 mean include the multiple time series analysis
  practice = 0, # 1 mean practice factor included, 0 mean not included
  practice_file = "path", # raster file of p factor
  t1 = 2005,
  t2 = 2010,
  map_resolution = 100
)

output_dir = "06_quesh/output/"

# Prepare the planning unit
pu1 <- st_read(path$pu_file)
pu <- rasterise_multipolygon(sf_object = pu1, raster_res = c(path$map_resolution, path$map_resolution), field = paste0(colnames(st_drop_geometry(pu1[1]))))

# Prepare R factor input
rainfall <- syncGeom(input = path$rainfall_file, ref = pu)

# Prepare K factor input
sand <- syncGeom(input = path$sand_file, ref = pu)
silt <- syncGeom(input = path$silt_file, ref = pu)
clay <- syncGeom(input = path$clay_file, ref = pu)
orgc <- syncGeom(input = path$orgc_file, ref = pu)

# Prepare LS factor input
dem <- syncGeom(input = path$dem_file, ref = pu)

# Prepare C factor input
c_ref <- readr::read_csv(path$c_ref_file)

if (path$multiseries == 1){
  landcover_t1 <- rast(path$lc_t1_file)
  landcover_t2 <- rast(path$lc_t2_file)
  landcover_t1_viz <- lc_class_categorize(landcover = landcover_t1, c_ref = c_ref)
  landcover_t2_viz <- lc_class_categorize(landcover = landcover_t2, c_ref = c_ref)
} else {
  landcover_t1 <- rast(path$lc_t1_file)
  landcover_t1_viz <- lc_class_categorize(landcover = landcover_t1, c_ref = c_ref)
}

# Prepare P factor input parameters
if (path$practice == 1){
  p <- rast(path$practice_file)
  p_factor <- syncGeom(input = p, ref = pu)
} else {
  p_factor <- pu %>% classify(cbind(1:nrow(unique(pu)), 1))
}

# Running QuES-H RUSLE Analysis -------------------------------------------

a <- quesh_rusle_calc(rainfall = rainfall, 
                      sand = sand, 
                      silt = silt, 
                      clay = clay, 
                      orgc = orgc, 
                      dem = dem, 
                      landcover_t1 = landcover_t1,
                      landcover_t2 = landcover_t2,
                      c_ref = c_ref, 
                      p_factor = p_factor,
                      multiseries = path$multiseries)

# Reclassify erosion rates based on China National Standard (2008)
breaks <- c(-Inf, 5, 25, 50, 80, 150, Inf)
labels <- c("Slight (< 5 ton/ha/yr)", 
            "Mild (5-25 ton/ha/yr)", 
            "Moderate (25-50 ton/ha/yr)", 
            "Strong (50-80 ton/ha/yr)", 
            "Very strong (80-150 ton/ha/yr)", 
            "Severe (> 150 ton/ha/yr)")
rcl_matrix <- cbind(breaks[-length(breaks)], breaks[-1], 1:(length(breaks)-1))

# Post Analysis Work
if (path$multiseries == 1){
  
  # Redefined the results
  erosion_t1 <- a[[1]]
  erosion_t2 <- a[[2]]
  r_factor <- a[[3]]
  k_factor <- a[[4]]
  ls_factor <- a[[5]]
  c_factor_t1 <- a[[6]]
  c_factor_t2 <- a[[7]]
  
  # Reclassify erosion rate
  erosion_classified_t1 <- classify(erosion_t1, rcl = rcl_matrix)
  erosion_classified_t2 <- classify(erosion_t2, rcl = rcl_matrix)
  levels(erosion_classified_t1) <- data.frame(id=1:6, category=labels)
  levels(erosion_classified_t2) <- data.frame(id=1:6, category=labels)
  
  # Create dataset of erosion estimation
  erosion_db_t1 <- erosion_dataset(erosion_classified = erosion_classified_t1)
  erosion_db_t2 <- erosion_dataset(erosion_classified = erosion_classified_t2)
  
  erosion_db_t2 <- erosion_db_t2 %>%
    rename(
      `Area (Ha) T2` = `Area (Ha)`,
      `Percentage (%) T2` = `Percentage (%)`
    )
  
  erosion_db <- erosion_db_t1 %>%
    rename(
      `Area (Ha) T1` = `Area (Ha)`,
      `Percentage (%) T1` = `Percentage (%)`
    ) %>%
    inner_join(erosion_db_t2, by = "Soil Erosion Rates")
  
  # Calculate erosion difference between two series of time
  e_diff <- erosion_classified_t2 - erosion_classified_t1
  e_rcl_matrix <- matrix(c(-Inf, -0.0001, 1,  # Class 1: Erosion risk decrease
                           -0.0001, 0.0001, 2,  # Class 2: No erosion risk changes
                           0.0001, Inf, 3),   # Class 3: Erosion risk increase
                         ncol = 3, byrow = TRUE)
  e_diff_classified <- classify(e_diff, rcl = e_rcl_matrix)
  e_labels <- c("Erosion risk decrease", "No erosion risk changes", "Erosion risk increase")
  levels(e_diff_classified) <- data.frame(id=1:3, category=e_labels)
  
} else {
  
  # Redefined the results
  erosion_t1 <- a[[1]]
  r_factor <- a[[2]]
  k_factor <- a[[3]]
  ls_factor <- a[[4]]
  c_factor <- a[[5]]
  
  # Reclassify erosion rate
  erosion_classified_t1 <- classify(erosion_t1, rcl = rcl_matrix)
  levels(erosion_classified_t1) <- data.frame(id=1:6, category=labels)
  
  # Create dataset of erosion estimation
  erosion_db_t1 <- erosion_dataset(erosion_classified = erosion_classified_t1)
}

# Export Results -----------------------------------------------------------

writeRaster(r_factor, paste0(output_dir, "r_factor.tif"), overwrite = TRUE)
writeRaster(k_factor, paste0(output_dir, "k_factor.tif"), overwrite = TRUE)
writeRaster(ls_factor, paste0(output_dir, "ls_factor.tif"), overwrite = TRUE)
writeRaster(c_factor_t1, paste0(output_dir, "c_factor", paste0(path$t1), ".tif"), overwrite = TRUE)
writeRaster(c_factor_t2, paste0(output_dir, "c_factor", paste0(path$t2), ".tif"), overwrite = TRUE)
writeRaster(erosion_t1, filename = paste0(output_dir, "soil_erosion", paste0(path$t1), ".tif"), overwrite = TRUE)
writeRaster(erosion_t2, filename = paste0(output_dir, "soil_erosion", paste0(path$t2), ".tif"), overwrite = TRUE)
writeRaster(erosion_classified_t1, filename = paste0(output_dir, "soil_erosion_reclass", paste0(path$t1), ".tif"), overwrite = TRUE)
writeRaster(erosion_classified_t2, filename = paste0(output_dir, "soil_erosion_reclass", paste0(path$t2), ".tif"), overwrite = TRUE)
write.csv(erosion_db_t1, file = paste0(output_dir, "soil_erosion", paste0(path$t1), ".csv"))
write.csv(erosion_db_t2, file = paste0(output_dir, "soil_erosion", paste0(path$t2), ".csv"))

# End of the script
end_time <- Sys.time()
cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")


# 9. Prepare parameters for report -------------------------------------

soil_stack <- c(sand, silt, clay, orgc)

report_params <- list(
  start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
  end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
  output_dir = output_dir,
  dem = dem,
  pu = pu,
  rainfall = rainfall,
  soil = soil_stack,
  landcover = landcover,
  r = r_factor,
  k = k_factor,
  ls = ls_factor,
  c = c_factor,
  p = p_factor,
  a = erosion_classified,
  df = erosion_db
)

# Render the R markdown report
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
}

rmarkdown::render(
  input = "06_quesh/report_template/quesh_report.Rmd",
  output_file = "QUES-H_report.html",
  output_dir = output_dir,
  params = report_params
)

}, error = function(e) {
  cat("An error occurred:\n")
  print(e)
}, finally = {
  cat("Script execution completed.\n")
})

# P - Practice factor preparation --------------------------------------

# slope_deg <- terrain(dem, v = "slope", unit="degree")  
# slope_pct <- tan(slope_deg * pi / 180) * 100
# 
# # There are 3 option for practice management according to Shin (1999)
# # You can choose the applied practices: Contouring; Strip Cropping; Terracing
# # change the parameter by the following order p_user <- c([contouring], [strip cropping], [terracing])
# # The value of 1 means the corresponding practice applied and 0 means not applied
# # If the value all 0, it means the P factor will be define as 1 (no practice applied)
# 
# p_user <- c(1, 0, 0) # change the value with 0 or 1 by this order: c([contouring], [strip cropping], [terracing])
# 
# p_factor <- calculate_p_shin(
#   slope_pct = slope_pct, 
#   p_user = p_user
# )
# 
# writeRaster(p_factor, paste0(output_dir, "p_factor.tif"), overwrite = TRUE)
