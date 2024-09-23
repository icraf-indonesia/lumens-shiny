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
    lc_dir = "data/raster/tutupan_lahan_Bungo_2010r.tif", # landcover directory that consist of time series land cover map
    c_ref_file = "data/data_quesh/c_factor_bungo_usda1972.csv", # csv file contained cover management factor for each landcover class
    map_resolution = 100
  )
  
  output_dir = "06_quesh/output/"
  
  # Prepare the planning unit
  pu1 <- st_read(path$pu_file)
  pu <- rasterise_multipolygon(
    sf_object = pu1, 
    raster_res = c(path$map_resolution, path$map_resolution), 
    field = paste0(colnames(st_drop_geometry(pu1[1])))
  )
  
  # 2. R - Rainfall erosivity preparation -----------------------------------------------------
  
  # Define R factor data
  rainfall_annual <- syncGeom(input = path$rainfall_file, ref = pu)
  
  # Calculate R factor - Rainfall Erosivity (Moore, 1979)
  r_factor <- calculate_r_moore(p = rainfall_annual)
  
  writeRaster(r_factor, paste0(output_dir, "r_factor.tif"), overwrite = TRUE)
  
  # 3. K - Soil erodibility preparation -------------------------------------
  
  # Define K factor data
  sand <- syncGeom(input = path$sand_file, ref = pu)
  silt <- syncGeom(input = path$silt_file, ref = pu)
  clay <- syncGeom(input = path$clay_file, ref = pu)
  orgc <- syncGeom(input = path$orgc_file, ref = pu)
  
  soil_stack <- c(sand, silt, clay, orgc)
  
  # Calculate K factor - Soil Erodibility (Williams, 1995)
  k_factor <- calculate_k_williams(
    sndprc = sand, 
    sltprc = silt, 
    clyprc = clay, 
    orcprc = orgc
  )
  
  writeRaster(k_factor, paste0(output_dir, "k_factor.tif"), overwrite = TRUE)
  
  # 4. LS - Length and steepnes preparation --------------------------------
  
  # Define LS factor data
  dem <- syncGeom(input = path$dem_file, ref = pu)
  slope_deg <- terrain(dem, v = "slope", unit="degree")  
  
  # Calculate LS factor by Moore & Burch (1986) - BRIN 
  ls_factor <- calculate_ls_moore(dem = dem)
  
  writeRaster(ls_factor, paste0(output_dir, "ls_factor.tif"), overwrite = TRUE)
  
  # 5. C - Cover management preparation -------------------------------------
  
  # Define C factor data
  c_ref <- readr::read_csv(path$c_ref_file)
  landcover <- rast(path$lc_dir)
  landcover_c <- landcover
  lookup_lc <-landcover_c %>% freq() %>%
    select(ID=value) %>%
    left_join(c_ref, by="ID") %>% select(-LC)
  
  levels(landcover_c)[[1]] <- lookup_lc
  
  c_factor <- landcover_c %>% as.numeric(1) %>% resample(pu, method="near")
  
  writeRaster(c_factor, paste0(output_dir, "c_factor.tif"), overwrite = TRUE)
  
  
  # 6. P - Practice factor preparation --------------------------------------
  
  slope_pct <- tan(slope_deg * pi / 180) * 100
  
  # There are 3 option for practice management according to Shin (1999)
  # You can choose the applied practices: Contouring; Strip Cropping; Terracing
  # change the parameter by the following order p_user <- c([contouring], [strip cropping], [terracing])
  # The value of 1 means the corresponding practice applied and 0 means not applied
  # If the value all 0, it means the P factor will be define as 1 (no practice applied)
  
  p_user <- c(1, 0, 0) # change the value with 0 or 1 by this order: c([contouring], [strip cropping], [terracing])
  
  p_factor <- calculate_p_shin(
    slope_pct = slope_pct, 
    p_user = p_user
  )
  
  writeRaster(p_factor, paste0(output_dir, "p_factor.tif"), overwrite = TRUE)
  
  # 7. Calculate soil erosion RUSLE ----------------------------------------------------
  
  # Redefine input parameters
  
  r <- r_factor
  k <- k_factor
  ls <- ls_factor
  c <- c_factor
  p <- p_factor
  
  # Calculate RUSLE
  a <- r*k*ls*c
  
  writeRaster(a, filename = paste0(output_dir, "soil_erosion.tif"), overwrite = TRUE)
  
  # 8. Data Visualization ---------------------------------------------------
  
  # Landcover preparation
  lc_class <-landcover %>% freq() %>%
    select(ID=value) %>%
    left_join(c_ref, by="ID") %>% select(-C_factor)
  levels(landcover)[[1]] <- lc_class
  
  # Reclassify erosion rates based on China National Standard (2008)
  breaks <- c(-Inf, 5, 25, 50, 80, 150, Inf)
  labels <- c("Slight (< 5 ton/ha/yr)", 
              "Mild (5-25 ton/ha/yr)", 
              "Moderate (25-50 ton/ha/yr)", 
              "Strong (50-80 ton/ha/yr)", 
              "Very strong (80-150 ton/ha/yr)", 
              "Severe (> 150 ton/ha/yr)")
  rcl_matrix <- cbind(breaks[-length(breaks)], breaks[-1], 1:(length(breaks)-1))
  
  erosion_classified <- classify(a, rcl = rcl_matrix)
  levels(erosion_classified) <- data.frame(id=1:6, category=labels)
  plot(erosion_classified)
  
  writeRaster(erosion_classified, filename = paste0(output_dir, "soil_erosion_reclass.tif"), overwrite = TRUE)
  
  # Create dataset
  erosion_db <- data.frame(erosion_classified) %>%
    group_by(across(everything())) %>% 
    summarise(count = n())
  colnames(erosion_db, do.NULL = FALSE)
  colnames(erosion_db) <- c("Soil Erosion Rates","Area (Ha)")
  erosion_db$`Area (Ha)`*(10000/(path$map_resolution^2))
  erosion_db$`Percentage (%)` <- (erosion_db$`Area (Ha)`/sum(erosion_db$`Area (Ha)`))*100
  
  # hist_erosion(df = erosion_db)
  
  write.csv(erosion_db, file = paste0(output_dir, "soil_erosion.csv"))
  
  # End of the script
  end_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  
  # 9. Prepare parameters for report -------------------------------------
  
  report_params <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    output_dir = output_dir,
    dem = dem,
    pu = pu,
    rainfall = rainfall_annual,
    soil = soil_stack,
    landcover = landcover,
    r = r,
    k = k,
    ls = ls,
    c = c,
    p = p,
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
