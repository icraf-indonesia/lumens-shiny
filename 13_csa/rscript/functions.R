read_shapefile <- function(shp_input) {
  if (is.null(shp_input)) return(NULL)
  
  prev_wd <- getwd()
  on.exit(setwd(prev_wd), add = TRUE)  # This ensures we always return to the previous working directory
  
  tryCatch({
    uploaded_dir <- dirname(shp_input$datapath[1])
    setwd(uploaded_dir)
    
    for (i in 1:nrow(shp_input)) {
      old_path <- shp_input$datapath[i]
      new_path <- file.path(uploaded_dir, shp_input$name[i])
      cat("Attempting to rename:", old_path, "to", new_path, "\n")
      rename_result <- file.rename(old_path, new_path)
      cat("Rename result:", rename_result, "\n")
      if (!rename_result) {
        cat("File exists (old):", file.exists(old_path), "\n")
        cat("File exists (new):", file.exists(new_path), "\n")
      }
    }
    
    shp_file <- shp_input$name[grep(pattern = "*.shp$", shp_input$name)]
    if (length(shp_file) == 0) {
      stop("No .shp file found in the uploaded files.")
    }
    
    required_extensions <- c("shp", "dbf", "prj", "shx")
    missing_files <- required_extensions[!required_extensions %in% tools::file_ext(list.files(uploaded_dir))]
    if (length(missing_files) > 0) {
      stop(paste("Missing required shapefile components:", paste(missing_files, collapse = ", ")))
    }
    
    cat("About to read shapefile:", shp_file, "\n")
    cat("Files in directory after renaming:\n")
    print(list.files(uploaded_dir))
    
    # Read and return the shapefile
    sf_object1 <- sf::st_read(shp_file)
    sf_object <- st_cast(sf_object1, "MULTIPOLYGON")
    return(sf_object)
  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    stop(paste("Error reading shapefile:", e$message))
  })
}

rasterise_multipolygon <- function(sf_object, raster_res = c(100,100), field = "ID"){
  
  # Error checking
  if (!inherits(sf_object, "sf")) stop("sf_object must be an sf object.")
  if (!all(sf::st_geometry_type(sf_object) == "MULTIPOLYGON")) stop("All features in sf_object must be MULTIPOLYGONs.")  # Check if sf_object has UTM projection
  if (!grepl("\\+units=m", st_crs(sf_object)$proj4string)) stop("sf_object must have UTM projection system.")
  if (is.null(sf::st_drop_geometry(sf_object)) || !(field %in% names(sf::st_drop_geometry(sf_object)))) stop("sf_object must contain an attribute table with at least one numeric/factor column.")
  if (!is.numeric(sf_object[[field]]) && !is.factor(sf_object[[field]])) stop("The field must be numeric or a factor.")
  
  # Convert the sf object to a SpatVector
  spatvect <- terra::vect(sf_object)
  
  # Define the extent based on the SpatVector
  raster_extent <- terra::ext(spatvect)
  
  # Create an empty SpatRaster based on the extent, resolution, and CRS
  raster_template <- terra::rast(raster_extent, resolution = raster_res, crs = terra::crs(spatvect))
  
  # Rasterize the SpatVector based on the SpatRaster template
  # Specify the field in the rasterize function
  rasterised_spatraster <- terra::rasterize(spatvect, raster_template, field = field)
  
  # Convert the 'Kabupaten' column of the sf_object to a lookup_table
  lookup_table <- sf::st_drop_geometry(sf_object)
  
  # Add legend to the rasterized SpatRaster using the lookup_table
  levels(rasterised_spatraster) <- lookup_table
  
  # Return the rasterized SpatRaster with legend
  return(rasterised_spatraster)
}

preprocess_data <- function(
    pathLULCT,
    zone_type,
    pathPU,
    pathLookupLC,
    pathLookupPU = NULL,
    pathLookupCO2,
    pathLookupSF,
    year = NULL
) {
  
  # --- Load packages ---
  library(terra)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(plotly)
  
  # -------------------------------
  # 1. READ INPUT DATA
  # -------------------------------
  LULCT <- rast(pathLULCT)
  LookupLC <- read_csv(pathLookupLC)
  LookupSF <- read_csv(pathLookupSF)
  LookupCO2 <- read_csv(pathLookupCO2)
  
  # Set names and levels for LULCT
  name_rast <- names(LULCT)
  levels(LULCT) <- LookupLC
  LULCT <- setNames(LULCT, name_rast)
  
  # Add year if provided
  if (!is.null(year)) {
    year <- as.numeric(year)
    terra::time(LULCT, tstep = "years") <- year
  }
  
  # -------------------------------
  # 1B. READ PLANNING UNITS (PU)
  # -------------------------------
  if (zone_type == "raster") {
    
    PU <- rast(pathPU)
    LookupPU <- read_csv(pathLookupPU)
    
    name_PU <- names(PU)
    levels(PU) <- LookupPU
    PU <- setNames(PU, name_PU)
    
  } else if (zone_type == "shapefile") {
    
    sf_object <- read_shapefile(pathPU)
    
    if (is.null(sf_object)) {
      stop("Failed to read shapefile. Please check your input.")
    }
    
    # Rename columns
    sf_object <- sf_object %>%
      dplyr::rename(Value = 1, planning_unit = 2)
    
    # Create lookup table from shapefile attributes
    LookupPU <- sf::st_drop_geometry(sf_object)
    
    lc_res <- terra::res(LULCT)
    PU <- rasterise_multipolygon(sf_object, raster_res = lc_res, field = "Value")
    
    levels(PU) <- LookupPU
  }
  
  # -------------------------------
  # 2. HARMONIZE RASTERS
  # -------------------------------
  PU <- terra::resample(PU, LULCT, method = "near")
  
  combinedRaster <- c(PU, LULCT)
  
  # -------------------------------
  # 3. BUILD FREQUENCY TABLE
  # -------------------------------
  res_m <- terra::res(LULCT)
  area_ha_per_pixel <- (res_m[1] * res_m[2]) / 10000
  
  combinedRasterTable <- combinedRaster %>%
    as_tibble() %>%
    tidyr::drop_na() %>%
    setNames(c("PU", "LC")) %>%
    dplyr::filter(LC == "Pertanian") %>%
    group_by(across(everything())) %>%
    mutate(Freq = n()) %>%
    ungroup() %>%
    distinct() %>%
    mutate(Ha = Freq * area_ha_per_pixel)
  
  # -------------------------------
  # 4. PREPARE LOOKUP TABLES
  # -------------------------------
  Lookup_wide <- LookupSF %>%
    select(variable, value) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(
      Total_EF = EF * SFw * SFs * SFr
    )
  
  # -------------------------------
  # 5. CALCULATE CH4 + CO2-eq
  # -------------------------------
  combinedRasterTable <- combinedRasterTable %>%
    mutate(
      CH4_emission = Lookup_wide$Total_EF * Lookup_wide$t * Ha * 1e-3,
      CH4_emission_CO2 = CH4_emission * LookupCO2$GWP[LookupCO2$Gas == "CH4"] * 1e-6
    )
  
  # -------------------------------
  # 6. SUM BY PU
  # -------------------------------
  summary_by_PU <- combinedRasterTable %>%
    group_by(PU) %>%
    summarise(
      `Emission from CH4 (Juta Ton CO2-eq/tahun)` = sum(CH4_emission_CO2, na.rm = TRUE)
    )
  
  # Add dummy N2O
  summary_by_PU$N2O_emission <- 0.5
  
  # Convert to long format
  summary_long <- summary_by_PU %>%
    pivot_longer(
      cols = c(`Emission from CH4 (Juta Ton CO2-eq/tahun)`, N2O_emission),
      names_to = "Gas",
      values_to = "Value"
    )
  
  # -------------------------------
  # 7. PLOT
  # -------------------------------
  p <- ggplot(summary_long,
              aes(
                x = reorder(PU, Value),
                y = Value,
                fill = Gas,
                text = paste0(
                  "PU: ", PU, "<br>",
                  "Gas: ", Gas, "<br>",
                  "Emisi: ", round(Value, 3), " Juta Ton CO₂-eq/tahun"
                )
              )) +
    geom_col() +
    labs(
      title = "Emisi CH₄ dan N₂O (CO₂-eq) per PU",
      x = "PU",
      y = "Juta Ton CO₂-eq / tahun",
      fill = "Jenis Gas"
    ) +
    theme_minimal() +
    coord_flip()
  
  plot_interactive <- ggplotly(p, tooltip = "text")
  
  # -------------------------------
  # RETURN ALL RESULTS
  # -------------------------------
  return(list(
    LULCT = LULCT,
    PU = PU,
    lookup_LC = LookupLC,
    lookup_PU = LookupPU,
    lookup_SF = LookupSF,
    lookup_CO2 = LookupCO2,
    combinedRaster = combinedRaster,
    combinedRasterTable = combinedRasterTable,
    summary_by_PU = summary_by_PU,
    summary_long = summary_long,
    plot = plot_interactive
  ))
}
