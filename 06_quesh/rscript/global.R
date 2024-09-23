### Required Library ###
install_load <- function (package1, ...)  {
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  for (package in packages) {
    # if package is installed locally, load
    if (package %in% rownames(installed.packages()))
      do.call('library', list(package))
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}

install_load(
  "shiny",
  "shinyFiles",
  "bslib",
  "terra", 
  "dplyr", 
  "sf", 
  "shinyvalidate",
  "remote",
  "shinyjs",
  "rmarkdown",
  "tools",
  "magrittr", 
  "lattice", 
  "classInt", 
  "ggplot2", 
  "scales",
  "tidyr",
  "DT"
)

# Sync geometric properties --------------------------------------------

syncGeom <- function(input, ref){
  ref1 <- rast(ref) %>% 
    classify(cbind(unique(values(.)), 1))
  input %>% 
    rast() %>%
    crop(ref1) %>% 
    resample(ref1) %>% 
    `*`(ref1)
}

# Categorized landcover based on its class --------------------------------

lc_class_categorize <- function(landcover, c_ref) {
  lc_class <-landcover %>% freq() %>%
    dplyr::select(ID=value) %>%
    left_join(c_ref, by="ID") %>% dplyr::select(-C_factor)
  levels(landcover)[[1]] <- lc_class
  return(landcover)
}

# Calculate erosion for each planning unit class -----------------------------------------------------------------------

compute_erosion_per_pu <- function(erosion_classified, pu){
  e_pu_stack <- c(erosion_classified, pu)
  e_pu_stack_df <- as.data.frame(e_pu_stack, xy = TRUE, cells = TRUE)
  colnames(e_pu_stack_df) <- c("cell", "x", "y", "soil_erosion", "planning_unit")
  e_pu_stack_df <- e_pu_stack_df[complete.cases(e_pu_stack_df), ]
  summary_e_pu_df <- e_pu_stack_df %>%
    group_by(planning_unit, soil_erosion) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = soil_erosion, values_from = count, values_fill = 0)
  return(summary_e_pu_df)
}

# Create dataset erosion result -------------------------------------------

erosion_dataset <- function(erosion_classified, map_resolution){
  erosion_db <- data.frame(erosion_classified) %>%
    group_by(across(everything())) %>% 
    summarise(count = n())
  colnames(erosion_db, do.NULL = FALSE)
  colnames(erosion_db) <- c("Class","Area (Ha)")
  erosion_db$`Area (Ha)`*(10000/(map_resolution^2))
  erosion_db$`Percentage (%)` <- (erosion_db$`Area (Ha)`/sum(erosion_db$`Area (Ha)`))*100
  return(erosion_db)
}

# Calculation of R (Moore, 1979) ---------------------------------------

calculate_r_moore <- function(rainfall) {
  ke <- 11.46*rainfall - 2226
  r <- 0.029*ke - 26
  r_factor <- 17.02*r # Conversion from imperial to SI units
  return(r_factor)
}

# Calculation of K (Williams, 1995) ------------------------------------

calculate_k_williams <- function(sndprc, sltprc, clyprc, orcprc){
  a <- (0.2 + 0.3*exp(-0.0256*sndprc*(1 - sltprc/100)))
  b <- (sltprc/(clyprc + sltprc))^0.3
  c <- 1 - (0.25*orcprc)/(orcprc + exp(3.72 - 2.95*orcprc))
  sn1 <- 1 - sndprc/100
  d <- 1 - (0.7*sn1)/(sn1 + exp(-5.51 + 22.9*sn1))
  k_factor <- 0.1317*a*b*c*d
  return(k_factor)
}

# Calculation of LS (Moore & Burch, 1986) - BRIN ----------------------------------------------------

calculate_ls_moore <- function(dem) {
  slope_deg <- terrain(dem, v = "slope", unit="degree") 
  flow_acc <- terrain(dem, v = "flowdir")
  cell_size <- res(dem)[1]
  slope_length <- flow_acc * cell_size
  # ls <- (slope_length / 22.13)^0.4 * ((0.01745 * sin(slope_deg)) / 0.0896)^1.3 * 1.6 => BRIN
  intermediate_values <- abs((0.01745 * sin(slope_deg)) / 0.0896) # prevent negative or zero slope values from causing calculation issues
  ls_factor <- (slope_length / 22.13)^0.4 * (intermediate_values)^1.3
  return(ls_factor)
}

# Calculate C by landcover ------------------------------------------------

calculate_c_lc <- function(landcover = landcover, c_ref = c_ref){
  landcover_c <- landcover
  lookup_lc <-landcover_c %>% freq() %>%
    select(ID=value) %>%
    left_join(c_ref, by="ID") %>% select(-LC)
  levels(landcover_c)[[1]] <- lookup_lc
  c_factor <- landcover_c %>% as.numeric(1) %>% resample(pu, method="near")
}

# QuES-H RUSLE Function ---------------------------------------------------

quesh_rusle_calc <- function(rainfall, sand, silt, clay, orgc, dem, landcover_t1, landcover_t2, c_ref, p_factor, multiseries, pu){
  # R factor calculation
  ke <- 11.46*rainfall - 2226
  r <- 0.029*ke - 26
  r_factor <- 17.02*r # Conversion from imperial to SI units
  
  # K factor calculation
  a <- (0.2 + 0.3*exp(-0.0256*sand*(1 - silt/100)))
  b <- (silt/(clay + silt))^0.3
  c <- 1 - (0.25*orgc)/(orgc + exp(3.72 - 2.95*orgc))
  sn1 <- 1 - sand/100
  d <- 1 - (0.7*sn1)/(sn1 + exp(-5.51 + 22.9*sn1))
  k_factor <- 0.1317*a*b*c*d
  
  # LS factor calculation
  slope_deg <- terrain(dem, v = "slope", unit="degree") 
  flow_acc <- terrain(dem, v = "flowdir")
  cell_size <- res(dem)[1]
  slope_length <- flow_acc * cell_size
  intermediate_values <- abs((0.01745 * sin(slope_deg)) / 0.0896)
  ls_factor <- (slope_length / 22.13)^0.4 * (intermediate_values)^1.3
  
  # C factor calculation
  calculate_c_factor <- function(landcover) {
    landcover_c <- landcover
    lookup_lc <- landcover_c %>% freq() %>%
      dplyr::select(ID=value) %>%
      left_join(c_ref, by="ID") %>% dplyr::select(-LC)
    levels(landcover_c)[[1]] <- lookup_lc
    c_factor <- landcover_c %>% as.numeric(1) %>% terra::resample(pu, method="near")
    return(c_factor)
  }
  
  # Erosion calculation
  calculate_erosion <- function(c_factor) {
    r_factor*k_factor*ls_factor*c_factor*p_factor
  }
  
  # Calculate for one or two time series based on is_two_series parameter
  if (multiseries == "single_step" || is.null(landcover_t2)) {
    c_factor_t1 <- calculate_c_factor(landcover_t1)
    erosion_t1 <- calculate_erosion(c_factor_t1)
    
    out <- list(
      erosion = erosion_t1,
      r_factor = r_factor,
      k_factor = k_factor,
      ls_factor = ls_factor,
      c_factor = c_factor_t1
    )
  } else {
    c_factor_t1 <- calculate_c_factor(landcover_t1)
    c_factor_t2 <- calculate_c_factor(landcover_t2)
    erosion_t1 <- calculate_erosion(c_factor_t1)
    erosion_t2 <- calculate_erosion(c_factor_t2)
    
    out <- list(
      erosion_t1 = erosion_t1,
      erosion_t2 = erosion_t2,
      r_factor = r_factor,
      k_factor = k_factor,
      ls_factor = ls_factor,
      c_factor_t1 = c_factor_t1,
      c_factor_t2 = c_factor_t2
    )
  }
  
  return(out)
}

# Calculation of P factor  ---------------------------------------------

calculate_p_shin <- function(slope_pct, p_user){
  p_factor_combined <- slope_pct
  p_factor_contouring <- slope_pct
  p_factor_strip_cropping <- slope_pct
  p_factor_terracing <- slope_pct
  
  # P factor for Contouring
  p_factor_contouring[slope_pct <= 0.7] <- 0.55
  p_factor_contouring[slope_pct > 0.7 & slope_pct <= 11.3] <- 0.60
  p_factor_contouring[slope_pct > 11.3 & slope_pct <= 17.6] <- 0.80
  p_factor_contouring[slope_pct > 17.6 & slope_pct <= 26.8] <- 0.90
  p_factor_contouring[slope_pct > 26.8] <- 1.00
  
  # P factor for Strip Cropping
  p_factor_strip_cropping[slope_pct <= 0.7] <- 0.27
  p_factor_strip_cropping[slope_pct > 0.7 & slope_pct <= 11.3] <- 0.30
  p_factor_strip_cropping[slope_pct > 11.3 & slope_pct <= 17.6] <- 0.40
  p_factor_strip_cropping[slope_pct > 17.6 & slope_pct <= 26.8] <- 0.45
  p_factor_strip_cropping[slope_pct > 26.8] <- 0.50
  
  # P factor for Terracing
  p_factor_terracing[slope_pct <= 0.7] <- 0.10
  p_factor_terracing[slope_pct > 0.7 & slope_pct <= 11.3] <- 0.12
  p_factor_terracing[slope_pct > 11.3 & slope_pct <= 17.6] <- 0.16
  p_factor_terracing[slope_pct > 17.6 & slope_pct <= 26.8] <- 0.18
  p_factor_terracing[slope_pct > 26.8] <- 0.20
  
  # Determine the combined P factor based on p user
  if (p_user[1] == 1 & p_user[2] == 1 & p_user[3] == 1) {
    # All scenarios applied
    p_factor_combined <- p_factor_contouring*p_factor_strip_cropping*p_factor_terracing
  } else if (p_user[1] == 1 & p_user[2] == 1 & p_user[3] == 0) {
    # Contouring and Strip Cropping only
    p_factor_combined <- p_factor_contouring*p_factor_strip_cropping
  } else if (p_user[1] == 1 & p_user[2] == 0 & p_user[3] == 1) {
    # Contouring and Terracing only
    p_factor_combined <- p_factor_contouring*p_factor_terracing
  } else if (p_user[1] == 0 & p_user[2] == 1 & p_user[3] == 1) {
    # Strip Cropping and Terracing only
    p_factor_combined <- p_factor_strip_cropping*p_factor_terracing
  } else if (p_user[1] == 1 & p_user[2] == 0 & p_user[3] == 0) {
    # Contouring only
    p_factor_combined <- p_factor_contouring
  } else if (p_user[1] == 0 & p_user[2] == 1 & p_user[3] == 0) {
    # Strip Cropping only
    p_factor_combined <- p_factor_strip_cropping
  } else if (p_user[1] == 0 & p_user[2] == 0 & p_user[3] == 1) {
    # Terracing only
    p_factor_combined <- p_factor_terracing
  } else {
    # No scenarios applied
    p_factor_combined[] <- 1
  }
  return(p_factor_combined)
}

# Plot Histogram of Soil Erosion Rate -------------------------------------
hist_erosion <- function(df) {
  ggplot(df, aes(x = `Soil Erosion Rates`)) +
    geom_bar(aes(y = `Area (Ha)`), stat = "identity", fill = "blue", alpha = 0.6) +
    geom_text(aes(y = `Area (Ha)`, label = scales::comma(`Area (Ha)`)), 
              vjust = -0.5, color = "black", size = 4) + 
    geom_line(aes(y = `Percentage (%)` * max(df$`Area (Ha)`) / max(df$`Percentage (%)`)), 
              group = 1, color = "red", size = 1.2) +
    geom_point(aes(y = `Percentage (%)` * max(df$`Area (Ha)`) / max(df$`Percentage (%)`)), 
               color = "red", size = 3) +
    scale_y_continuous(
      name = "Area (Ha)",
      labels = scales::comma,  
      sec.axis = sec_axis(~ . * max(df$`Percentage (%)`) / max(df$`Area (Ha)`), 
                          name = "Percentage (%)", labels = scales::comma)
    ) +
    labs(title = "Soil Erosion Rates: Area (Ha) and Percentage (%)",
         x = "Soil Erosion Rates") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = "blue"),
      axis.title.y.right = element_text(color = "red"),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
    )
}


# Download Soil Data ------------------------------------------------------

#' Download SoilGrids data
#'
#' This function downloads soil data from SoilGrids for a specified location,
#' variable of interest, depth, and quantile.
#'
#' @param location_name A string specifying the name of the location.
#' @param aoi A SpatRaster object representing the area of interest.
#' @param voi A string specifying the variable of interest.
#' @param depth A string specifying the depth.
#' @param quantile A string specifying the quantile.
#'
#' @return A SpatRaster object containing the downloaded and processed soil data.
#' @export
#'
#' @importFrom sf st_bbox st_buffer st_as_sfc st_crs
#' @importFrom terra rast project crop resample writeRaster
#' @importFrom gdalUtilities gdal_translate
#' @importFrom units set_units
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' aoi <- terra::rast(system.file("external/test.grd", package="terra"))
#' soil_data <- download_soilgrids("test_location", aoi, "soc", "0-5cm", "mean")
#' }
download_soilgrids <- function(location_name, aoi, voi, depth, quantile) {
  
  # Constants
  URL_BASE <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"
  IGH_CRS <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
  
  # Construct file paths
  variable <- paste0(URL_BASE, voi, "/", voi)
  layer <- paste(variable, depth, quantile, sep = "_")
  vrt_layer <- paste0(layer, '.vrt')
  tif_layer <- file.path(getwd(), paste0(location_name,"_", basename(layer),'.tif'))
  
  # Project AOI to IGH CRS
  aoi_igh <- terra::project(aoi, IGH_CRS)
  
  # Expand bounding box
  expand_bbox <- function(bbox, distance) {
    bbox_sf <- sf::st_as_sfc(sf::st_bbox(bbox))
    if (is.na(sf::st_crs(bbox_sf))) {
      warning("CRS not set. Assuming WGS84.")
      sf::st_crs(bbox_sf) <- 4326
    }
    bbox_expanded <- sf::st_buffer(bbox_sf, dist = units::set_units(distance, "m"))
    return(sf::st_bbox(bbox_expanded))
  }
  
  bbox <- sf::st_bbox(aoi_igh) %>% 
    expand_bbox(100) # Add 100 m buffer
  
  bb <- c(bbox$xmin, bbox$ymax, bbox$xmax, bbox$ymin)
  
  # Download data
  gdalUtilities::gdal_translate(
    vrt_layer, 
    tif_layer,
    tr = c(250, 250),
    projwin = bb,
    projwin_srs = IGH_CRS
  )
  
  # Process downloaded data
  downloaded_soil_map <- terra::rast(tif_layer) %>% 
    terra::project(terra::crs(aoi)) %>%
    terra::crop(aoi) %>%
    terra::resample(aoi) %>% 
    `*`(aoi)
  
  # Save processed data
  terra::writeRaster(downloaded_soil_map, tif_layer, overwrite = TRUE)
  
  return(downloaded_soil_map)
}

#' Check and Install Required Packages
#'
#' This function checks if a list of required packages are installed and loaded.
#' If any packages are missing or cannot be loaded, it prompts the user to install them.
#'
#' @param required_packages A character vector of package names to check and potentially install.
#'
#' @return None. This function is called for its side effects.
#'
#' @details
#' The function performs the following steps:
#' 1. Checks if each package in the list is installed.
#' 2. Attempts to load each installed package.
#' 3. If any packages are missing or fail to load, prompts the user to install them.
#' 4. If the user agrees, attempts to install and load the missing packages.
#'
#' @examples
#' \dontrun{
#' required_packages <- c("dplyr", "ggplot2", "tidyr")
#' check_and_install_packages(required_packages)
#' }
#'
#' @export
check_and_install_packages <- function(required_packages) {
  # Check if each package is installed and can be loaded
  missing_packages <- character(0)
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      missing_packages <- c(missing_packages, package)
    } else {
      tryCatch(
        {
          library(package, character.only = TRUE)
          cat(paste0("Package '", package, "' is installed and loaded.\n"))
        },
        error = function(e) {
          missing_packages <<- c(missing_packages, package)
          cat(paste0("Package '", package, "' is installed but could not be loaded: ", e$message, "\n"))
        }
      )
    }
  }
  
  # If there are missing packages, ask the user if they want to install them
  if (length(missing_packages) > 0) {
    cat("\nThe following packages are missing or could not be loaded:\n")
    cat(paste0("- ", missing_packages, "\n"))
    
    install_choice <- readline(prompt = "Do you want to install/reinstall these packages? (y/n): ")
    
    if (tolower(install_choice) == "y") {
      for (package in missing_packages) {
        cat(paste0("\nAttempting to install package '", package, "'...\n"))
        tryCatch(
          {
            install.packages(package)
            library(package, character.only = TRUE)
            cat(paste0("Package '", package, "' has been successfully installed and loaded.\n"))
          },
          error = function(e) {
            cat(paste0("Failed to install package '", package, "': ", e$message, "\n"))
          }
        )
      }
    } else {
      cat("\nPackage installation skipped. Some required packages are missing.\n")
    }
  } else {
    cat("\nAll required packages are installed and loaded.\n")
  }
}

#' Rasterize an sf MULTIPOLYGON object
#'
#' This function rasterizes an sf MULTIPOLYGON object to a SpatRaster object. The function also retains
#' an attribute table from the sf object, by assigning categorical ID values to the raster values.
#' The rasterized SpatRaster object will also contain a legend derived from the attribute table of the sf object.
#'
#' @param sf_object An sf MULTIPOLYGON object. It must contain an attribute table, with at least one categorical ID (numeric).
#' @param raster_res A numeric vector specifying the resolution of the raster. Default is c(100,100).
#' @param field A character string specifying the field name to be used for rasterization from the sf object. Default is "ID".
#' @return A SpatRaster object that is a rasterized version of the input sf object, with a legend derived from the attribute table of the sf object.
#' @importFrom sf st_drop_geometry st_geometry_type st_crs
#' @importFrom terra vect ext rast rasterize levels
#' @export
#' @examples
#' rasterise_multipolygon(sf_object = ntt_admin, raster_res = c(100,100), field = "ID")
rasterise_multipolygon <- function(sf_object, raster_res = c(100,100), field = "ID"){
  
  # Error checking
  if (!inherits(sf_object, "sf")) stop("sf_object must be an sf object.")
  if (!all(sf::st_geometry_type(sf_object) == "MULTIPOLYGON")) stop("All features in sf_object must be MULTIPOLYGONs.")  # Check if sf_object has UTM projection
  if (!grepl("\\+proj=utm", st_crs(sf_object)$proj4string)) stop("sf_object must have UTM projection system.")
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

#' Read Shapefile
#'
#' This function reads a shapefile from uploaded files, handling file renaming and validation.
#'
#' @param shp_input List. Input data for the shapefile, including file paths and names.
#'
#' @return sf object. The read shapefile.
#'
#' @importFrom sf st_read
#' @importFrom tools file_ext
#'
#' @export
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
    sf_object <- sf::st_read(shp_file)
    return(sf_object)
  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    stop(paste("Error reading shapefile:", e$message))
  })
}