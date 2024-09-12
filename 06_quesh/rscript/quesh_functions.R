### Data Preparation for RUSLE Modelling ###


# 1. Calculation of R (Moore, 1979) ---------------------------------------

calculate_r_moore <- function(p) {
  ke <- 11.46*p - 2226
  r <- 0.029*ke - 26
  r_si <- 17.02*r # Conversion from imperial to SI units
  return(r_si)
}


# 2. Calculation of K (Williams, 1995) ------------------------------------

calculate_k_williams <- function(sndprc, sltprc, clyprc, orcprc){
  a <- (0.2 + 0.3*exp(-0.0256*sndprc*(1 - sltprc/100)))
  b <- (sltprc/(clyprc + sltprc))^0.3
  c <- 1 - (0.25*orcprc)/(orcprc + exp(3.72 - 2.95*orcprc))
  sn1 <- 1 - sndprc/100
  d <- 1 - (0.7*sn1)/(sn1 + exp(-5.51 + 22.9*sn1))
  k <- 0.1317*a*b*c*d
  return(k)
}

# 3. Calculation of LS  ----------------------------------------------------

# Calculate LS by (Moore & Burch, 1986) - BRIN
calculate_ls_moore <- function(dem, slope, flow_acc) {
  cell_size <- res(dem)[1]
  slope_length <- flow_acc * cell_size
  ls_factor <- (slope_length / 22.13)^0.4 * (0.01745 * sin(slope * pi / 180) / 0.0896)^1.3 * 1.6
  return(ls_factor)
}

# Calculate LS by previous LUMENS RUSLE Script
calculate_ls <- function(slope, aspect) {
  ls <- (1 + (sin(slope * pi / 180) / 0.0896)^1.3) *
    ((sin((aspect - 180) * pi / 180) + 0.5) / 1.5)^0.6
  return(ls)
}

# 4. Calculation of C (Van der Knijff et al, 2000) ------------------------

calculate_c_knijff <- function(ndvi) {
  alpha <- 2 # as suggested by Knijff 2000
  beta <- 1 # as suggested by Knijff 2000
  c <- min(exp(-alpha * (ndvi/(beta - ndvi))), 1)
  return(c)
}


# 5. Calculation of C using landcover -------------------------------------

calculate_c_lc <- function(landcover, c_ref) {
  landcover[landcover == path$raster.nodata] <- NA
  landcover_c <- landcover
  c_ref2 <- as.matrix(c_ref[,1])
  c_ref3 <- as.matrix(c_ref[,3])
  c_ref4 <- cbind(c_ref2, c_ref3)
  c_ref4 <- rbind(c_ref4, c(0, NA))
  c_factor <- classify(landcover_c, c_ref4)
  return(c_factor)
}

# 6. Sync geometric properties --------------------------------------------

syncGeom <- function(input, ref){
  ref1 <- rast(ref) %>% 
    classify(cbind(unique(values(.)), 1))
  input %>% 
    rast() %>%
    crop(ref1) %>% 
    resample(ref1) %>% 
    `*`(ref1)
}


# 7. Calculation of P factor  ---------------------------------------------

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

