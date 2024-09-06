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

# 3. Calculation of LS ----------------------------------------------------

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

calculate_c_lc <- function(lc) {
  for (i in 1:length(lc)) {
    landcover <- lc[i]
    lc_factor <- as.factor(landcover)
    levels(lc_factor) <- c_ref
    c <- catalyze(lc_factor)
  }
  return(c)
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
  tif_layer <- file.path(getwd(), paste0(basename(layer), "_", location_name, '.tif'))
  
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
