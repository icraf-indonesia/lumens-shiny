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