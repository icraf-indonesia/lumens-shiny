# Prepare land cover data -------------------------------------------------
#' Prepare Land Cover Data
#'
#' This function loads a raster file and adds a legend to it based on the provided lookup table.
#'
#' @param path A character string specifying the file path to the raster data.
#' @param year A numeric value indicating the year of the land cover data.
#' @param lookup_table A data frame containing the lookup information for land cover classes.
#'
#' @return A SpatRaster object with added legend information.
#'
#' @importFrom terra rast
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' lc_data <- prepare_lc_data("path/to/raster.tif", 2020, lc_lookup_table)
#' }
#'
#' @export
prepare_lc_data <- function(path, year, lookup_table) {
  rast(path) %>%
    add_legend_to_categorical_raster(year = year, lookup_table = lookup_table)
}


#' Add legend to categorical raster
#'
#' This function adds a legend to a categorical raster file, often containing information about land cover or planning units.
#'
#' @param raster_file A categorical raster file (an object of class `SpatRaster`)
#' @param lookup_table A corresponding lookup table of descriptions for each class category
#' @param year An optional year to be associated with the raster file
#'
#' @return A raster file that contains descriptions for each class category
#' @importFrom terra levels freq time names
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' add_legend_to_categorical_raster(raster_file = kalbar_11,
#'               lookup_table = lc_lookup_klhk,
#'               year = 2011) %>%
#'               plot()
#' }

add_legend_to_categorical_raster <- function(raster_file, lookup_table, year = NULL) {
  # Check if raster_file is a SpatRaster object
  if (!inherits(raster_file, "SpatRaster")) {
    stop("raster_file should be a SpatRaster object")
  }

  # Check if lookup_table is a data frame
  if (!is.data.frame(lookup_table)) {
    stop("lookup_table should be a data frame")
  }

  # Check if the first column of lookup_table is numeric or convertible to numeric
  first_column <- lookup_table[[1]]
  if (!is.numeric(first_column) && any(is.na(as.numeric(first_column)))) {
    stop("The first column of lookup_table should be numeric or convertible to numeric")
  }

  # Check if year is a numeric value or NULL, and if it consists of 4 digits
  if (!is.null(year) && (!is.numeric(year) || nchar(as.character(year)) != 4)) {
    stop("year should be a numeric value consisting of 4 digits")
  }

  # Filter lookup_table to only include values present in raster_file
  lookup_table <- lookup_table[lookup_table[[1]] %in% terra::freq(raster_file)[["value"]], ]

  # Convert lookup_table into a data frame
  lookup_table <- data.frame(lookup_table)

  # Convert the first column to numeric if it is not already
  if (!is.numeric(first_column)) {
    lookup_table[[1]] <- as.numeric(first_column)
  }

  # Get the names of raster_file
  name_rast <- names(raster_file)

  # Set the levels of raster_file to be lookup_table
  levels(raster_file) <- lookup_table

  # Set the names of raster_file
  raster_file <- setNames(raster_file, name_rast)

  # Set the year if year is not NULL
  if (!is.null(year)) {
    terra::time(raster_file, tstep="years") <- year
  }

  # Return the modified raster_file
  return(raster_file)
}



harmonise_nodata <- function(..., nodata_value = 0) {
  # Collect all raster arguments
  rasters <- list(...)

  # Check if we have at least one raster
  if (length(rasters) == 0) {
    stop("At least one raster must be provided")
  }

  # Create binary maps for each raster
  binary_maps <- lapply(rasters, function(r) {
    ifel(is.na(r) | r == nodata_value, 0, 1)
  })

  # Create a basemap by multiplying all binary maps
  basemap <- Reduce(`*`, binary_maps)

  # Apply the basemap to each original raster
  harmonised_rasters <- lapply(rasters, function(r) {
    r_harmonised <- r * basemap
    r_harmonised[is.na(r_harmonised) | r_harmonised == nodata_value] <- NA
    return(r_harmonised)
  })

  # Return the list of harmonised rasters
  return(harmonised_rasters)
}

#' Check and Harmonise Geometries of Multiple SpatRaster Objects
#'
#' This function checks the geometric consistency between multiple SpatRaster objects
#' and harmonizes them to match the geometry of the first input raster if inconsistencies
#' are found.
#'
#' @param ... SpatRaster objects to be checked and potentially harmonized.
#'
#' @return A list containing the input SpatRaster objects, potentially resampled
#'         to match the geometry of the first input raster.
#'
#' @details
#' The function compares the geometries of all input rasters to the first one using
#' terra::compareGeom(). If inconsistencies are found, it resamples the
#' inconsistent rasters to match the first raster using the "near" method.
#'
#' @note
#' This function issues warnings when inconsistencies are detected and
#' messages about the harmonization process.
#'
#' @importFrom terra compareGeom resample
#'
#' @export
check_and_harmonise_geometries <- function(...) {
  # Collect all SpatRaster arguments
  rasters <- list(...)

  # Check if we have at least one raster
  if (length(rasters) == 0) {
    stop("At least one SpatRaster must be provided")
  }

  # The first raster will be our reference
  reference_raster <- rasters[[1]]

  harmonised_layers <- character()

  # Check and harmonize each raster from the second onward
  for (i in 2:length(rasters)) {
    if (!terra::compareGeom(reference_raster, rasters[[i]], stopOnError = FALSE)) {
      raster_name <- deparse(substitute(...))[[i]]
      warning(paste("Inconsistent geometry detected for", raster_name, ". Harmonizing..."))
      rasters[[i]] <- terra::resample(rasters[[i]], reference_raster, method = "near")
      harmonised_layers <- c(harmonised_layers, raster_name)
    }
  }

  if (length(harmonised_layers) > 0) {
    message("Harmonization complete. The following layers were harmonised to match the first input raster:")
    message(paste("-", harmonised_layers, collapse = "\n"))
  } else {
    message("All input geometries are consistent.")
  }

  # Name the output list elements using the input argument names
  names(rasters) <- sapply(substitute(list(...))[-1], deparse)

  return(rasters)
}

#' Set up initial parameters for TECI analysis
#'
#' This function initializes and returns all the necessary parameters for the TECI analysis.
#' It creates a temporary folder and writes the input landuse raster to this folder.
#'
#' @param landuse A raster object representing land use
#' @param ouput_dir A string path to the QUES-B directory (default: "data/quesb_test/")
#' @param classdesc A string path to the class descriptor file (default: "data/quesb_test/descriptors.fcd")
#' @param cont_fsq A string path to the contrast CSV file (default: "data/quesb_test/contrast_euc.fsq")
#' @param fca A string path to the Fragstats model file (default: "data/quesb_test/teciuf.fca")
#' @param adjacent_only Numeric, whether to consider only adjacent cells (default: 1)
#' @param gridres Numeric, grid resolution (default: 10000)
#' @param windowsize Numeric, size of the moving window (default: 1000)
#' @param window.shape Numeric, shape of the moving window (default: 1)
#' @param raster.nodata Numeric, value to be treated as NoData in the raster (default: 0)
#'
#' @return A list containing all necessary parameters for TECI analysis
#'
#' @import raster
#'
#' @examples
#' landuse <- raster("path/to/landuse.tif")
#' params <- setupTECIParameters(landuse)
#'
#' @export
setupTECIParameters <- function(landuse,
                                output_dir,
                                classdesc,
                                cont_fsq,
                                fca,
                                adjacent_only = 1,
                                gridres = 10000,
                                windowsize = 1000,
                                window.shape = 1,
                                raster.nodata = 0,
                                fragstats_path = NULL) {

  # Create a temporary directory
  temp_dir <- tempfile(pattern = "TECI_temp_")
  dir.create(temp_dir, showWarnings = FALSE)

  # Write the landuse raster to the temporary directory
  lu_path <- file.path(temp_dir, basename(sources(landuse)))
  writeRaster(landuse, filename = lu_path,  overwrite = TRUE)

  # Initialize parameters list
  params <- list()

  # Model ID (typically 1 for TECI analysis)
  params$modid <- 1

  # Internal name (typically empty string for TECI)
  params$internal <- ''

  # CPF parameter (typically empty string for TECI)
  params$cpf <- ''

  # I/O information
  params$io <- '[BAND:1]'

  # Description (typically empty string for TECI)
  params$desc <- ''

  # Driver library (GDAL for geospatial data)
  params$drlib <- 'GDAL'

  # Driver name (GeoTIFF for this case)
  params$drname <- 'GeoTIFF grid (.tif)'

  # Driver ID (specific to GDAL for GeoTIFF)
  params$drid <- '63B45E15-C8E5-44f6-A9AB-60E1852CDB5D'

  # Extract extent information from the landuse raster
  params$xl1 <- xmin(landuse)
  params$yl1 <- ymin(landuse)
  params$xu1 <- xmax(landuse)
  params$yu1 <- ymax(landuse)

  # Extract cell size (resolution) from the landuse raster
  params$csize1 <- xres(landuse)

  # Extract row and column counts from the landuse raster
  params$rowc1 <- nrow(landuse)
  params$colc1 <- ncol(landuse)

  # Allow class zero (typically 1 for yes)
  params$aczero <- "1"

  # No data value (typically 255 for 8-bit rasters)
  params$nodata <- 255

  # Background value (typically 999 for TECI analysis)
  params$bvalue <- 999

  # Store the land use path
  params$lu_path <- lu_path

  # Add new parameters
  params$output_dir <- normalizePath(output_dir)
  params$classdesc <- normalizePath(classdesc)
  params$cont_fsq <- normalizePath(cont_fsq)
  params$fca <- normalizePath(fca)
  params$adjacent_only <- adjacent_only
  params$gridres <- gridres
  params$windowsize <- windowsize
  params$window.shape <- window.shape
  params$raster.nodata <- raster.nodata
  params$fragstats_path <- fragstats_path

  # Store the temporary directory path
  params$temp_dir <- temp_dir

  # Return the parameters list
  return(params)
}


#' Clean up previous TECI process results
#'
#' This function removes files and directories from previous TECI analyses
#' to ensure a clean slate for the new analysis.
#'
#' @param params A list of parameters, including the path to the land use raster file
#'
#' @return NULL
#'
#' @examples
#' params <- list(lu_path = "path/to/landuse.tif")
#' cleanupPreviousTECI(params)
#'
#' @export
cleanupPreviousTECI <- function(params) {
  # Loop through potential previous output directories
  for (i in 1:3) {
    # Construct the path for the potential previous output
    mwout <- paste0(params$lu_path, '_mw', i)

    # Check if the directory exists
    if (dir.exists(mwout)) {
      # If it exists, attempt to delete it
      unlink(mwout, recursive = TRUE)
      message(paste("Deleted previous TECI output directory:", mwout))
    } else {
      message(paste("No previous TECI output directory found:", mwout))
    }
  }
}


#' Set up Fragstats SQLite database connection
#'
#' This function establishes a connection to the SQLite database used by Fragstats.
#' It checks for the existence of the Fragstats model file (.fca) and connects to it.
#'
#' @param params A list of parameters, including the path to the QUES-B directory
#'
#' @return A database connection object
#'
#' @import RSQLite
#'
#' @examples
#' params <- list(quesb_dir = "path/to/quesb_directory")
#' db_conn <- setupFragstatsDatabase(params)
#'
#' @export
setupFragstatsDatabase <- function(params) {
  # Check if the Fragstats model file exists
  if (!file.exists(params$fca)) {
    stop("Fragstats model file (teciuf.fca) not found in the QUES-B directory.")
  }

  message("Fragstats model file found!")

  # Set up the database connection
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv,
                   dbname = params$fca,
                   max.con = 200,
                   fetch.default.rec = 500,
                   force.reload = FALSE,
                   shared.cache = FALSE,
                   staged.queries = TRUE)

  # Clear existing records from frg_landscape_layers table
  dbExecute(con, "DELETE FROM frg_landscape_layers")

  return(con)
}


#' Reclassify a raster to binary values
#'
#' This function reclassifies a SpatRaster object to binary values,
#' where a specified target value becomes 1, and all other non-NA values become 0.
#'
#' @param raster A SpatRaster object to be reclassified.
#' @param target_value The value in the input raster that should be reclassified to 1.
#'
#' @return A new SpatRaster object with binary values (1, 0, and NA).
#'
#' @importFrom terra app
#'
#' @examples
#' \dontrun{
#'   library(terra)
#'   # Create a sample raster
#'   r <- rast(nrows=10, ncols=10, vals=sample(1:5, 100, replace=TRUE))
#'   # Reclassify, setting all 3's to 1 and other values to 0
#'   r_binary <- reclassify_to_binary(r, target_value = 3)
#' }
#'
#' @export
reclassify_to_binary <- function(raster, target_value) {
  reclass <- function(x) {
    # Use nested ifelse for the reclassification logic:
    # If x equals target_value, return 1
    # Else, if x is NA, return NA
    # Otherwise, return 0
    ifelse(x == target_value, 1, ifelse(is.na(x), NA, 0))
  }

  app(raster, reclass)
}

library(terra)

#' Create a binary raster from input raster
#'
#' @param raster SpatRaster. The input raster.
#'
#' @return SpatRaster. A binary raster where 1 represents any non-NA value and 0 represents NA.
#'
#' @examples
#' \dontrun{
#' # Create a sample raster
#' r <- rast(nrows=10, ncols=10, vals=sample(c(1:5, NA), 100, replace=TRUE))
#'
#' # Create binary raster
#' binary_r <- create_binary_raster(r)
#' plot(binary_r)
#' }
#'
#' @export
create_binary_raster <- function(raster) {
  # Check if input is a SpatRaster
  if (!inherits(raster, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }

  # Create binary raster
  binary_raster <- ifel(is.na(raster), NA, 1)

  # Set layer name
  names(binary_raster) <- paste0("binary_", names(raster))

  return(binary_raster)
}

#' Update Fragstats parameters in the database
#'
#' This function updates various parameters in the Fragstats SQLite database
#' necessary for the TECI analysis.
#'
#' @param db_conn A database connection object
#' @param params A list of parameters including paths and analysis settings
#'
#' @return NULL
#'
#' @import RSQLite
#'
#' @examples
#' params <- list(
#'   classdesc = "path/to/classdesc.csv",
#'   quesb_dir = "path/to/quesb_dir",
#'   cont_fsq = "path/to/cont_fsq.csv",
#'   windowsize = 1000,
#'   window.shape = 1,
#'   adjacent_only = 0
#' )
#' db_conn <- setupFragstatsDatabase(params)
#' updateFragstatsParameters(db_conn, params)
#'
#' @export
updateFragstatsParameters <- function(db_conn, params) {

  # Update class descriptor file path
  dbExecute(db_conn, sprintf("UPDATE frg_table_strings SET value='%s' WHERE rec_id=5;", params$classdesc))

  # Update edge contrast file path
  dbExecute(db_conn, sprintf("UPDATE frg_table_strings SET value='%s' WHERE rec_id=2;", params$cont_fsq))
  # if (params$adjacent_only == 1) {
  #   dbExecute(db_conn, sprintf("UPDATE frg_table_strings SET value='%s' WHERE rec_id=2;", normalizePath(params$cont_fsq)))
  # } else {
  #   # If not adjacent_only, use modified contrast CSV
  #   cont_fsq_mod <- gsub("\\.csv$", "_mod.csv", params$cont_fsq)
  #   dbExecute(db_conn, sprintf("UPDATE frg_table_strings SET value='%s' WHERE rec_id=2;", normalizePath(cont_fsq_mod)))
  # }

  # Update output directory
  dbExecute(db_conn, sprintf("UPDATE frg_table_strings SET value='%s' WHERE rec_id=6;",params$output_dir))

  # Update window size for square and circle
  dbExecute(db_conn, sprintf("UPDATE frg_table_numerics SET value=%d WHERE rec_id=18;", params$windowsize))
  dbExecute(db_conn, sprintf("UPDATE frg_table_numerics SET value=%d WHERE rec_id=19;", params$windowsize))

  # Update window shape
  dbExecute(db_conn, sprintf("UPDATE frg_table_numerics SET value=%d WHERE rec_id=13;", params$window.shape))

  # If not adjacent_only, update additional parameters
  if (params$adjacent_only == 0) {
    dbExecute(db_conn, "UPDATE frg_table_metrics SET value='0' WHERE rec_id=173;")
    dbExecute(db_conn, "UPDATE frg_table_metrics SET value='1' WHERE rec_id=280;")
    dbExecute(db_conn, "UPDATE frg_table_options SET value='0' WHERE rec_id=27;")
    dbExecute(db_conn, "UPDATE frg_table_options SET value='1' WHERE rec_id=29;")
  }
}

#' Insert landscape layer information into Fragstats database
#'
#' This function verifies the required parameters and inserts the landscape layer
#' information into the frg_landscape_layers table of the Fragstats SQLite database.
#'
#' @param db_conn A database connection object
#' @param params A list of parameters including landscape layer information
#'
#' @return NULL
#'
#' @import RSQLite
#'
#' @examples
#' params <- list(
#'   modid = 1,
#'   internal = '',
#'   lu_path = 'path/to/landuse.tif',
#'   io = '[BAND:1]',
#'   drlib = 'GDAL',
#'   drname = 'GeoTIFF grid (.tif)',
#'   drid = '63B45E15-C8E5-44f6-A9AB-60E1852CDB5D',
#'   xl1 = 100, yl1 = 200, xu1 = 1100, yu1 = 1200,
#'   csize1 = 30,
#'   rowc1 = 1000, colc1 = 1000,
#'   aczero = '1',
#'   nodata = 255,
#'   bvalue = 999
#' )
#' db_conn <- setupFragstatsDatabase(params)
#' insertLandscapeLayer(db_conn, params)
#'
#' @export
insertLandscapeLayer <- function(db_conn, params) {
  # Define required parameters
  required_params <- c("modid", "internal", "lu_path", "io", "drlib", "drname", "drid",
                       "xl1", "yl1", "xu1", "yu1", "csize1", "rowc1", "colc1",
                       "aczero", "nodata", "bvalue")

  # Check if all required parameters are present
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop(paste("Missing required parameters:", paste(missing_params, collapse = ", ")))
  }

  # Construct the SQL INSERT statement
  sql <- paste0(
    "INSERT INTO frg_landscape_layers",
    "(model_id, name_internal, name_external, io_info, driver_lib, driver_name, driver_id, ",
    "xll, yll, xur, yur, cell_size, row_count, col_count, allow_class_zero, no_data_value, background_value) ",
    "VALUES ",
    "(:modid, :internal, :lu_path, :io, :drlib, :drname, :drid, ",
    ":xl1, :yl1, :xu1, :yu1, :csize1, :rowc1, :colc1, :aczero, :nodata, :bvalue)"
  )

  # Execute the INSERT statement
  tryCatch({
    result <- dbExecute(db_conn, sql, params = params[required_params])

    if (result != 1) {
      warning("Unexpected number of rows affected when inserting landscape layer.")
    } else {
      message("Landscape layer information successfully inserted.")
    }
  }, error = function(e) {
    stop(paste("Error inserting landscape layer:", e$message))
  })
}


#' Execute Fragstats for TECI analysis
#'
#' This function executes the Fragstats software to perform the TECI analysis.
#' It supports Fragstats 4.x versions.
#'
#' @param params A list of parameters including paths to Fragstats and Fragstats settings
#'
#' @return NULL
#'
#' @importFrom utils file.path
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   output_dir = "path/to/output_dir",
#'   fca = "path/to/teciuf.fca"
#' )
#' executeFragstats(params)
#' }
#'
#' @export
executeFragstats <- function(params) {
  # Find Fragstats directory if not provided
  if (is.null(params$fragstats_path)) {
    program_files <- c("C:/Program Files/", "C:/Program Files (x86)/")
    fragstats_dirs <- list.files(program_files, pattern = "^Fragstats 4", full.names = TRUE)

    if (length(fragstats_dirs) == 0) {
      stop("No Fragstats 4.x installation found.")
    }

    # Sort directories to use the latest version if multiple are found
    fragstats_path <- sort(fragstats_dirs, decreasing = TRUE)[1]
  }

  message(paste("Using Fragstats installation:", fragstats_path))

  # Check if Fragstats directory exists
  if (!dir.exists(fragstats_path)) {
    stop("Specified Fragstats directory does not exist.")
  }

  # Set working directory to Fragstats directory
  old_wd <- setwd(fragstats_path)
  on.exit(setwd(old_wd), add = TRUE)

  # Prepare Fragstats command
  sysout <- file.path(params$output_dir, "fragout")
  command <- sprintf('frg_cmd -m "%s" -o "%s"', params$fca, sysout)

  # Execute Fragstats
  result <- system(command)

  if(result != 0) {
    stop("Fragstats execution failed. Check Fragstats installation and parameters.")
  } else {
    message("Fragstats execution completed successfully.")
  }
}



#' Process TECI output from Fragstats
#'
#' This function processes the output from Fragstats to create the final TECI raster.
#' It supports output from different versions of Fragstats 4.x.
#'
#' @param params A list of parameters including paths and output settings
#'
#' @return A raster object representing the TECI values
#'
#' @import raster
#'
#' @examples
#' params <- list(
#'   lu_path = "path/to/landuse.tif",
#'   quesb_dir = "path/to/quesb_dir",
#'   fragstats_version = "4.3"
#' )
#' teci_raster <- processTECIOutput(params)
#'
#' @export
processTECIOutput <- function(params) {
  # Construct path to TECI output
  mwout <- paste0(params$lu_path, '_mw1')
  teci_file <- list.files(mwout, full.names = TRUE)[1]

  if(is.null(teci_file) || !file.exists(teci_file)) {
    stop("TECI output file not found.")
  }

  # Read TECI raster
  teci_raster <- rast(teci_file)

  # Set NA value
  NAflag(teci_raster) <- -999

  return(teci_raster)
}


#' Perform TECI (Total Edge Contrast Index) analysis
#'
#' This function performs a complete TECI analysis on the given land use raster,
#' including setup, Fragstats execution, and output processing.
#'
#' @param landuse SpatRaster. A raster object representing the land use.
#' @param output_dir character. Path to the output directory for TECI results.
#' @param classdesc character. Path to the class descriptor file.
#' @param cont_fsq character. Path to the contrast FSQ file.
#' @param fca character. Path to the Fragstats model file.
#' @param adjacent_only numeric. Whether to consider only adjacent cells (1) or not (0). Default is 1.
#' @param gridres numeric. Grid resolution for analysis. Default is 10000.
#' @param windowsize numeric. Size of the moving window for TECI calculation. Default is 1000.
#' @param window.shape numeric. Shape of the moving window (1 for square, 2 for circle). Default is 1.
#' @param raster.nodata numeric. Value to be treated as NoData in the raster. Default is 0.
#'
#' @return SpatRaster. A raster object representing the TECI values.
#'
#' @details
#' This function performs the following steps:
#' 1. Sets up parameters for TECI analysis
#' 2. Cleans up previous TECI results (commented out in current version)
#' 3. Sets up and updates the Fragstats database
#' 4. Inserts the landscape layer information
#' 5. Executes Fragstats for TECI calculation
#' 6. Processes the TECI output
#' 7. Writes the result to a file and returns it as a SpatRaster object
#'
#' @note
#' This function requires Fragstats 4.x to be installed on the system.
#' The function uses several helper functions that should be defined elsewhere in the script.
#'
#' @import terra
#' @import dplyr
#' @import RSQLite
#'
#' @examples
#' \dontrun{
#' landuse <- rast("path/to/landuse.tif")
#' result <- teci_analysis(
#'   landuse,
#'   output_dir = "data/teci_output/",
#'   classdesc = "data/descriptors.fcd",
#'   cont_fsq = "data/contrast_euc.fsq",
#'   fca = "data/teciuf.fca"
#' )
#' }
#'
#' @export
teci_analysis <- function(landuse,
                          output_dir,
                          classdesc,
                          cont_fsq,
                          fca,
                          adjacent_only = 1,
                          gridres = 10000,
                          windowsize = 1000,
                          window.shape = 1,
                          raster.nodata = 0,
                          fragstats_path = NULL) {

  # Set up parameters
  params <- setupTECIParameters(landuse = landuse,
                                output_dir = output_dir,
                                classdesc = classdesc,
                                cont_fsq = cont_fsq,
                                fca = fca,
                                adjacent_only = adjacent_only,
                                gridres = gridres,
                                windowsize = windowsize,
                                window.shape = window.shape,
                                raster.nodata = raster.nodata,
                                fragstats_path = NULL)

  # Clean up previous TECI results
  #cleanupPreviousTECI(params = params)

  # Retrieve focal area ID (land cover class that represents a habitat of interest)
  focal_area_ID <- params$classdesc %>%
    read.csv() %>%
    filter(Enabled %in% TRUE) %>%
    pull(ID) %>%
    .[1]

  # Create a focal area map
  focal_area_map <- reclassify_to_binary(landuse, focal_area_ID)

  # Set name and time attributes for the result raster
  names(focal_area_map) <- paste0(
    "focal_area_",focal_area_ID,"_",
    sub("\\.[^.]+$",
        "",
        basename(terra::sources(landuse))))

  terra::time(focal_area_map, tstep="years") <- time(landuse)

  # Construct output file path and write result to file
  focal_area_path <- file.path(output_dir,
                         paste0("focal_area_",
                                focal_area_ID,
                                "_",
                                basename(terra::sources(landuse))))

  writeRaster(focal_area_map, focal_area_path, overwrite = TRUE)


  # Set up Fragstats database
  db_conn <- setupFragstatsDatabase(params)

  # Update Fragstats parameters
  updateFragstatsParameters(db_conn, params)

  # Insert landscape layer
  insertLandscapeLayer(db_conn, params)

  # Execute Fragstats
  executeFragstats(params)

  # Process TECI output
  result <- processTECIOutput(params)


  # Set name and time attributes for the result raster
  names(result) <- paste0(
    "teci_",focal_area_ID,"_",
    sub("\\.[^.]+$",
        "",
        basename(terra::sources(landuse))))

  terra::time(result, tstep="years") <- time(landuse)

  # Construct output file path and write result to file
  teci_path <- file.path(output_dir,
                         paste0("teci_",
                                focal_area_ID,
                                "_",
                                basename(terra::sources(landuse))))

  writeRaster(result, teci_path, overwrite = TRUE)

  # Clean up temporary directory and close database connection
  unlink(params$temp_dir, recursive = TRUE)
  dbDisconnect(db_conn)

  # Return the resulting TECI and raster
  return(list(teci = rast(teci_path), focal_area = rast(focal_area_path)))
}



#' Generate Sampling Grid
#'
#' This function generates a sampling grid (polygon) based on a reference raster.
#' It uses the sf and terra packages for spatial operations.
#'
#' @param ref SpatRaster. A reference raster object.
#' @param g_res numeric. The desired grid resolution. If 0, n will be used instead.
#' @param n integer. The number of points to generate if g_res is 0.
#'
#' @return sf object. A polygon grid covering the extent of the input raster.
#'
#' @import sf
#' @import terra
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' # Create a sample raster
#' r <- rast(nrows=100, ncols=100, xmin=0, xmax=10, ymin=0, ymax=10)
#' values(r) <- 1:ncell(r)
#'
#' # Generate sampling grid
#' grid <- generate_sampling_grid(r, g_res = 1)
#' plot(grid)
#' }
#'
#' @export
generate_sampling_grid <- function(ref, n = 1000, seed = 100) {
  # Ensure ref is a SpatRaster
  if (!inherits(ref, "SpatRaster")) {
    stop("ref must be a SpatRaster object")
  }

  binary_raster <- create_binary_raster(ref)

  # Convert raster to polygon
  ref_poly_sf <- as.polygons(binary_raster, dissolve=TRUE) %>%
              st_as_sf()
  # Generate points
  set.seed(seed)
  sampling_points <- st_sample(ref_poly_sf,
                               size = n,
                               type = "regular",
                               exact = TRUE,
                               replace=TRUE,
                               progress = TRUE,
                               great_circles = FALSE)
  # Calculate min_distance
  min_distance <- min(apply(st_distance(sampling_points), 1,
                            function(x) min(x[x > 0]))) %>%
    floor()
  # produce sampling grid
  sampling_grid <- st_buffer(sampling_points, dist = min_distance/2, endCapStyle = "SQUARE") %>%
    st_as_sf() %>%
    mutate(ID = row_number(),  .before =1) %>%
    mutate(area = st_area(.), .after =1) %>%
    mutate(area = units::set_units(x = area, value = "ha")) %>%
    terra::vect()

  return(sampling_grid)
}

