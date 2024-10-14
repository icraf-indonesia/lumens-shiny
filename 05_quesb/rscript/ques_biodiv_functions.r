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
                                windowsize = 1000,
                                window.shape = 1,
                                raster.nodata = 0,
                                fragstats_path = NULL) {

  # Create a temporary directory
  temp_dir <- tempfile(pattern = "TECI_temp_")
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Write the landuse raster to the temporary directory
  lu_path <- file.path(temp_dir, basename(sources(landuse)))
  
  if (!endsWith(tolower(lu_path), ".tif")) {
    lu_path <- paste0(lu_path, ".tif")
  }
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
  fca <- if(is.null(fca)) {
    warning("fca is NULL. Using default value: teciuf.fca")
    "teciuf.fca"
  } else fca
  params$fca <- normalizePath(fca)
  params$adjacent_only <- adjacent_only
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
    ifelse(x %in% target_value, 1, ifelse(is.na(x), NA, 0))
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
  desc_tab <- read.csv(params$classdesc)
  id_focal <- desc_tab %>% filter(Enabled %in% TRUE) %>% pull(ID)
  # Construct path to TECI output
  mwout <- paste0(params$lu_path, '_mw1')
  teci_file_name <- paste0("teci_", id_focal, ".tif")
  teci_files <- list.files(mwout, full.names = TRUE)
  matching_file <- teci_files[grep(paste0("/", teci_file_name, "$"), teci_files, fixed = FALSE)][1]
  
  # Check if a match was found
  if (length(matching_file) == 0) {
    print("No matching file found")
  } else if (length(matching_file) > 1) {
    print("Multiple matching files found. Using the first match.")
    print(matching_file[1])
  } else {
    print("Matching file found:")
    print(matching_file)
  }

  if(is.null(matching_file) || !file.exists(matching_file)) {
    stop("TECI output file not found.")
  }

  # Read TECI raster
  teci_raster <- rast(matching_file)

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
#' @param windowsize numeric. Size of the moving window for TECI calculation. Default is 1000.
#' @param window.shape numeric. Shape of the moving window (1 for square, 2 for circle). Default is 1.
#' @param raster.nodata numeric. Value to be treated as NoData in the raster. Default is 0.
#' @param fragstats_path character. Path to the Fragstats executable. Default is NULL.
#'
#' @return list. A list containing three elements:
#'   \item{teci}{SpatRaster. A raster object representing the TECI values.}
#'   \item{focal_area}{SpatRaster. A binary raster representing the focal area.}
#'   \item{total_area}{numeric. Total area of the landscape in hectares.}
#'
#' @details
#' This function performs the following steps:
#' 1. Checks if the output directory exists
#' 2. Calculates the total area of the landscape in hectares
#' 3. Processes the class descriptor file
#' 4. Creates a focal area map based on the habitat of interest
#' 5. Sets up parameters for TECI analysis
#' 6. Sets up and updates the Fragstats database
#' 7. Inserts the landscape layer information
#' 8. Executes Fragstats for TECI calculation
#' 9. Processes the TECI output
#' 10. Writes the results to files and returns them as a list
#'
#' @note
#' This function requires Fragstats 4.x to be installed on the system.
#' The function uses several helper functions that should be defined elsewhere in the script,
#' such as create_binary_raster(), setupTECIParameters(), reclassify_to_binary(),
#' setupFragstatsDatabase(), updateFragstatsParameters(), insertLandscapeLayer(),
#' executeFragstats(), and processTECIOutput().
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
#' # Access the results
#' teci_raster <- result$teci
#' focal_area_raster <- result$focal_area
#' total_landscape_area <- result$total_area
#' }
#'
#' @export
teci_analysis <- function(landuse,
                          output_dir,
                          classdesc,
                          cont_fsq,
                          fca = fca,
                          adjacent_only = 1,
                          windowsize = 1000,
                          window.shape = 1,
                          raster.nodata = 0,
                          fragstats_path = NULL,
                          timestep = NULL) {
  
  # Check if the output directory exists
  if (!dir.exists(output_dir)) {
    stop("Error: The specified output directory does not exist: ", output_dir)
  }
  # Calculate total area of the landscape (in Hectares)
  total_area_ <- landuse %>%
    create_binary_raster() %>%
    # Convert to ha
    suppressWarnings(aggregate(., fact = c(100, 100) / res(.),
                               fun = "modal")) %>%
    freq() %>%
    pull(count) %>% 
    sum()
  
  # Read and process the class descriptor file
  classdesc_tbl <- classdesc %>%
    read.csv() %>%
    rename(ID = 1, CLASS = 2, BIODIV = 3) %>%
    filter(!ID %in% raster.nodata)
  
  # Prepare the class descriptor for Fragstats
  classdesc_fcd <- classdesc_tbl %>%
    rename(ID = 1, Name = 2, Enabled = 3) %>%
    mutate(Enabled = as.logical(Enabled)) %>%
    mutate(IsBackground = FALSE)
  
  # Create a path for the processed class descriptor file
  classdesc_path <- paste0(output_dir, "/", "habitat_lookup.fcd")
  
  # Write the processed class descriptor to a file
  write.table(classdesc_fcd,
              classdesc_path,
              sep = ",",
              row.names = FALSE,
              quote = FALSE)
  
  # Set up parameters for TECI analysis
  params <- setupTECIParameters(landuse = landuse,
                                output_dir = output_dir,
                                classdesc = classdesc_path,
                                cont_fsq = cont_fsq,
                                fca = fca,
                                adjacent_only = adjacent_only,
                                windowsize = windowsize,
                                window.shape = window.shape,
                                raster.nodata = raster.nodata,
                                fragstats_path = NULL)
  
  # Retrieve focal area ID (land cover class that represents a habitat of interest)
  focal_area_ID <- classdesc_tbl %>%
    filter(BIODIV %in% 1) %>%
    pull(1) %>%
    .[1]
  
  # Create a focal area map
  focal_area_map <- reclassify_to_binary(landuse, focal_area_ID)
  
  # Set name and time attributes for the focal area raster
  names(focal_area_map) <- paste0(
    "focal_area_", focal_area_ID, "_",
    if (!is.null(timestep)) paste0(timestep, "_"),
    sub("\\.[^.]+$", "", basename(terra::sources(landuse))))
  
  terra::time(focal_area_map, tstep = "years") <- timestep
  
  # Construct output file path and write focal area result to file
  focal_area_filename <- paste0("focal_area_",
                                focal_area_ID,
                                if (!is.null(timestep)) paste0("_", timestep),
                                "_",
                                sub("\\.[^.]+$", "", basename(terra::sources(landuse))))
  
  focal_area_path <- file.path(output_dir, focal_area_filename)
  
  if (!endsWith(tolower(focal_area_path), ".tif")) {
    focal_area_path <- paste0(focal_area_path, ".tif")
  }
  
  writeRaster(focal_area_map, focal_area_path, overwrite = TRUE)
  
  # Set up Fragstats database
  db_conn <- setupFragstatsDatabase(params)
  
  # Update Fragstats parameters
  updateFragstatsParameters(db_conn, params)
  
  # Insert landscape layer into Fragstats database
  insertLandscapeLayer(db_conn, params)
  
  # Execute Fragstats for TECI calculation
  executeFragstats(params)
  
  # Process TECI output
  result <- processTECIOutput(params)

  # Set name and time attributes for the TECI result raster
  names(result) <- paste0(
    "teci_", focal_area_ID, "_",
    if (!is.null(timestep)) paste0(timestep, "_"),
    sub("\\.[^.]+$", "", basename(terra::sources(landuse))))
  
  terra::time(result, tstep = "years") <- timestep
  
  # Construct output file path and write TECI result to file
  teci_filename <- paste0("teci_",
                          focal_area_ID,
                          if (!is.null(timestep)) paste0("_", timestep),
                          "_",
                          sub("\\.[^.]+$", "", basename(terra::sources(landuse))))
  
  teci_path <- file.path(output_dir, teci_filename)
  
  if (!endsWith(tolower(teci_path), ".tif")) {
    teci_path <- paste0(teci_path, ".tif")
  }
  
  writeRaster(result, teci_path, overwrite = TRUE)
  
  # Clean up temporary directory and close database connection
  unlink(params$temp_dir, recursive = TRUE)
  dbDisconnect(db_conn)
  
  # Return the resulting TECI raster, focal area raster, and total area
  return(list(
    teci = rast(teci_path),
    focal_area = rast(focal_area_path),
    total_area = total_area_,
    path_teci_map = teci_path,
    path_focal_area = focal_area_path
  ))
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


#' Calculate DIFA (Degree of Integration of Focal Areas)
#'
#' This function calculates the DIFA table, score, and plot based on input TECI map,
#' focal area, sampling grid, and total landscape area.
#'
#' @param teci_map SpatRaster object representing the TECI (Threat Ecological Corridor Index) map
#' @param focal_area SpatRaster object representing the focal area
#' @param sampling_grid SpatVector object representing the sampling grid
#' @param output_dir character. Path to the output directory for TECI results.
#' @param total_area_landscape Numeric value representing the total landscape area in hectares
#'
#' @return A list containing:
#'   \item{difa_table}{A data frame containing the DIFA table}
#'   \item{difa_score}{Numeric value representing the DIFA score}
#'   \item{difa_year}{Numeric value representing the year (can be NULL if not found)}
#'   \item{difa_plot}{A ggplot2 object representing the DIFA plot}
#'
#' @import terra
#' @import tidyverse
#' @import caTools
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- calculate_difa(teci_map, focal_area, sampling_grid, total_area_landscape)
#' print(result$difa_plot)
#' }
calculate_difa <- function(teci_map, focal_area, sampling_grid, total_area_landscape, output_dir) {
  # Input validation
  if (!inherits(teci_map, "SpatRaster")) {
    stop("teci_map must be a SpatRaster object")
  }
  if (!inherits(focal_area, "SpatRaster")) {
    stop("focal_area must be a SpatRaster object")
  }
  if (!inherits(sampling_grid, "SpatVector")) {
    stop("sampling_grid must be a SpatVector object")
  }
  if (!("ID" %in% names(sampling_grid))) {
    stop("sampling_grid must contain a column named 'ID'")
  }
  if (!is.numeric(total_area_landscape) || length(total_area_landscape) != 1) {
    stop("total_area_landscape must be a single numeric value")
  }

  # Try to extract year from teci_map or focal_area
  year <- tryCatch(
    teci_map %>% time() %>% as.numeric(),
    error = function(e)
      tryCatch(
        focal_area %>% time() %>% as.numeric(),
        error = function(e)
          NULL
      )
  )

  # Calculate median TECI values for each grid cell
  grid_teci <- terra::zonal(
    teci_map,
    sampling_grid,
    fun = "median",
    na.rm = TRUE,
    small = FALSE
  )

  # Aggregate focal area and calculate sum for each grid cell
  grid_focal_area <- suppressWarnings(terra::aggregate(
    focal_area,
    fact = c(100, 100) / res(focal_area),
    fun = "modal"
  )) %>%
    terra::zonal(
      .,
      sampling_grid,
      fun = "sum",
      na.rm = TRUE,
      small = FALSE
    )

  # Combine grid data with TECI and focal area information
  raw_extract <- sampling_grid %>%
    as.data.frame() %>%
    cbind(grid_teci) %>%
    cbind(grid_focal_area) %>%
    as_tibble() %>%
    arrange(.[3]) %>%
    mutate(
      cumsum_fa = cumsum(.[[4]]),
      cumsum_fa_perc = cumsum_fa / total_area_landscape * 100
    ) %>%
    tidyr::drop_na()

  # Prepare DIFA table
  difa_table <- raw_extract %>%
    select(TECI = 3, FocalArea = 6) %>%
    bind_rows(tibble(TECI = 100, FocalArea = max(raw_extract[6])))

  difa_path <- paste0(output_dir,
                      "/",
                      gsub("teci", "difa_tbl", names(teci_map)),
                      ".csv")

  write.csv(difa_table, difa_path, quote = FALSE, row.names = FALSE)

  # Calculate DIFA score using trapezoidal rule
  difa_score <- round((caTools::trapz(na.omit(difa_table[["TECI"]]),
                                      difa_table[["FocalArea"]])) / 100,
                      digits = 2
  )

  # Create DIFA plot
  # difa_plot <- ggplot(difa_table, aes(x = TECI, y = FocalArea, xend = 100, yend = 100)) +
  #   geom_area(alpha = 0.8) +
  #   labs(x = "DIFA", y = "Cumulative Focal Area (%)") +
  #   theme_bw() +
  #   labs(caption = paste("DIFA Score = ", difa_score))

  # Return results as a list
  return(list(
    difa_table = difa_table,
    difa_score = difa_score,
    difa_year = year,
   # difa_plot = difa_plot
   difa_path = difa_path

  ))
}

#' Perform QuESB (Quantification of  Environmental Services - Biodiversity) analysis for a single time period
#'
#' This function performs a complete QuES-B analysis for a single time period, including
#' TECI (Total Edge Contrast Index) analysis and DIFA (Degree of Integration of Focal Areas) calculation.
#'
#' @param lulc_lut_path character. Path to the land use/land cover lookup table CSV file.
#' @param lc_t1_path character. Path to the land cover raster file for the time period.
#' @param t1 numeric. Year of the land cover data.
#' @param raster.nodata numeric. Value to be treated as NoData in the raster. Default is 0.
#' @param contab_path character. Path to the contrast table (FSQ file).
#' @param sampling_points numeric. Number of sampling points for the grid. Default is 1000.
#' @param window_size numeric. Size of the moving window for TECI calculation in meters. Default is 1000.
#'   This dispersal radius is used as a threshold for ecological connectivity, assuming that species
#'   associated with the focal area can typically move or disperse up to this distance. Areas beyond
#'   this radius are considered too far from the focal habitat to contribute significantly to its
#'   ecological function or connectivity.
#' @param window.shape numeric. Shape of the moving window (0 for square, 1 for circle). Default is 0.
#' @param output_dir character. Path to the directory for output files.
#' @param fca_path character. Path to the Fragstats model file (optional). Default is NULL.
#' @param fragstats_path character. Path to Fragstats software (optional). Default is NULL.
#'
#' @return A list containing the following elements:
#'   \item{teci_map}{SpatRaster. A raster object representing the TECI values.}
#'   \item{focal_area}{SpatRaster. A binary raster representing the focal area.}
#'   \item{total_area}{units. Total area of the landscape in hectares.}
#'   \item{difa_table}{data.frame. Table of DIFA calculations.}
#'   \item{difa_score}{numeric. Overall DIFA score.}
#'   \item{difa_plot}{ggplot. Plot of DIFA results.}
#'
#' @details
#' This function performs the following steps:
#' 1. Loads and prepares the land cover data
#' 2. Generates a sampling grid
#' 3. Performs TECI analysis
#' 4. Calculates DIFA
#' 5. Returns the results as a list
#'
#' @note
#' This function requires Fragstats to be installed on the system if `fragstats_path` is provided.
#' It also depends on several helper functions: prepare_lc_data(), generate_sampling_grid(),
#' teci_analysis(), and calculate_difa(). Ensure these functions are available in your environment.
#'
#' @import terra
#' @import units
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' result <- quesb_single_period(
#'   lulc_lut_path = "path/to/habitat_lookup.csv",
#'   lc_t1_path = "path/to/lc_2010.tif",
#'   t1 = 2010,
#'   contab_path = "path/to/contrast_euc.fsq",
#'   output_dir = "path/to/output/",
#'   sampling_points = 1000,
#'   window_size = 1000
#' )
#'
#' # Access the results
#' teci_map <- result$teci_map
#' difa_score <- result$difa_score
#' plot(result$difa_plot)
#' }
#'
#' @export
quesb_single_period <- function(lulc_lut_path,
                                lc_t1_path,
                                t1,
                                raster.nodata = 0,
                                contab_path,
                                sampling_points = 1000,
                                window_size = 1000,
                                window.shape = 0,
                                output_dir,
                                fca_path = NULL,
                                fragstats_path = NULL) {

  # Load and prepare data
  # Load land use/land cover lookup table
  lulc_lut <- read.csv(lulc_lut_path)

  # Load and prepare land cover map
  lc_t1 <- prepare_lc_data(lc_t1_path,
                           year = t1,
                           lookup_table = lulc_lut)

  # check if nodata value present
  lc_freq <- lc_t1 %>% freq() %>% tibble::rownames_to_column("ID")

  if (raster.nodata %in% as.numeric(lc_freq[["ID"]])) {
    NAflag(lc_t1) <- raster.nodata
    message(sprintf("NoData value %s found in land cover map. Setting NAflag to this value.", raster.nodata))
  } else {
    message(sprintf("NoData value %s not found in land cover map. NAflag remains unchanged.", raster.nodata))
  }

  if (!grepl("\\+units=m", terra::crs(lc_t1, proj = TRUE))){
    stop("Raster is not in metre units. Please provide a raster with metre units.")
    } else { message("Raster is in metre units")}
  # Generate sampling grid
  # Create a polygon grid for sampling
  sampling_grid <- generate_sampling_grid(lc_t1,
                                          n = sampling_points)


  # Construct output file path and write sampling grid
  sampling_grid_path <- file.path(output_dir,
                         paste0("sampling_grid",
                                "_",
                                tools::file_path_sans_ext(
                                  basename(
                                    terra::sources(lc_t1))), ".shp"))
  writeVector(sampling_grid,
              sampling_grid_path,
              filetype = "ESRI Shapefile",
              overwrite = TRUE)

  # Perform TECI analysis
  teci_t1 <- teci_analysis(
    landuse = lc_t1,
    output_dir = output_dir,
    classdesc = lulc_lut_path,
    cont_fsq = contab_path,
    fca = fca_path,
    adjacent_only = 1,
    windowsize = window_size,
    window.shape = window.shape,
    raster.nodata = raster.nodata,
    fragstats_path = fragstats_path,
    timestep = t1  # Pass the time period for t1
  )

  # Calculate DIFA (Degree of Integration of Focal Areas) table
  difa_teci_t1 <- calculate_difa(teci_map = teci_t1$teci,
                               focal_area = teci_t1$focal_area,
                               sampling_grid = sampling_grid,
                               output_dir = output_dir,
                               total_area_landscape = teci_t1$total_area)

  # Return results
  return(
    list(teci_map_t1 = teci_t1$teci,
         focal_area_t1 = teci_t1$focal_area,
         total_area = teci_t1$total_area %>% units::as_units("ha"),
         difa_table_t1 = teci_t1$difa_table,
         difa_score_t1 = teci_t1$difa_score,
         path_teci_map_t1 = teci_t1$path_teci_map,
         path_focal_area_t1 = teci_t1$path_focal_area,
         path_sampling_grid = sampling_grid_path,
         path_difa_table_t1 = difa_teci_t1$difa_path)
  )
}


quesb_two_periods <- function(lulc_lut_path,
                              lc_t1_path,
                              t1,
                              lc_t2_path,
                              t2,
                              raster.nodata = 0,
                              contab_path,
                              sampling_points = 1000,
                              window_size = 1000,
                              window.shape = 0,
                              output_dir,
                              fca_path = NULL,
                              fragstats_path = NULL) {
  
  
  # Load and prepare data
  # Load land use/land cover lookup table
  lulc_lut <- read.csv(lulc_lut_path)
  
  # Load and prepare land cover maps
  lc_t1 <- prepare_lc_data(lc_t1_path, year = t1, lookup_table = lulc_lut)
  lc_t2 <- prepare_lc_data(lc_t2_path, year = t2, lookup_table = lulc_lut)
  
  # Harmonize NoData values
  list_lc <- harmonise_nodata(lc_t1, lc_t2, nodata_value = raster.nodata)
  lc_t1 <- list_lc[[1]]
  lc_t2 <- list_lc[[2]]
  
  # Harmonize geometries
  list_lc <- check_and_harmonise_geometries(lc_t1, lc_t2)
  lc_t1 <- list_lc$lc_t1
  lc_t2 <- list_lc$lc_t2
  
  # Generate sampling grid
  sampling_grid <- generate_sampling_grid(lc_t1, n = sampling_points)
  
  # Save sampling grid
  sampling_grid_path <- file.path(output_dir,
                                  paste0("sampling_grid",
                                         "_",
                                         tools::file_path_sans_ext(
                                           basename(
                                             terra::sources(lc_t1))), ".shp"))
  writeVector(sampling_grid,
              sampling_grid_path,
              filetype = "ESRI Shapefile",
              overwrite = TRUE)
  # Perform TECI analysis for both time periods
  teci_t1 <- teci_analysis(
    landuse = lc_t1,
    output_dir = output_dir,
    classdesc = lulc_lut_path,
    cont_fsq = contab_path,
    fca = fca_path,
    adjacent_only = 1,
    windowsize = window_size,
    window.shape = window.shape,
    raster.nodata = raster.nodata,
    fragstats_path = fragstats_path,
    timestep = t1  # Pass the time period for t1
  )
  
  teci_t2 <- teci_analysis(
    landuse = lc_t2,
    output_dir = output_dir,
    classdesc = lulc_lut_path,
    cont_fsq = contab_path,
    fca = fca_path,
    adjacent_only = 1,
    windowsize = window_size,
    window.shape = window.shape,
    raster.nodata = raster.nodata,
    fragstats_path = fragstats_path,
    timestep = t2  # Pass the time period for t2
  )
  
  
  
  # Calculate DIFA for both time periods
  difa_t1 <- calculate_difa(
    teci_map = teci_t1$teci,
    focal_area = teci_t1$focal_area,
    sampling_grid = sampling_grid,
    output_dir = output_dir,
    total_area_landscape = teci_t1$total_area
  )
  
  difa_t2 <- calculate_difa(
    teci_map = teci_t2$teci,
    focal_area = teci_t2$focal_area,
    sampling_grid = sampling_grid,
    output_dir = output_dir,
    total_area_landscape = teci_t2$total_area
  )
  
  # Calculate TECI Difference map
  teci_2_bin<- reclassify_to_binary(teci_t2$teci, 1) %>% reclassify_to_binary(0) %>% 
    classify( cbind(NA,0))
  teci_1_bin<- reclassify_to_binary(teci_t1$teci, 1) %>% reclassify_to_binary(0)%>% 
    classify( cbind(NA,0))
  teci_bin <- teci_2_bin + teci_1_bin
  teci_bin <- reclassify_to_binary(teci_bin, c(1,2)) %>% 
    classify(cbind(0,NA))
    
  teci_focal_change <- (teci_2_bin + teci_1_bin) %>%
    classify(cbind(0,NA))
  
  teci_bin_diff<- teci_focal_change %>% classify(cbind(1,NA)) %>% 
    classify(cbind(2,1))
  
  teci_loss <- teci_focal_change %>%  classify(cbind(2,NA)) %>% 
    classify(cbind(1,999)) %>% classify(cbind(NA,0))
    
  
  teci_t2_reclassed <- classify(teci_t2$teci, cbind(NA,0))
  teci_t1_reclassed <- classify(teci_t1$teci, cbind(NA,0))
  
  constant_focal_area <- ((teci_t2_reclassed - teci_t1_reclassed)*teci_bin_diff) %>%
    reclassify_to_binary(0) %>%
    reclassify_to_binary(1) %>%
    classify(cbind(NA,0))
  
  # Calculate the difference between TECI maps
  teci_diff <- ((teci_t2_reclassed - teci_t1_reclassed)*teci_bin_diff) %>% 
    classify(cbind(NA,0))
  
  teci_decrease <- (teci_diff<0) %>% classify(cbind(TRUE,2)) 
  teci_increase <- (teci_diff>0) %>% classify(cbind(TRUE,3)) 
  
  # interpretation
  # 999 means focal area loss (disconnected)
  # positive value 0-100 increase in total edge contrast value (segregated)
  # negative value -100-0 decrease in total edge contrast value (integrated)
  net_change <- (teci_diff+ teci_loss)*teci_bin
  
  # Save the difference map
  teci_diff_path <- file.path(output_dir, paste0("teci_change_",t1,"_",t2,".tif"))
  writeRaster(net_change, teci_diff_path, overwrite = TRUE)
  
  # interpretation net_change_category
  # 4 means focal area loss (disconnected)
  # 3 increase in total edge contrast value (segregated)
  # 2 decrease in total edge contrast value (integrated)
  # 1 core focal area with 0 teci value
  net_change_category_lookup_table <- tibble(Value= c(1:4),
         Description = c("Fully integrated core focal area",
                         "Decrease in total edge contrast value (integration)",
                         "Increase in total edge contrast value (segregation)",
                         "Focal edge area loss"))
  
  net_change_category <- (constant_focal_area+teci_increase+teci_decrease+classify(teci_loss, cbind(999,4))) %>% 
    classify(cbind(0,NA))
  
  levels(net_change_category)[[1]]<- data.frame(net_change_category_lookup_table)
  
  # Save the difference category map
  teci_diff_reclassed_path <- file.path(output_dir, paste0("teci_change_categories",t1,"_",t2,".tif"))
  writeRaster(net_change_category, teci_diff_reclassed_path, overwrite = TRUE)
  
  # Return results
  return(
    list(
      teci_map_t1 = teci_t1$teci,
      teci_map_t2 = teci_t2$teci,
      teci_difference = net_change,
      teci_diff_category = net_change_category,
      focal_area_t1 = teci_t1$focal_area,
      focal_area_t2 = teci_t2$focal_area,
      total_area = teci_t1$total_area %>% units::as_units("ha"),
      difa_table_t1 = difa_t1$difa_table,
      difa_score_t1 = difa_t1$difa_score,
      difa_table_t2 = difa_t2$difa_table,
      difa_score_t2 = difa_t2$difa_score,
      path_teci_map_t1 = teci_t1$path_teci_map,
      path_teci_map_t2 = teci_t2$path_teci_map,
      path_focal_area_t1 = teci_t1$path_focal_area,
      path_focal_area_t2 = teci_t2$path_focal_area,
      path_sampling_grid = sampling_grid_path,
      path_difa_table_t1 = difa_t1$difa_path,
      path_difa_table_t2 = difa_t2$difa_path,
      path_teci_difference = teci_diff_path,
      path_teci_diff_category = teci_diff_reclassed_path
    )
  )
}


format_session_info_table <- function() {
  si <- sessionInfo()
  
  # Extract R version info
  r_version <- si$R.version[c("major", "minor", "year", "month", "day", "nickname")]
  r_version <- paste0(
    "R ", r_version$major, ".", r_version$minor,
    " (", r_version$year, "-", r_version$month, "-", r_version$day, ")",
    " '", r_version$nickname, "'"
  )
  
  # Extract platform and OS info
  platform_os <- paste(si$platform, "|", si$running)
  
  # Extract locale info
  locale_info <- strsplit(si[[3]], ";")[[1]]
  locale_info <- paste(locale_info, collapse = "<br>")
  
  # Extract .libpaths, accomodate multiple library paths
  lib_paths <- .libPaths() |> paste( collapse = "<br>")
  
  # Combine all info into a single tibble
  session_summary <- tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details = c(r_version, platform_os, lib_paths, locale_info)
  )
  
  return(session_summary)
}



# plot_categorical_raster -------------------------------------------------

#' Plot a categorical raster map
#'
#' This function takes a raster object as input and produces a ggplot. If the raster
#' object includes a "color_pallete" column with hex color codes, these colors are
#' used for the fill scale. Otherwise, the default `scale_fill_hypso_d()` fill scale
#' from the tidyterra package is used.
#'
#' @param raster_object A raster object.
#'
#' @return A ggplot object.
#' @importFrom tidyterra scale_fill_hypso_d
#' @importFrom ggplot2 ggplot theme_bw labs theme scale_fill_manual element_text unit element_blank guides guide_legend
#' @importFrom tidyterra geom_spatraster scale_fill_hypso_d
#' @export
plot_categorical_raster <- function(raster_object) {
  # Check if raster_object has a color_pallete column and it contains hex color codes
  if ("color_palette" %in% names(cats(raster_object)[[1]]) && all(grepl("^#[0-9A-Fa-f]{6}$", cats(raster_object)$color_pallete))) {
    fill_scale <- scale_fill_manual(values = cats(raster_object)[[1]]$color_palette, na.value = "white")
  } else {
    fill_scale <- scale_fill_hypso_d()
  }
  if(!is.na(time(raster_object))) {
    plot_title <- time(raster_object)
  } else {
    plot_title <- names(raster_object)
  }
  # Generate the plot
  plot_lc <- ggplot() +
    geom_spatraster(data = raster_object) +
    fill_scale +
    theme_bw() +
    labs(title = plot_title, fill = NULL) +
    guides(fill = guide_legend(title.position = "top", ncol=3))+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.key.height = unit(0.25, "cm"),
          legend.key.width = unit(0.25, "cm"),
          legend.position = "bottom",
          legend.justification = c(0,0.8))

  return(plot_lc)
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


#' Run QuES-B Analysis
#'
#' This function performs a QuES-B analysis using provided inputs and generates a report.
#'
#' @param lc_t1_path A character string specifying the path to the land cover raster file for the first time period.
#' @param t1 A numeric value representing the time period of the first land cover raster.
#' @param nodata_class A numeric value representing the no-data class in the raster.
#' @param lulc_lut_path A character string specifying the path to the land use/land cover look-up table.
#' @param contab_path A character string specifying the path to the contingency table.
#' @param sampling_points A numeric value specifying the number of sampling points to use.
#' @param window_size A numeric value specifying the size of the window to use for the analysis.
#' @param window.shape A numeric value specifying the shape of the window (0 for square, 1 for circle).
#' @param fca_path An optional character string specifying the path to the focal class analysis (FCA) file. Defaults to NULL.
#' @param fragstats_path An optional character string specifying the path to the Fragstats executable. Defaults to NULL.
#' @param output_dir A character string specifying the directory where output files and reports will be saved.
#' @param report_template_path A character string specifying the path to the R Markdown report template.
#'
#' @details
#' This function performs a series of steps to run the QuES-B analysis, including:
#' \itemize{
#'   \item Creating the output directory if it does not exist.
#'   \item Running the `quesb_single_period` function with the provided parameters to perform the core analysis.
#'   \item Generating a report using an R Markdown template.
#' }
#'
#' The function sets a default path for the FCA file if not provided. For Fragstats, it searches for an installation
#' in the Program Files directories if a path is not provided.
#'
#' @return A list (`report_params`) containing parameters used in the report generation, including paths to various
#' output files and the analysis results from `quesb_single_period`.
#'
#' @examples
#' \dontrun{
#' run_ques_b(
#'   lc_t1_path = "path/to/lc_t1.tif",
#'   t1 = 2000,
#'   nodata_class = -9999,
#'   lulc_lut_path = "path/to/lulc_lut.csv",
#'   contab_path = "path/to/contab.csv",
#'   sampling_points = 1000,
#'   window_size = 5,
#'   window.shape = 0,
#'   fca_path = NULL,
#'   fragstats_path = NULL,
#'   output_dir = "path/to/output",
#'   report_template_path = "path/to/report_template.Rmd"
#' )
#' }
#'
#' @import rmarkdown
#' @importFrom utils dir.create list.files
#' @importFrom methods is
#' @export
run_ques_b <- function(lc_t1_path, t1, lc_t2_path = NULL, t2 = NULL, nodata_class, lulc_lut_path, contab_path,
                       sampling_points, window_size, window.shape, fca_path = NULL,
                       fragstats_path = NULL, output_dir, report_template_path) {
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Run QuES-B analysis
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  if (!is.null(lc_t2_path) && !is.null(t2)) {
    # Run two-period analysis
    quesb_result <- quesb_two_periods(
      lc_t1_path = lc_t1_path,
      t1 = t1,
      lc_t2_path = lc_t2_path,
      t2 = t2,
      raster.nodata = nodata_class,
      lulc_lut_path = lulc_lut_path,
      contab_path = contab_path,
      output_dir = output_dir,
      sampling_points = sampling_points,
      window_size = window_size,
      window.shape = window.shape,
      fca_path = fca_path,
      fragstats_path = fragstats_path[1]
    )
  } else {
    # Run single-period analysis
    quesb_result <- quesb_single_period(
      lc_t1_path = lc_t1_path,
      t1 = t1,
      raster.nodata = nodata_class,
      lulc_lut_path = lulc_lut_path,
      contab_path = contab_path,
      output_dir = output_dir,
      sampling_points = sampling_points,
      window_size = window_size,
      window.shape = window.shape,
      fca_path = fca_path,
      fragstats_path = fragstats_path[1]
    )
  }
  
  # End of the script
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  session_log <- format_session_info_table()
  
  # Prepare parameters for report rendering
  report_params <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    output_dir = output_dir,
    total_area = quesb_result$total_area,  # Ensure total_area is included
    session_log = session_log,
    inputs = list(
      lc_t1_path = fs::path_abs(lc_t1_path),
      t1 = t1,
      raster.nodata = nodata_class,
      lulc_lut_path = fs::path_abs(lulc_lut_path),
      contab_path = fs::path_abs(contab_path),
      output_dir = fs::path_abs(output_dir),
      sampling_points = sampling_points,
      window_size = window_size,
      window.shape = window.shape
    ),
    dir_sampling_grid = basename(quesb_result$path_sampling_grid)
  )
  
  if (!is.null(lc_t2_path) && !is.null(t2)) {
    report_params$inputs$lc_t2_path <- lc_t2_path
    report_params$inputs$t2 <- t2
    report_params$dir_teci_map_t1 <- basename(quesb_result$path_teci_map_t1)
    report_params$dir_teci_map_t2 <- basename(quesb_result$path_teci_map_t2)
    report_params$dir_focal_area_t1 <- basename(quesb_result$path_focal_area_t1)
    report_params$dir_focal_area_t2 <- basename(quesb_result$path_focal_area_t2)
    report_params$dir_teci_difference <- basename(quesb_result$path_teci_difference)
    report_params$dir_teci_diff_category <- basename(quesb_result$path_teci_diff_category)
    report_params$dir_difa_table_t1 <- basename(quesb_result$path_difa_table_t1)
    report_params$dir_difa_table_t2 <- basename(quesb_result$path_difa_table_t2)
    report_params$difa_score_t1 <- quesb_result$difa_score_t1
    report_params$difa_score_t2 <- quesb_result$difa_score_t2
    
  } else {
    report_params$dir_teci_map_t1 <- basename(quesb_result$path_teci_map_t1)
    report_params$dir_focal_area_t1 <- basename(quesb_result$path_focal_area_t1)
    report_params$dir_difa_table_t1 <- basename(quesb_result$path_difa_table_t1)
    report_params$difa_score_t1 <- quesb_result$difa_score_t1
  }

  report_params_path <- file.path(output_dir, "output_parameters.rds") %>% fs::path_abs()
  save(report_params, file= report_params_path)
  
  # Render the R Markdown report
  rmarkdown::render(
    input = report_template_path,
    output_file = "QuES_B_report.html",
    output_dir = output_dir,
    params = report_params,
    knit_root_dir = getwd()
  )
  
  return(report_params)
}



plot_teci_difference_map <- function(teci_diff_raster) {
  library(ggplot2)
  library(tidyterra)
  
  # Create the plot
  ggplot() +
    geom_spatraster(data = teci_diff_raster) +
    scale_fill_gradient2(
      low = "green",
      mid = "white",
      high = "red",
      midpoint = 0,
      name = "TECI Difference"
    ) +
    theme_bw() +
    labs(title = "TECI Difference Map") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.position = "right"
    )
}
