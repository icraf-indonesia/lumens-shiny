#' Perform Suitability Analysis
#'
#' This function performs suitability analysis based on harmonised rasters and suitability parameters.
#' It calculates the actual and potential suitability maps and returns the results as a list.
#'
#' @param harmonised_rasters A SpatRaster object representing the harmonised raster layers.
#' @param suitability_parameter A data frame or tibble containing the crop suitability parameters.
#' @param lookup_intervention A tibble of intervention options
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{suitability_map}{A list with the actual suitability map and related data.}
#'   \item{suitability_polygon}{A polygon layer with potential suitability attributes.}
#' }
#'
#' @importFrom terra rast
#' @importFrom dplyr left_join mutate case_when
#' @importFrom sf st_transform
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming harmonised_rasters, suitability_parameter,
#'   # and path_lookup_intervention are predefined
#'   result <- perform_suitability_analysis(harmonised_rasters,
#'   suitability_parameter, path_lookup_intervention)
#' }
perform_suitability_analysis <-
  function(harmonised_rasters,
           suitability_parameter,
           lookup_intervention) {
    
    
    # Check input classes
    stopifnot(
      inherits(harmonised_rasters, "SpatRaster"),
      is.data.frame(suitability_parameter) ||
        inherits(suitability_parameter, "tbl_df")
    )
    
    # 3a. Suitability Analysis Actual
    suitability_map <-
      process_suitability(suitability_factors = harmonised_rasters,
                          crop_suitability = suitability_parameter)
    
    # 3b. Suitability Analysis Potential
    intervention_table <- list("low", "med", "high") %>%
      map(
        ~ calculate_suitability_potential_table(
          lookup_intervention = lookup_intervention,
          intervention_level = .,
          suitability_attr = suitability_map[["suitability_attr"]],
          lookup_suitability_layer = suitability_map[["lookup_suitability_factors"]]
        )
      )
    
    sutability_attr_pot <-
      Reduce(function(x, y)
        left_join(x, y, by = "ID"), intervention_table)
    
    suitability_polygon <- suitability_map$suitability_polygon |>
      left_join(sutability_attr_pot, by = "ID") |>
      mutate(
        suitability_potential_low  = case_when(suitability == "S1" ~ "S1", .default = suitability_potential_low),
        suitability_potential_med  = case_when(suitability == "S1" ~ "S1", .default = suitability_potential_med),
        suitability_potential_high  = case_when(suitability == "S1" ~ "S1", .default = suitability_potential_high)
      ) |>
      st_transform(crs = 4326)
    
    suitability_map$suitability_polygon <- suitability_polygon
    
    return(suitability_map)
  }



#' Process Suitability Factors for Crop Suitability Analysis
#'
#' This function integrates environmental suitability factors with crop suitability parameters to produce
#' a comprehensive analysis of crop suitability. It involves reclassifying raster layers based on
#' suitability parameters, creating a frequency table, and converting rasters to spatial polygons.
#' This function relies on specific functions from terra, dplyr, tidyr, and sf packages.
#'
#' @param suitability_factors A SpatRaster object from the terra package representing environmental factors
#' affecting crop growth such as soil quality, climate conditions, etc.
#' @param crop_suitability A dataframe with crop suitability parameters, each row representing a different
#' parameter and its associated suitability conditions.
#'
#' @return A list containing four elements:
#' \itemize{
#'   \item{suitability_raster}{A SpatRaster object representing the combined suitability analysis.}
#'   \item{suitability_polygon}{An sf object representing suitability areas as polygons.}
#'   \item{suitability_attr}{A dataframe containing attributes for each suitability category.}
#'   \item{suitability_by_factors}{A list of SpatRaster objects for individual suitability factors.}
#' }
#' @importFrom terra freq cats as.polygons subset activeCat levels
#' @importFrom dplyr select left_join mutate group_by summarise pull filter row_number rowwise rename pick
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom sf st_as_sf
#' @importFrom purrr map map2
#' @importFrom rlang .data
#' @importFrom tibble as_tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming suitability_factors and crop_suitability are already defined
#'   suitability_results <- process_suitability(suitability_factors, crop_suitability)
#'   View(suitability_results$suitability_raster)
#'   View(suitability_results$suitability_polygon)
#'   View(suitability_results$suitability_attr)
#'   # Explore individual factor suitability rasters
#'   lapply(suitability_results$suitability_by_factors, View)
#' }
# Function to Process Suitability Data for Crop Suitability Analysis
process_suitability <- function(suitability_factors, crop_suitability) {
  
  # Step 1: Create a DataFrame for Layer Lookup
  # This step involves creating a DataFrame that maps each layer in the suitability_factors to a name.
  lookup_suitability_factors <- create_layer_dataframe(suitability_factors)
  lookup_suitability_factors_names <- lookup_suitability_factors |> pull(names)
  
  # Step 2: Identify and List Crop Parameters
  # Extracts unique parameter names from the crop_suitability DataFrame.
  crop_suitability_param_names <- crop_suitability |> pull(name_parameter) |> unique()
  
  # Step 3: Identify Names Not in Crop Suitability
  # Finds layer names in lookup_suitability_factors that are not in crop_suitability parameters.
  names_not_in_crop_suitability <- setdiff(lookup_suitability_factors_names, crop_suitability_param_names)
  
  # Step 4: Subset Suitability Factors
  # If there are names not in crop suitability, subset the suitability factors to exclude them.
  if (!is.null(names_not_in_crop_suitability)) {
    suitability_factors <- terra::subset(suitability_factors, names_not_in_crop_suitability, negate = TRUE)
    lookup_suitability_factors <- lookup_suitability_factors |>
      filter(!names %in% names_not_in_crop_suitability) |>
      dplyr::mutate(ID = row_number())
  }
  
  # Step 5: Cross-Check and Print Names Analysed
  # Intersects the names of the suitability factors with the crop suitability parameters and prints them.
  names_analysed <- intersect(names(suitability_factors), crop_suitability_param_names)
  print_names_analysed_info(names_analysed)
  
  # Step 6: Classify Suitability of Each Predictor
  # Applies predefined functions to classify and stack suitability factors.
  suitability_factors_reclass <- classify_and_stack_suitability_factors(
    stacked_raster = suitability_factors,
    suitability_data = crop_suitability)
  
  # Step 7: Combine Suitability Rasters
  # Concatenates the levels of all the suitability rasters into one.
  suitability_raster <- concat_rasters(suitability_factors_reclass)
  
  # Step 8: Create Frequency Table for Suitability Raster
  # Generates a frequency table for the combined suitability raster.
  suitability_raster_freq <- terra::freq(suitability_raster) |> dplyr::select(categories = value, count)
  # Step 9: Extract and Process Attribute Table
  # Processes the attribute table of the suitability raster for further analysis.
  suitability_attr <- terra::levels(suitability_raster)[[1]] |>
    as_tibble() |>
    rename(categories = 2) |>
    left_join(suitability_raster_freq, by = "categories") |>
    mutate(class_category = strsplit(as.character(categories), "_")) |>
    rowwise() |>
    mutate(class = list(determine_suitability(class_category))) |>
    tidyr::unnest_wider(class) |>
    mutate(limiting_factor_id = limiting_factor) |>
    tidyr::unnest_longer(limiting_factor_id, keep_empty = TRUE) |>
    left_join(lookup_suitability_factors, by = c("limiting_factor_id" = "ID")) |>
    rename(limiting_factor_actual = names) |>
    group_by(ID, categories, class_category, suitability, count) |>
    summarise(limiting_factor_actual = list(pick(limiting_factor_actual)), .groups = 'drop') |>
    unnest_longer(col = class_category) |>
    group_by(ID) |>
    mutate(id_factor = seq_along(class_category)) |>
    left_join(lookup_suitability_factors, by = c("id_factor" = "ID")) |>
    mutate(names = ifelse(class_category %in% "S1", NA, names)) |>
    group_by(ID, categories, suitability, count, limiting_factor_actual) |>
    rename(limiting_factor_potential = names) |>
    summarise(limiting_factor_potential = list(na.omit(pick(limiting_factor_potential))), .groups = 'drop') |>
    mutate(limiting_factor_potential = map2(limiting_factor_actual, limiting_factor_potential, ~ {
      potential_unique <- setdiff(.y$limiting_factor_potential, .x$limiting_factor_actual)
      
      if (length(potential_unique) == 0) {
        return(NA)
      } else {
        return(tibble(limiting_factor_potential = potential_unique))
      }
    })) |>
    mutate(limiting_factor_actual = map(limiting_factor_actual, ~unlist(.x, use.names=FALSE))) |>
    mutate(limiting_factor_potential = map(limiting_factor_potential, ~unlist(.x, use.names=FALSE)))
  
  
  # Step 10: Update Levels of the Suitability Raster
  # Updates the categorical levels of the suitability raster based on the attribute table.
  levels(suitability_raster) <- as.data.frame(suitability_attr)
  terra::activeCat(suitability_raster) <- "ID"
  
  # Step 11: Convert Raster to Polygons and Join with Attribute Table
  # Converts the raster data into polygon format and merges it with the attribute table.
  suitability_polygon <- suitability_raster |>
    as.polygons() |>
    sf::st_as_sf() |>
    left_join(suitability_attr, by = "ID")
  
  # Step 12: Return Results
  # Returns a list containing the processed data in various formats.
  return(list(suitability_raster = suitability_raster,
              suitability_polygon = suitability_polygon,
              suitability_attr = suitability_attr,
              suitability_by_factors = suitability_factors_reclass,
              lookup_suitability_factors = lookup_suitability_factors))
}

#' Create Layer Dataframe from SpatRaster
#'
#' This function takes a `SpatRaster` object and returns a dataframe with two columns:
#' `ID` and `names`. `ID` is a sequence number starting from 1 for each layer in the
#' SpatRaster, and `names` are the names of these layers.
#'
#' @param spatraster A `SpatRaster` object.
#' @importFrom terra names
#'
#' @return A dataframe with two columns: `ID` and `names`.
#'         `ID` is a sequence from 1 to the number of layers in the `SpatRaster` object,
#'         and `names` are the names of the layers in the `SpatRaster` object.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a SpatRaster object named suitability_factors
#' # suitability_factors <- ...
#' dataframe <- create_layer_dataframe(suitability_factors)
#' print(dataframe)
#' }
#'
#' @export
create_layer_dataframe <- function(spatraster) {
  # Extract the names of the layers
  layer_names <- names(spatraster)
  
  # Create a sequence of IDs
  ids <- seq_along(layer_names)
  
  # Combine into a dataframe
  dataframe <- data.frame(ID = ids, names = layer_names)
  
  return(dataframe)
}

#' Print Information about Analyzed Names
#'
#' This function prints the number of objects and their names
#' contained in the input vector. It's useful for quickly viewing
#' the contents and the count of elements in the vector.
#'
#' @param names_analysed A vector of names (character) to be analyzed.
#'
#' @return None. This function is used for its side effect of printing
#' to the console.
#'
#' @examples
#' \dontrun{
#' names_analysed <- c("clim_temperature_avg", "clim_precipitation_tot",
#'                     "clim_humidity", "soil_coarse_fragments", "soil_depth",
#'                     "soil_cec", "soil_base_saturation", "soil_ph",
#'                     "soil_organic_c", "soil_nitrogen", "soil_salinity",
#'                     "soil_esp", "soil_slope")
#' print_names_analysed_info(names_analysed)
#' }
#'
print_names_analysed_info <- function(names_analysed) {
  # Find the number of names
  num_names <- length(names_analysed)
  
  # Create the message
  message <- paste("Number of objects:", num_names, "\nNames of objects:", paste(names_analysed, collapse = ", "))
  
  # Print the message
  print(message)
}

#' Process Suitability Factors for Crop Suitability Analysis
#'
#' This function integrates environmental suitability factors with crop suitability parameters to produce
#' a comprehensive analysis of crop suitability. It involves reclassifying raster layers based on
#' suitability parameters, creating a frequency table, and converting rasters to spatial polygons.
#' This function relies on specific functions from terra, dplyr, tidyr, and sf packages.
#'
#' @param suitability_factors A SpatRaster object from the terra package representing environmental factors
#' affecting crop growth such as soil quality, climate conditions, etc.
#' @param crop_suitability A dataframe with crop suitability parameters, each row representing a different
#' parameter and its associated suitability conditions.
#'
#' @return A list containing four elements:
#' \itemize{
#'   \item{suitability_raster}{A SpatRaster object representing the combined suitability analysis.}
#'   \item{suitability_polygon}{An sf object representing suitability areas as polygons.}
#'   \item{suitability_attr}{A dataframe containing attributes for each suitability category.}
#'   \item{suitability_by_factors}{A list of SpatRaster objects for individual suitability factors.}
#' }
#' @importFrom terra freq cats as.polygons subset activeCat levels
#' @importFrom dplyr select left_join mutate group_by summarise pull filter row_number rowwise rename pick
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom sf st_as_sf
#' @importFrom purrr map map2
#' @importFrom rlang .data
#' @importFrom tibble as_tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming suitability_factors and crop_suitability are already defined
#'   suitability_results <- process_suitability(suitability_factors, crop_suitability)
#'   View(suitability_results$suitability_raster)
#'   View(suitability_results$suitability_polygon)
#'   View(suitability_results$suitability_attr)
#'   # Explore individual factor suitability rasters
#'   lapply(suitability_results$suitability_by_factors, View)
#' }
# Function to Process Suitability Data for Crop Suitability Analysis
process_suitability <- function(suitability_factors, crop_suitability) {
  
  # Step 1: Create a DataFrame for Layer Lookup
  # This step involves creating a DataFrame that maps each layer in the suitability_factors to a name.
  lookup_suitability_factors <- create_layer_dataframe(suitability_factors)
  lookup_suitability_factors_names <- lookup_suitability_factors |> pull(names)
  
  # Step 2: Identify and List Crop Parameters
  # Extracts unique parameter names from the crop_suitability DataFrame.
  crop_suitability_param_names <- crop_suitability |> pull(name_parameter) |> unique()
  
  # Step 3: Identify Names Not in Crop Suitability
  # Finds layer names in lookup_suitability_factors that are not in crop_suitability parameters.
  names_not_in_crop_suitability <- setdiff(lookup_suitability_factors_names, crop_suitability_param_names)
  
  # Step 4: Subset Suitability Factors
  # If there are names not in crop suitability, subset the suitability factors to exclude them.
  if (!is.null(names_not_in_crop_suitability)) {
    suitability_factors <- terra::subset(suitability_factors, names_not_in_crop_suitability, negate = TRUE)
    lookup_suitability_factors <- lookup_suitability_factors |>
      filter(!names %in% names_not_in_crop_suitability) |>
      dplyr::mutate(ID = row_number())
  }
  
  # Step 5: Cross-Check and Print Names Analysed
  # Intersects the names of the suitability factors with the crop suitability parameters and prints them.
  names_analysed <- intersect(names(suitability_factors), crop_suitability_param_names)
  print_names_analysed_info(names_analysed)
  
  # Step 6: Classify Suitability of Each Predictor
  # Applies predefined functions to classify and stack suitability factors.
  suitability_factors_reclass <- classify_and_stack_suitability_factors(
    stacked_raster = suitability_factors,
    suitability_data = crop_suitability)
  
  # Step 7: Combine Suitability Rasters
  # Concatenates the levels of all the suitability rasters into one.
  suitability_raster <- concat_rasters(suitability_factors_reclass)
  
  # Step 8: Create Frequency Table for Suitability Raster
  # Generates a frequency table for the combined suitability raster.
  suitability_raster_freq <- terra::freq(suitability_raster) |> dplyr::select(categories = value, count)
  # Step 9: Extract and Process Attribute Table
  # Processes the attribute table of the suitability raster for further analysis.
  suitability_attr <- terra::levels(suitability_raster)[[1]] |>
    as_tibble() |>
    rename(categories = 2) |>
    left_join(suitability_raster_freq, by = "categories") |>
    mutate(class_category = strsplit(as.character(categories), "_")) |>
    rowwise() |>
    mutate(class = list(determine_suitability(class_category))) |>
    tidyr::unnest_wider(class) |>
    mutate(limiting_factor_id = limiting_factor) |>
    tidyr::unnest_longer(limiting_factor_id, keep_empty = TRUE) |>
    left_join(lookup_suitability_factors, by = c("limiting_factor_id" = "ID")) |>
    rename(limiting_factor_actual = names) |>
    group_by(ID, categories, class_category, suitability, count) |>
    summarise(limiting_factor_actual = list(pick(limiting_factor_actual)), .groups = 'drop') |>
    unnest_longer(col = class_category) |>
    group_by(ID) |>
    mutate(id_factor = seq_along(class_category)) |>
    left_join(lookup_suitability_factors, by = c("id_factor" = "ID")) |>
    mutate(names = ifelse(class_category %in% "S1", NA, names)) |>
    group_by(ID, categories, suitability, count, limiting_factor_actual) |>
    rename(limiting_factor_potential = names) |>
    summarise(limiting_factor_potential = list(na.omit(pick(limiting_factor_potential))), .groups = 'drop') |>
    mutate(limiting_factor_potential = map2(limiting_factor_actual, limiting_factor_potential, ~ {
      potential_unique <- setdiff(.y$limiting_factor_potential, .x$limiting_factor_actual)
      
      if (length(potential_unique) == 0) {
        return(NA)
      } else {
        return(tibble(limiting_factor_potential = potential_unique))
      }
    })) |>
    mutate(limiting_factor_actual = map(limiting_factor_actual, ~unlist(.x, use.names=FALSE))) |>
    mutate(limiting_factor_potential = map(limiting_factor_potential, ~unlist(.x, use.names=FALSE)))
  
  
  # Step 10: Update Levels of the Suitability Raster
  # Updates the categorical levels of the suitability raster based on the attribute table.
  levels(suitability_raster) <- as.data.frame(suitability_attr)
  terra::activeCat(suitability_raster) <- "ID"
  
  # Step 11: Convert Raster to Polygons and Join with Attribute Table
  # Converts the raster data into polygon format and merges it with the attribute table.
  suitability_polygon <- suitability_raster |>
    as.polygons() |>
    sf::st_as_sf() |>
    left_join(suitability_attr, by = "ID")
  
  # Step 12: Return Results
  # Returns a list containing the processed data in various formats.
  return(list(suitability_raster = suitability_raster,
              suitability_polygon = suitability_polygon,
              suitability_attr = suitability_attr,
              suitability_by_factors = suitability_factors_reclass,
              lookup_suitability_factors = lookup_suitability_factors))
}

#' Classify and Stack Suitability Factors
#'
#' This function processes each layer of a stacked `SpatRaster` object
#' using the `classify_suitability_predictors` function and returns a stacked
#' raster with classified layers.
#'
#' @param stacked_raster A stacked `SpatRaster` object.
#' @param suitability_data A data frame containing crop suitability parameters.
#' @return A stacked `SpatRaster` object with classified layers.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have suitability_factors
#'   # as a SpatRaster and crop_suitability as a data frame
#'   stacked_suitability <- classify_and_stack_suitability_factors(
#'                               suitability_factors, crop_suitability)
#' }
#' @importFrom terra rast nlyr
#' @export
classify_and_stack_suitability_factors <- function(stacked_raster, suitability_data) {
  # Apply the classify_suitability_predictors function to each layer using lapply
  
  reclassified_rasters <- lapply(1:nlyr(stacked_raster), function(i) {
    #print(names(stacked_raster[[i]]))
    classify_suitability_predictors(stacked_raster[[i]], suitability_data)
  })
  
  # Stack the reclassified rasters
  stacked_suitability <- rast(reclassified_rasters)
  
  # Return the stacked raster
  return(stacked_suitability)
}

#' Concatenate Raster Layers
#'
#' This function concatenates all raster layers within a `terra` raster object
#' into a single raster object.
#'
#' @param rasters A `terra` raster object containing multiple raster layers.
#'
#' @return A single `terra` raster object containing all concatenated layers.
#'
#' @importFrom terra rast nlyr concats droplevels
#'
#' @export
concat_rasters <- function(rasters) {
  result <- rasters[[1]]
  for (i in 2:nlyr(rasters)) {
    result <- concats(result, rasters[[i]]) |>
      droplevels()
  }
  return(result)
}

#' Classify raster based on crop suitability parameters
#'
#' @param raster_input A SpatRaster object.
#' @param suitability_data A data frame containing crop suitability parameters.
#' @return A reclassified SpatRaster.
#' @examples
#' \dontrun{
#' classified_raster <- classify_suitability_predictors(
#'                       raster_input = clim_temperature_avg,
#'                       suitability_data = crop_suitability)
#' }
#' @importFrom terra classify droplevels
#' @importFrom dplyr filter select mutate case_when
#' @importFrom stringr str_starts str_detect str_extract str_ends str_split
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom tidyr unnest_longer replace_na
#' @importFrom magrittr %>%
#' @export
classify_suitability_predictors <- function(raster_input, suitability_data) {
  # Check if raster_input is of the correct type
  if (!inherits(raster_input, "SpatRaster")) {
    stop("raster_input must be a SpatRaster object")
  }
  
  # Check if suitability_data is of the correct type
  if (!is.data.frame(suitability_data)) {
    stop("suitability_data must be a data.frame")
  }
  
  # Check if the necessary columns exist in suitability_data
  required_columns <- c("value", "class", "name_parameter")
  if (!all(required_columns %in% names(suitability_data))) {
    stop(paste("suitability_data must contain the following columns:", paste(required_columns, collapse = ", ")))
  }
  
  # Check if the name of raster_input exists in the name_parameter column of suitability_data
  if (!(names(raster_input) %in% suitability_data$name_parameter)) {
    stop(paste("The name of raster_input must exist in the name_parameter column of suitability_data. Please check your inputs."))
  }
  # Filter suitability_data to match raster_input
  suitability_data <- suitability_data %>%
    filter(name_parameter %in% names(raster_input))
  
  if (suitability_data[["name_parameter"]][1] == "soil_texture") {
    if (file.exists(system.file("extdata/lookup_tables/lookup_texture_usda.csv", package = "LaSEM"))) {
      texture_lookup <- readr::read_csv(
        system.file(
          "extdata/lookup_tables/lookup_texture_usda.csv",
          package = "LaSEM"
        )
      ) |> select(texture_kemtan, TEXTURE_USDA)
      
    } else if (file.exists("inst/extdata/lookup_tables/lookup_texture_usda.csv")) {
      texture_lookup <- readr::read_csv("inst/extdata/lookup_tables/lookup_texture_usda.csv")
    } else {
      errorCondition("texture_lookup table is not found")
    }
    
    # Apply the mapping function to each element in the value list
    suitability_data <- suitability_data %>%
      mutate(value = map(value, ~ str_split(.x, pattern = "_", simplify = TRUE))) %>%
      mutate(value = map(value, ~ map(.x, map_texture_code, lookup_table = texture_lookup))) |>
      mutate(value = map(value, unlist)) |>
      mutate(class = factor(class, levels = c("S1", "S2", "S3", "N")))
    
    reclass_matrix <- suitability_data |>
      select(value, class) |>
      unnest_longer(value) |>
      mutate(class = as.numeric(class)) |>
      as.matrix()
    
  } else {
    
    # Pre-process the suitability data frame
    suppressWarnings({
      suitability_data <- suitability_data %>%
        mutate(
          lower = case_when(
            str_starts(value, ">") ~ as.numeric(str_extract(value, "\\d+\\.?\\d*$")),
            str_detect(value, "\\d+-\\d+") ~ as.numeric(str_extract(value, "^\\d+\\.?\\d*")),
            TRUE ~ NA_real_
          ),
          upper = ifelse(str_ends(value, "<"), as.numeric(str_extract(
            value, "\\d+\\.?\\d*"
          )), NA)
        ) %>%
        mutate(
          upper = ifelse(is.na(upper), ifelse(
            str_starts(value, ">"), Inf, as.numeric(str_extract(value, "\\d+\\.?\\d*$"))
          ), upper),
          lower = replace_na(lower,-Inf),
          class = factor(class, levels = c("S1", "S2", "S3", "N"))
        )
      
    })
    
    
    
    # Define the reclassification matrix
    reclass_matrix <- suitability_data %>%
      dplyr::select(lower, upper, class) %>%
      mutate(class = as.numeric(class)) %>%
      as.matrix()
  }
  
  rast_name <- names(raster_input)
  
  # Reclassify the raster
  r_reclassified <- terra::classify(raster_input, rcl = reclass_matrix,
                                    include.lowest = TRUE)
  
  # Create the lookup data frame
  lookup_df <- data.frame(
    class = 1:4,
    level = c("S1", "S2", "S3", "N")
  )
  
  # Reassign raster values to predefined levels
  levels(r_reclassified)<- lookup_df
  names(r_reclassified) <- rast_name
  r_reclassified<- terra::droplevels(r_reclassified)
  
  return(r_reclassified)
}



soil_texture <- tibble::tribble(
  ~TEXTURE_USDA, ~TEXTURE_USDA_OLD, ~description_eng, ~description_idn, ~code_id, ~texture_kemtan, ~Color,
  1, 1, "clay (heavy)", "liat", "C", "sh", "#d5c36b",
  2, 2, "silty clay", "liat berdebu", "SC", "h", "#b96947",
  5, 4, "silty clay loam", "lempung liat berpasir", "SCL", "ah", "#9d3706",
  4, 5, "clay loam", "lempung berliat", "CL", "ah", "#ae868f",
  10, 6, "silt", "debu", "Si", "s", "#f86714",
  8, 7, "silty loam", "lempung berdebu", "SiL", "s", "#46d143",
  3, 8, "sandy clay", "liat berpasir", "SC", "h", "#368f20",
  7, 9, "loam", "lempung", "L", "s", "#3e5a14",
  6, 10, "sandy clay loam", "lempung liat berpasir", "SCL", "ah", "#ffd557",
  9, 11, "sandy loam", "lempung berpasir", "SL", "ak", "#fff72e",
  11, 12, "loamy sand", "pasir berlempung", "LS", "k", "#ff5a9d",
  12, 13, "sand", "pasir", "S", "k", "#ff005b"
)

#' Determine Suitability and Limiting Factors
#'
#' This function evaluates a vector of suitability classes and determines the
#' lowest suitability class based on a predefined priority order. It also
#' identifies the positions of this class within the vector.
#'
#' @param class_vector A vector of characters representing suitability classes.
#'        The classes are expected to be "S1", "S2", "S3", or "N", with "N" being
#'        the lowest and "S1" the highest suitability class.
#'
#' @return A list containing two elements: 'suitability', which is the lowest
#'         suitability class found in the input vector, and 'limiting_factor',
#'         which is a vector of positions where this class occurs in the input.
#'         If the highest class is "S1", 'limiting_factor' is set to NA.
#'
#' @examples
#' determine_suitability(c("S1", "S1", "S3", "S3", "S2"))
#'
#' @importFrom purrr map_chr
#' @importFrom stats na.omit
#' @export
determine_suitability <- function(class_vector) {
  # Define the order of priority for the suitability classes
  priority_order <- c("N", "S3", "S2", "S1")
  
  # Determine the highest priority class present in the vector
  lowest_class <- priority_order |>
    purrr::map_chr(~ifelse(any(class_vector == .x), .x, NA_character_)) |>
    na.omit() |>
    (\(.) .[1])()
  
  # Determine the positions of the highest priority class
  limiting_factor <- which(class_vector == lowest_class)
  
  # If the highest class is "S1", set limiting_factor to NA
  if (lowest_class == "S1") {
    limiting_factor <- NULL
  }
  
  # Return a list containing suitability and limiting factors
  return(list(suitability = lowest_class, limiting_factor = limiting_factor))
}


#' Calculate Suitability Potential Table
#'
#' This function reads a CSV file for lookup intervention data and processes it along with a given suitability attribute dataframe to calculate the suitability potential table.
#'
#' @param lookup_intervention A tibble containing lookup intervention data.
#' @param intervention_level A string indicating the level of intervention.
#' @param suitability_attr A dataframe containing suitability attributes with columns name_parameter, ID, intervention, limiting_factor_actual, and suitability.
#' @param lookup_suitability_layer A dataframe with two columns: `ID` and `names`.
#'         `ID` is a sequence from 1 to the number of layers in the `SpatRaster` object,
#'         and `names` are the names of the layers in the `SpatRaster` object.
#'
#' @return A tibble containing the calculated suitability potential table.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate group_by ungroup summarise rename_with rename case_when
#' @importFrom tidyr unnest_longer
#' @importFrom stringr str_count fixed
#' @importFrom tidyselect all_of
#' @importFrom tibble is_tibble
#'
#' @examples
#' \dontrun{
#'   path <- "data/lookup_tables/lookup_intervention.csv"
#'   level <- "high"
#'   attr <- readr::read_csv("output/kesesuaian_jagung_aktual.csv")
#'   calculate_suitability_potential_table(path, level, attr)
#' }
calculate_suitability_potential_table <- function(lookup_intervention, intervention_level, suitability_attr, lookup_suitability_layer) {
  
  
  if (!is.data.frame(lookup_intervention)) {
    stop("File specified in lookup_intervention is not a data frame.")
  }
  
  if (!is.character(intervention_level) || length(intervention_level) != 1) {
    stop("intervention_level must be a single object string.")
  }
  
  if (!is_tibble(suitability_attr) ||
      !all(c( "ID",  "limiting_factor_actual", "suitability") %in% names(suitability_attr))) {
    stop("suitability_attr must be a data frame with specific columns.")
  }
  
  # Reading and processing the lookup intervention data
  lookup_intervention <- lookup_intervention |>
    select(-c("no", "karakteristik_lahan")) |>
    mutate(
      low = ifelse(is.na(low), 0, str_count(low, pattern = fixed("+"))),
      med = ifelse(is.na(med), 0, str_count(med, pattern = fixed("+"))),
      high = ifelse(is.na(high), 0, str_count(high, pattern = fixed("+")))
    ) |>
    group_by(name_parameter, intervention)
  
  # Filtering and renaming
  lookup_intervention_filtered <- lookup_intervention |>
    dplyr::select(all_of(intervention_level)) |>
    ungroup() |>
    rename(dummy_intervention = intervention_level)
  # Creating the suitability potential table
  # Add a check if maximum is higher or equal with the list of uncontrolled limiting factor then update limiting_factor_name, also update the intervention_potential
  suitability_potential_table <- suitability_attr |>
    mutate(class_category = strsplit(as.character(categories), "_")) |>
    dplyr::select(all_of(c("ID", "suitability", "class_category" ))) |> # take it from class_category instead, do not use limiting factor name, then unnest longer class_category, join with the lookup_suitability_factors, the rest are fine
    #mutate(limiting_factor_name = str_split(limiting_factor_name, ", ")) |>
    unnest_longer(col = class_category) |>
    group_by(ID) |>
    mutate(id_factor = seq_along(class_category)) |>
    left_join(lookup_suitability_layer, by = c("id_factor" = "ID")) |>
    left_join(lookup_intervention_filtered, by = c("names" = "name_parameter")) |>
    mutate(
      dummy_suitability = case_when(
        class_category == "S1" ~ 1,
        class_category == "S2" ~ 2,
        class_category == "S3" ~ 3,
        class_category == "N"  ~ 4,
        TRUE ~ NA_real_
      )
    ) |>
    filter(!suitability %in% "S1") |>
    filter(!class_category  %in% "S1") |>
    mutate(
      dummy_suitability_pot_id = dummy_suitability - dummy_intervention,
      dummy_suitability_pot_id = case_when(dummy_suitability_pot_id < 1 ~ 1, TRUE ~ dummy_suitability_pot_id)
    ) |>
    group_by(ID, suitability) |>
    summarise(
      dummy_suitability_potential = max(dummy_suitability_pot_id, na.rm = FALSE),
      #limiting_factors_complete =  list(pick(names)),
      # intervention_potential = case_when(
      #   all(intervention == TRUE) ~ "all",
      #   any(intervention == TRUE) & any(intervention == FALSE) ~ "partial",
      #   !any(intervention == TRUE) ~ "none"
      # ),
      .groups = "drop") |>
    mutate(
      suitability_potential = paste0("S", dummy_suitability_potential),
      suitability_potential = case_when(suitability_potential %in% "S4" ~ "N", TRUE ~ suitability_potential),
      suitability_potential = case_when(suitability_potential %in% "SNA" ~ suitability, TRUE ~ suitability_potential)
    ) |>
    dplyr::select(-contains("dummy"), -suitability) |>
    rename_with(~ paste(., intervention_level, sep = "_"), -c("ID"))
  
  return(suitability_potential_table)
}

#' Map Texture Code to TEXTURE_USDA Value
#'
#' This function maps a texture code to its corresponding TEXTURE_USDA value using a lookup table.
#'
#' @param texture_code A character string representing the texture code.
#' @param lookup_table A data frame containing the mapping between texture codes and TEXTURE_USDA values.
#'
#' @return A character string representing the TEXTURE_USDA value corresponding to the input texture code.
#'         If the texture code is not found in the lookup table, the function returns NA.
#'
#' @importFrom dplyr filter pull
#'
#' @examples
#' lookup_table <- data.frame(
#'   texture_kemtan = c("1", "2", "3"),
#'   TEXTURE_USDA = c("Sandy", "Loamy", "Clayey")
#' )
#'
#' map_texture_code("1", lookup_table)  # Returns "Sandy"
#' map_texture_code("4", lookup_table)  # Returns NA
#'
#' @export
map_texture_code <- function(texture_code, lookup_table) {
  texture_usda <- lookup_table %>%
    filter(texture_kemtan == texture_code) %>%
    pull(TEXTURE_USDA)
  
  if (length(texture_usda) == 0) {
    return(NA)
  }
  
  return(texture_usda)
}

#' Stack Raster Layers
#'
#' This function stacks a list of raster layers and sets the variable names based on the corresponding parameter names.
#'
#' @param raster_list A list of SpatRaster objects to be stacked.
#' @param parameter_names A vector of parameter names corresponding to each raster layer in the list.
#'
#' @return A SpatRaster object representing the stacked raster layers with variable names set.
#' @importFrom terra rast names varnames
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming raster_list and parameter_names are predefined
#' stacked_rasters <- stack_raster_layers(raster_list, parameter_names)
#' }
stack_raster_layers <- function(raster_list, parameter_names) {
  # Check if raster_list is a list and parameter_names is a vector of the same length
  if (!is.list(raster_list) || !is.vector(parameter_names) || length(raster_list) != length(parameter_names)) {
    stop("raster_list must be a list and parameter_names must be a vector of the same length as raster_list")
  }
  
  # Set names of raster objects based on parameter names
  named_raster_list <- setNames(raster_list, parameter_names)
  
  # Stack the raster layers
  stacked_raster_factors <- rast(named_raster_list)
  
  # Set variable names of the stacked raster object
  #varnames(stacked_raster_factors) <- names(stacked_raster_factors)
  
  return(stacked_raster_factors)
}


#' Read Raster Files
#'
#' This function reads raster files from the provided file paths and returns a list of raster objects.
#' If a file is not found, it attempts to load the raster from the 'ALSA' package.
#'
#' @param climate_soil_data A tibble or data frame containing the paths to raster files (`raster_path`).
#'
#' @return A list of SpatRaster objects representing the loaded raster files.
#' @importFrom terra rast
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming climate_soil_data is predefined
#' raster_objects <- read_raster_files(climate_soil_data)
#' }
read_raster_files <- function(climate_soil_data) {
  # Check if climate_soil_data is a data frame and has the 'raster_path' column
  if (!is.data.frame(climate_soil_data) || !("raster_path" %in% names(climate_soil_data))) {
    stop("climate_soil_data must be a data frame with a 'raster_path' column")
  }
  
  # Load raster objects from file paths
  climate_soil_data <- climate_soil_data %>%
    mutate(raster_object = map(raster_path, ~ {
      if (file.exists(.x)) {
        rast(.x)
      } else {
        message(paste("File not found:", .x, "Trying to load from package 'ALSA'"))
        rast(system.file(.x, package = "ALSA"))
      }
    }))
  
  # Return the list of raster objects
  return(pull(climate_soil_data, raster_object))
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
  locale_info <- strsplit(si[["locale"]], ";")[[1]]
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


