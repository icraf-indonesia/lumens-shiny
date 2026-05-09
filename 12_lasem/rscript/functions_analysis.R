# Analysis Functions for LaSEM
# Core suitability analysis pipeline functions

#' Prepare Suitability Inputs
#'
#' Creates a lookup table from raster layers and subsets rasters to match
#' crop suitability parameters.
#'
#' @param suitability_factors A SpatRaster object.
#' @param crop_suitability A data frame with crop suitability parameters.
#'
#' @return A list with:
#'   - suitability_factors: Subset SpatRaster
#'   - lookup_suitability_factors: Lookup tibble with ID and names
#'
#' @importFrom terra names nlyr subset
#' @importFrom dplyr filter mutate pull row_number
#' @export
prepare_suitability_inputs <- function(suitability_factors, crop_suitability) {
  # Create lookup table for raster layers
  lookup_suitability_factors <- create_layer_dataframe(suitability_factors)
  lookup_names <- lookup_suitability_factors |> pull(names)

  # Extract unique parameter names from crop suitability
  crop_param_names <- crop_suitability |>
    pull(name_parameter) |>
    unique()

  # Find names in lookup that are not in crop parameters
  names_not_in_crop <- setdiff(lookup_names, crop_param_names)
  names_in_crop <- intersect(lookup_names, crop_param_names)

  # Validate that at least one parameter matches before subsetting
  if (length(names_in_crop) == 0) {
    stop("No matching parameters found between rasters and crop suitability data.")
  }

  # Subset rasters to exclude unmatched layers
  if (length(names_not_in_crop) > 0) {
    suitability_factors <- terra::subset(
      suitability_factors,
      names_not_in_crop,
      negate = TRUE
    )
    lookup_suitability_factors <- lookup_suitability_factors |>
      filter(!names %in% names_not_in_crop) |>
      mutate(ID = row_number())
  }

  list(
    suitability_factors = suitability_factors,
    lookup_suitability_factors = lookup_suitability_factors
  )
}

#' Validate Parameter Coverage
#'
#' Checks that raster layer names overlap with crop suitability parameters.
#' Messages the count and names of matching parameters.
#'
#' @param suitability_factors A SpatRaster object.
#' @param crop_suitability A data frame with crop suitability parameters.
#'
#' @return Character vector of matched parameter names.
#'
#' @importFrom terra names
#' @importFrom dplyr pull
#' @export
validate_parameter_coverage <- function(suitability_factors, crop_suitability) {
  crop_param_names <- crop_suitability |>
    pull(name_parameter) |>
    unique()

  names_analysed <- intersect(names(suitability_factors), crop_param_names)

  if (length(names_analysed) == 0) {
    stop("No matching parameters found between rasters and crop suitability data.")
  }

  message(
    "Number of objects: ", length(names_analysed),
    "\nNames of objects: ", paste(names_analysed, collapse = ", ")
  )

  names_analysed
}

#' Build Suitability Raster
#'
#' Classifies each factor, concatenates them, and computes frequency table.
#'
#' @param suitability_factors A SpatRaster object.
#' @param crop_suitability A data frame with crop suitability parameters.
#'
#' @return A list with:
#'   - suitability_raster: Concatenated classified raster
#'   - suitability_raster_freq: Frequency table
#'   - suitability_by_factors: Individual factor classifications
#'
#' @importFrom terra freq
#' @importFrom dplyr select
#' @export
build_suitability_raster <- function(suitability_factors, crop_suitability) {
  # Step 6: Classify each factor
  suitability_factors_reclass <- classify_and_stack_suitability_factors(
    stacked_raster = suitability_factors,
    suitability_data = crop_suitability
  )

  # Step 7: Concatenate all classified rasters
  suitability_raster <- concat_rasters(suitability_factors_reclass)

  # Step 8: Create frequency table
  suitability_raster_freq <- terra::freq(suitability_raster) |>
    dplyr::select(categories = value, count)

  list(
    suitability_raster = suitability_raster,
    suitability_raster_freq = suitability_raster_freq,
    suitability_by_factors = suitability_factors_reclass
  )
}

# Helper functions copied from original LaSEM_functions.R
# TODO: Refactor style in Phase 2

classify_and_stack_suitability_factors <- function(stacked_raster, suitability_data) {
  reclassified_rasters <- lapply(seq_len(terra::nlyr(stacked_raster)), function(i) {
    classify_suitability_predictors(stacked_raster[[i]], suitability_data)
  })
  terra::rast(reclassified_rasters)
}

concat_rasters <- function(rasters) {
  result <- rasters[[1]]
  for (i in seq_len(terra::nlyr(rasters))) {
    result <- terra::concats(result, rasters[[i]]) |>
      terra::droplevels()
  }
  result
}

classify_suitability_predictors <- function(raster_input, suitability_data) {
  if (!inherits(raster_input, "SpatRaster")) {
    stop("raster_input must be a SpatRaster object")
  }
  if (!is.data.frame(suitability_data)) {
    stop("suitability_data must be a data.frame")
  }

  required_columns <- c("value", "class", "name_parameter")
  if (!all(required_columns %in% names(suitability_data))) {
    stop(paste(
      "suitability_data must contain the following columns:",
      paste(required_columns, collapse = ", ")
    ))
  }

  if (!(names(raster_input) %in% suitability_data$name_parameter)) {
    stop("The name of raster_input must exist in the name_parameter column of suitability_data.")
  }

  suitability_data <- suitability_data |>
    dplyr::filter(name_parameter %in% names(raster_input))

  if (suitability_data[["name_parameter"]][1] == "soil_texture") {
    if (file.exists(system.file("extdata/lookup_tables/lookup_texture_usda.csv",
      package = "LaSEM"
    ))) {
      texture_lookup <- readr::read_csv(
        system.file("extdata/lookup_tables/lookup_texture_usda.csv",
          package = "LaSEM"
        )
      ) |> dplyr::select(texture_kemtan, TEXTURE_USDA)
    } else {
      texture_lookup <- tibble::tibble(
        texture_kemtan = c("sh", "h", "ah", "ah", "s", "s", "h", "s", "ah", "ak", "k", "k"),
        TEXTURE_USDA = c(1, 2, 5, 4, 10, 8, 3, 7, 6, 9, 11, 12)
      )
    }

    suitability_data <- suitability_data |>
      dplyr::mutate(value = purrr::map(value, ~ stringr::str_split(.x, pattern = "_", simplify = TRUE))) |>
      dplyr::mutate(value = purrr::map(value, ~ purrr::map(.x, map_texture_code, lookup_table = texture_lookup))) |>
      dplyr::mutate(value = purrr::map(value, unlist)) |>
      dplyr::mutate(class = factor(class, levels = c("S1", "S2", "S3", "N")))

    reclass_matrix <- suitability_data |>
      dplyr::select(value, class) |>
      tidyr::unnest_longer(value) |>
      dplyr::mutate(class = as.numeric(class)) |>
      as.matrix()
  } else {
    suppressWarnings({
      suitability_data <- suitability_data |>
        dplyr::mutate(
          lower = dplyr::case_when(
            stringr::str_starts(value, ">") ~ as.numeric(stringr::str_extract(value, "\\d+\\.?\\d*$")),
            stringr::str_detect(value, "\\d+-\\d+") ~ as.numeric(stringr::str_extract(value, "^\\d+\\.?\\d*")),
            TRUE ~ NA_real_
          ),
          upper = ifelse(stringr::str_ends(value, "<"), as.numeric(stringr::str_extract(
            value, "\\d+\\.?\\d*"
          )), NA)
        ) |>
        dplyr::mutate(
          upper = ifelse(is.na(upper), ifelse(
            stringr::str_starts(value, ">"), Inf, as.numeric(stringr::str_extract(value, "\\d+\\.?\\d*$"))
          ), upper),
          lower = tidyr::replace_na(lower, -Inf),
          class = factor(class, levels = c("S1", "S2", "S3", "N"))
        )
    })

    reclass_matrix <- suitability_data |>
      dplyr::select(lower, upper, class) |>
      dplyr::mutate(class = as.numeric(class)) |>
      as.matrix()
  }

  rast_name <- names(raster_input)

  r_reclassified <- terra::classify(raster_input,
    rcl = reclass_matrix,
    include.lowest = TRUE
  )

  lookup_df <- data.frame(
    class = seq_len(4),
    level = c("S1", "S2", "S3", "N")
  )

  levels(r_reclassified) <- lookup_df
  names(r_reclassified) <- rast_name
  terra::droplevels(r_reclassified)
}

map_texture_code <- function(texture_code, lookup_table) {
  texture_usda <- lookup_table |>
    dplyr::filter(texture_kemtan == texture_code) |>
    dplyr::pull(TEXTURE_USDA)

  if (length(texture_usda) == 0) {
    return(NA)
  }

  texture_usda
}

determine_suitability <- function(class_vector) {
  priority_order <- c("N", "S3", "S2", "S1")

  lowest_class <- priority_order |>
    purrr::map_chr(~ ifelse(any(class_vector == .x), .x, NA_character_)) |>
    stats::na.omit() |>
    (\(.) .[1])()

  limiting_factor <- which(class_vector == lowest_class)

  if (lowest_class == "S1") {
    limiting_factor <- NULL
  }

  list(suitability = lowest_class, limiting_factor = limiting_factor)
}

#' Build Suitability Attributes
#'
#' Processes the attribute table of the suitability raster, resolving
#' limiting factors and potential improvements.
#'
#' @param suitability_raster A classified SpatRaster.
#' @param freq Frequency table from terra::freq().
#' @param lookup Lookup tibble with ID and names.
#'
#' @return A tibble with attributes for each suitability category.
#'
#' @importFrom terra levels
#' @importFrom dplyr left_join mutate rowwise rename group_by summarise pick
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom purrr map map2
#' @importFrom tibble as_tibble tibble
#' @export
build_suitability_attributes <- function(suitability_raster, freq, lookup) {
  suitability_attr <- terra::levels(suitability_raster)[[1]] |>
    as_tibble() |>
    rename(categories = 2) |>
    left_join(freq, by = "categories") |>
    mutate(class_category = strsplit(as.character(categories), "_")) |>
    rowwise() |>
    mutate(class = list(determine_suitability(class_category))) |>
    tidyr::unnest_wider(class) |>
    mutate(limiting_factor_id = limiting_factor) |>
    tidyr::unnest_longer(limiting_factor_id, keep_empty = TRUE) |>
    left_join(lookup, by = c("limiting_factor_id" = "ID")) |>
    rename(limiting_factor_actual = names) |>
    group_by(ID, categories, class_category, suitability, count) |>
    summarise(limiting_factor_actual = list(pick(limiting_factor_actual)), .groups = "drop") |>
    tidyr::unnest_longer(col = class_category) |>
    group_by(ID) |>
    mutate(id_factor = seq_along(class_category)) |>
    left_join(lookup, by = c("id_factor" = "ID")) |>
    mutate(names = ifelse(class_category %in% "S1", NA, names)) |>
    group_by(ID, categories, suitability, count, limiting_factor_actual) |>
    rename(limiting_factor_potential = names) |>
    summarise(limiting_factor_potential = list(na.omit(pick(limiting_factor_potential))), .groups = "drop") |>
    mutate(limiting_factor_potential = map2(limiting_factor_actual, limiting_factor_potential, ~ {
      potential_unique <- setdiff(.y$limiting_factor_potential, .x$limiting_factor_actual)
      if (length(potential_unique) == 0) {
        return(NA)
      } else {
        return(tibble(limiting_factor_potential = potential_unique))
      }
    })) |>
    mutate(limiting_factor_actual = map(limiting_factor_actual, ~ unlist(.x, use.names = FALSE))) |>
    mutate(limiting_factor_potential = map(limiting_factor_potential, ~ unlist(.x, use.names = FALSE)))

  suitability_attr
}

#' Format Suitability Outputs
#'
#' Attaches levels to raster, converts to polygons, and formats the final output list.
#'
#' @param suitability_raster A classified SpatRaster.
#' @param suitability_attr Attribute tibble.
#' @param by_factors Individual factor classifications SpatRaster.
#' @param lookup Lookup tibble.
#'
#' @return A list with 5 components matching the original process_suitability() output.
#'
#' @importFrom terra levels activeCat as.polygons
#' @importFrom sf st_as_sf
#' @importFrom dplyr left_join
#' @export
format_suitability_outputs <- function(suitability_raster, suitability_attr,
                                       by_factors, lookup) {
  levels(suitability_raster) <- as.data.frame(suitability_attr)
  terra::activeCat(suitability_raster) <- "ID"

  suitability_polygon <- suitability_raster |>
    terra::as.polygons() |>
    sf::st_as_sf() |>
    dplyr::left_join(suitability_attr, by = "ID")

  list(
    suitability_raster = suitability_raster,
    suitability_polygon = suitability_polygon,
    suitability_attr = suitability_attr,
    suitability_by_factors = by_factors,
    lookup_suitability_factors = lookup
  )
}

#' Process Suitability Data for Crop Suitability Analysis
#'
#' Orchestrates the full suitability analysis pipeline by calling
#' the decomposed step functions.
#'
#' @param suitability_factors A SpatRaster object.
#' @param crop_suitability A data frame with crop suitability parameters.
#'
#' @return A list containing:
#'   \item{suitability_raster}{A SpatRaster object.}
#'   \item{suitability_polygon}{An sf object.}
#'   \item{suitability_attr}{A dataframe with attributes.}
#'   \item{suitability_by_factors}{A SpatRaster with individual factor classifications.}
#'   \item{lookup_suitability_factors}{A lookup tibble.}
#'
#' @export
process_suitability <- function(suitability_factors, crop_suitability) {
  # Step 1-4: Prepare inputs
  prepared <- prepare_suitability_inputs(suitability_factors, crop_suitability)

  # Step 5: Validate coverage
  validate_parameter_coverage(prepared$suitability_factors, crop_suitability)

  # Step 6-8: Build suitability raster
  built_raster <- build_suitability_raster(
    prepared$suitability_factors,
    crop_suitability
  )

  # Step 9: Build attributes
  attr <- build_suitability_attributes(
    built_raster$suitability_raster,
    built_raster$suitability_raster_freq,
    prepared$lookup_suitability_factors
  )

  # Step 10-12: Format outputs
  format_suitability_outputs(
    built_raster$suitability_raster,
    attr,
    built_raster$suitability_by_factors,
    prepared$lookup_suitability_factors
  )
}
