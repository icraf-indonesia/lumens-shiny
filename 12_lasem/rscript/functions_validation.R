# Validation Functions for LaSEM
# Input validation and quality checks

#' Validate CSV Schema
#'
#' Checks that a data frame contains required columns with correct data types.
#'
#' @param data A data frame or tibble to validate.
#' @param required_cols Character vector of required column names.
#'
#' @return A list with:
#'   - valid: Logical indicating if validation passed.
#'   - errors: A tibble with error messages (empty if valid).
#'
#' @importFrom dplyr tibble
#' @export
validate_csv_schema <- function(data, required_cols) {
  errors <- dplyr::tibble(field = character(), message = character())

  # Check missing columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    errors <- dplyr::tibble(
      field = "columns",
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
    return(list(valid = FALSE, errors = errors))
  }

  # Check data types
  # ID should be numeric
  if ("ID" %in% names(data) && !is.numeric(data$ID)) {
    errors <- dplyr::tibble(
      field = "ID",
      message = "Column 'ID' should be numeric."
    )
    return(list(valid = FALSE, errors = errors))
  }

  # availability should be character
  if ("availability" %in% names(data) && !is.character(data$availability)) {
    errors <- dplyr::tibble(
      field = "availability",
      message = "Column 'availability' should be character (Yes/No)."
    )
    return(list(valid = FALSE, errors = errors))
  }

  list(valid = TRUE, errors = errors)
}

#' Validate Raster Geometry
#'
#' Checks that all rasters in a list have consistent CRS, extent, and resolution.
#'
#' @param raster_list A list of SpatRaster objects.
#'
#' @return A list with:
#'   - valid: Logical indicating if all rasters are compatible.
#'   - errors: A tibble with error messages (empty if valid).
#'   - details: A tibble with CRS and extent info for each raster.
#'
#' @importFrom terra crs ext res compareGeom
#' @importFrom dplyr tibble bind_rows
#' @export
validate_raster_geometry <- function(raster_list) {
  errors <- dplyr::tibble(field = character(), message = character())

  if (length(raster_list) < 2) {
    return(list(valid = TRUE, errors = errors, details = NULL))
  }

  # Extract metadata for each raster
  details <- lapply(seq_along(raster_list), function(i) {
    r <- raster_list[[i]]
    dplyr::tibble(
      index = i,
      name = names(r),
      crs = as.character(terra::crs(r)),
      xmin = terra::ext(r)[1],
      xmax = terra::ext(r)[2],
      ymin = terra::ext(r)[3],
      ymax = terra::ext(r)[4],
      xres = terra::res(r)[1],
      yres = terra::res(r)[2]
    )
  }) |> dplyr::bind_rows()

  # Check CRS consistency
  unique_crs <- unique(details$crs)
  if (length(unique_crs) > 1) {
    errors <- dplyr::tibble(
      field = "CRS",
      message = paste(
        "Inconsistent CRS found:",
        paste(unique_crs, collapse = "; ")
      )
    )
    return(list(valid = FALSE, errors = errors, details = details))
  }

  # Check extent consistency
  unique_extents <- details |>
    dplyr::distinct(xmin, xmax, ymin, ymax)

  if (nrow(unique_extents) > 1) {
    errors <- dplyr::tibble(
      field = "extent",
      message = "Inconsistent raster extents. All rasters must have the same spatial extent."
    )
    return(list(valid = FALSE, errors = errors, details = details))
  }

  # Use terra::compareGeom for thorough check
  comparison_result <- tryCatch({
    do.call(terra::compareGeom, c(raster_list, list(stopOnError = FALSE)))
  }, error = function(e) {
    FALSE
  })

  if (!isTRUE(comparison_result)) {
    errors <- dplyr::tibble(
      field = "geometry",
      message = "Raster geometries are not compatible. Check resolution, extent, and CRS."
    )
    return(list(valid = FALSE, errors = errors, details = details))
  }

  list(valid = TRUE, errors = errors, details = details)
}
