# I/O Functions for LaSEM
# Data loading and file handling utilities

#' Create Layer Dataframe from SpatRaster
#'
#' Creates a dataframe mapping each layer in a SpatRaster to an ID and name.
#'
#' @param spatraster A SpatRaster object.
#'
#' @return A dataframe with columns `ID` and `names`.
#'
#' @importFrom terra names
#' @export
create_layer_dataframe <- function(spatraster) {
  layer_names <- names(spatraster)
  ids <- seq_along(layer_names)
  data.frame(ID = ids, names = layer_names)
}
