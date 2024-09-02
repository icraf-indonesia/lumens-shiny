# Pre-QuES Land cover change analysis -------------------------------------

#' @title Pre-QuES Land cover change analysis
#' @description This function preprocesses land cover data for visualization, calculation, and summarization.
#' It checks the consistency of input data, creates visualizations of input data, calculates land cover frequency,
#' creates a crosstabulation of land cover changes, and summarizes results at both the landscape and planning unit level.
#' The function also converts pixels to hectares if `convert_to_Ha` is set to TRUE.
#' @param lc_t1 A spatRaster object representing land cover data for period T1.
#' @param lc_t2 A spatRaster object representing land cover data for period T2.
#' @param admin_ A spatRaster object representing planning unit data. Can also be a sf multipolygon object,
#' in which case it will be rasterized.
#' @param cutoff_landscape minimum number of pixel/ area to be displayed in sankey plot at landscape level
#' @param cutoff_pu minimum number of pixel/ area to be displayed in sankey plot at planing unit level
#' @param convert_to_Ha Logical flag indicating whether to convert pixel count to hectares based on the resolution of the `lc_t1` raster.
#' @return A list of results containing input data visualizations, landscape level results, and planning unit level results.
#' @importFrom terra rast cats compareGeom
#' @importFrom dplyr select group_by_at summarise pull
#' @importFrom purrr map
#' @importFrom methods is
#' @importFrom stats setNames
#' @export
#' @examples
#' \dontrun{
#' # Load and annotate land cover data for period T1
#' lc_t1_ <- LUMENSR_example("NTT_LC90.tif") %>%
#'   terra::rast() %>%
#'   add_legend_to_categorical_raster(
#'     raster_file = .,
#'     lookup_table = lc_lookup_klhk_sequence,
#'     year = 1990)
#'
#' # Load and annotate land cover data for period T2
# lc_t2_ <- LUMENSR_example("NTT_LC20.tif") %>%
#'   terra::rast() %>%
#'   add_legend_to_categorical_raster(
#'     raster_file = .,
#'     lookup_table = lc_lookup_klhk_sequence,
#'    year = 2020)
#'
#' # Load planning unit data
#' admin_z <- LUMENSR_example("ntt_admin_spatraster.tif") %>%
#'   terra::rast()
#' ques_pre(lc_t1_, lc_t2_, admin_z)
#' }

ques_pre <- function(lc_t1, lc_t2, admin_, cutoff_landscape = 5000, cutoff_pu = 500, convert_to_Ha = TRUE) {

  ## Plot planning unit
  if (!is(admin_, "SpatRaster")) {
    stopifnot(is(admin_, "sf")) # Ensure admin_ is either SpatRaster or sf (multipolygon)
    plot_admin <- plot_planning_unit(admin_) #%>% ggplot_to_image(image_width = 20, image_height = 14)
    admin_ <- rasterise_multipolygon(admin_) # convert admin_ to a spatraster
  } else {

    plot_admin <- plot_categorical_raster(admin_) #%>% ggplot_to_image(image_width = 20, image_height = 14)

  }


  # Guardrails to check the input types
  stopifnot(is(lc_t1, "SpatRaster"), is(lc_t2, "SpatRaster"))

  # Guardrails to check if the input rasters have attribute tables
  stopifnot(!is.null(cats(lc_t1)[[1]]), !is.null(cats(lc_t2)[[1]]))

  # Guardrail to ensure identical extent and projection system
  compareGeom(lc_t1, lc_t2, admin_, stopOnError = TRUE)

  # Plot land cover for both periods and the planning unit
  ## Plot land cover T1
  plot_lc_t1 <- plot_categorical_raster(lc_t1) #%>% ggplot_to_image(image_width = 20, image_height = 14)
  ## Plot land cover T2
  plot_lc_t2 <- plot_categorical_raster(lc_t2) #%>% ggplot_to_image(image_width = 20, image_height = 14)

  # Calculate and tabulate land cover composition
  lc_freq_table <- calc_lc_freq(raster_list = list(lc_t1, lc_t2)) %>%
    abbreviate_by_column( "Jenis tutupan lahan", remove_vowels = FALSE)
  lc_composition_tbl <- lc_freq_table #%>%
  #rmarkdown::paged_table(options = list(cols.min.print = 3))

  # Plot land cover composition
  lc_composition_barplot <- lc_freq_table %>%
    plot_lc_freq(column_lc_type = "Jenis tutupan lahan",
                 column_T1 = names(lc_freq_table)[2],
                 column_T2 = names(lc_freq_table)[3])

  # Store visualization results
  input_dataviz <- list(
    plot_lc_t1 = plot_lc_t1,
    plot_lc_t2 = plot_lc_t2,
    plot_admin = plot_admin,
    tbl_lookup_lc_t1 = cats(lc_t1)[[1]],
    tbl_lookup_lc_t2 = cats(lc_t2)[[1]],
    tbl_lookup_admin = cats(admin_)[[1]],
    lc_composition_tbl = lc_composition_tbl,
    lc_composition_barplot = lc_composition_barplot
  )
  # Create crosstabulation
  crosstab_matrix_landscape <- create_crosstab(land_cover = c(lc_t1, lc_t2))[["crosstab_square"]] |>
    as.data.frame.matrix()

  crosstab_result <- create_crosstab(land_cover = c(lc_t1, lc_t2), zone = admin_)[["crosstab_long"]]

  # Get spatResolution
  if( convert_to_Ha == TRUE) {
    SpatRes <- calc_res_conv_factor_to_ha(raster_input = lc_t1)

    crosstab_result <- mutate(crosstab_result, Ha = Freq*SpatRes)
    crosstab_matrix_landscape <- crosstab_result
  }

  # Summarize crosstabulation at landscape level
  # Subsetting the crosstab_result data frame
  selected_cols <- select(crosstab_result, -names(admin_))

  # Getting the names of the columns to be grouped
  group_cols <- setdiff(names(selected_cols), c("Freq", "Ha"))

  # Grouping the data frame by the columns selected above
  grouped_df <- group_by_at(selected_cols, group_cols)

  # Summarizing the grouped data
  if ("Ha" %in% names(grouped_df)) {
    crosstab_landscape <- summarise(grouped_df, Freq = sum(.data[["Freq"]]), Ha = sum(.data[["Ha"]]), .groups = "drop")
  } else {
    crosstab_landscape <- summarise(grouped_df, Freq = sum(.data[["Freq"]]), .groups = "drop")
  }
  # Create Sankey diagrams at landscape level
  ## Sankey diagram showing all changes
  sankey_landscape <- crosstab_landscape %>%
    create_sankey(area_cutoff = cutoff_landscape, change_only = FALSE)

  ## Sankey diagram showing only changes
  sankey_landscape_chg_only<- crosstab_landscape %>%
    create_sankey(area_cutoff = cutoff_landscape, change_only = TRUE)

  # Compute 10 dominant land use changes
  luc_top_10 <- crosstab_landscape %>% calc_top_lcc(n_rows = 10)

  # Tabulate and plot 10 dominant land use changes
  luc_top_10_barplot <- luc_top_10 %>%
    plot_lcc_freq_bar(col_T1 = as.character(time(lc_t1)), col_T2 = as.character(time(lc_t2)),
                      Freq = if ("Ha" %in% names(luc_top_10)) "Ha" else "Freq")

  # Store results at landscape level
  landscape_level <- list(
    sankey_landscape= sankey_landscape,
    sankey_landscape_chg_only = sankey_landscape_chg_only,
    luc_top_10_tbl = luc_top_10,
    luc_top_10_barplot = luc_top_10_barplot,
    crosstab_landscape = crosstab_matrix_landscape,
    crosstab_long = crosstab_result
  )

  # Compute summaries for each planning unit
  pu_names <- crosstab_result %>% pull(names(admin_)) %>% unique()
  pu_level <- purrr::map(pu_names, ~ lcc_summary_by_pu(crosstab_tbl = crosstab_result, pu_column = names(admin_), pu_name = .x, sankey_area_cutoff = cutoff_pu, n_top_lcc = 10))
  pu_level <- stats::setNames(pu_level,pu_names)

  # Return all results
  return(list(input_dataviz = input_dataviz, landscape_level = landscape_level, pu_level = pu_level))
}


# ques_pre_traj -----------------------------------------------------------

#' Calculate land use and land cover change trajectory
#'
#' This function calculates the trajectory map, creates a cross-tabulation of land cover and administrative zones,
#' and plots a bar chart of the trajectory data.
#'
#' @param lc_t1_ A raster layer representing land cover at time 1.
#' @param lc_t2_ A raster layer representing land cover at time 2.
#' @param admin_ A raster layer representing administrative zones.
#' @param lookup_traj_reclass A lookup table for reclassifying trajectories.
#' @param lookup_trajectory_complete A lookup table for completing trajectories.
#' @param trajectory_column_name The name of the trajectory column. Default is "trajectory".
#' @param convert_to_Ha Logical. If TRUE, the pixel counts are converted to hectares. Default is TRUE.
#' @return A list containing the trajectory map, the area table, and the bar plot.
#' @importFrom terra freq
#' @importFrom dplyr group_by summarise mutate arrange select rename
#' @importFrom tidyr drop_na
#' @importFrom stringr str_detect
#' @export
#' @examples
#' \dontrun{
#' ques_pre_trajectory(lc_t1_, lc_t2_, admin_, lookup_traj_reclass, lookup_trajectory_complete)
#' }
ques_pre_trajectory <- function(lc_t1_, lc_t2_, admin_, lookup_traj_reclass, lookup_trajectory_complete, trajectory_column_name = "trajectory", convert_to_Ha = TRUE){
  # Calculate the trajectory map
  luc_trajectory_map <-
    calc_trajectory_map(
      lc_t1_ = lc_t1_,
      lc_t2_ = lc_t2_,
      lookup_traj_reclass = lookup_traj_reclass,
      lookup_trajectory_complete = lookup_trajectory_complete,
      trajectory_column_name = trajectory_column_name
    )

  # Create a cross-tabulation of land cover and administrative zones
  crosstab_traj <- create_crosstab(land_cover = luc_trajectory_map, zone = admin_)[["crosstab_long"]]
  names(crosstab_traj)[1]<- trajectory_column_name

  # Create a frequency table of the trajectory map
  table_traj_area <- luc_trajectory_map |>
    terra::freq() |>
    dplyr::group_by(value) |>
    summarise(count=sum(count)) |>
    rename("Trajectory" = 1, "Pixel"= 2)

  # Convert pixel counts to hectares if convert_to_Ha is TRUE
  if(convert_to_Ha == TRUE) {
    SpatRes <- calc_res_conv_factor_to_ha(raster_input = lc_t1_)
    crosstab_traj <- mutate(crosstab_traj, Ha = Freq*SpatRes)
    table_traj_area <- mutate(table_traj_area, Ha = Pixel*SpatRes) |> select(-Pixel) |> arrange(-Ha) |> tidyr::drop_na()
  }

  # Create a bar plot of the trajectory data
  barplot_traj <- plot_bar_trajectory(table_traj_area)

  # Store results at landscape level
  landscape_level <- list(
    luc_trajectory_map = luc_trajectory_map,
    #crosstab_traj  = crosstab_traj,
    table_traj_area = table_traj_area,
    barplot_traj = barplot_traj
  )
  # Compute summaries for each planning unit
  pu_names <- crosstab_traj |> pull(names(admin_)) |> unique()
  pu_level <- purrr::map(pu_names, ~ lcc_trajectory_by_pu(crosstab_tbl = crosstab_traj, pu_column = names(admin_), pu_name = .x))
  pu_level <- stats::setNames(pu_level, pu_names)

  return(list(landscape_level = landscape_level, pu_level = pu_level))
}


# calc_traj_map -----------------------------------------------------------

#' Calculate Trajectory Map
#'
#' This function calculates the trajectory map by reclassifying two rasters
#' (representing land cover at two different time points), concatenating them,
#' and then adding categorical information based on a provided lookup table.
#'
#' @param lc_t1_ A raster object representing the land cover at time 1.
#' @param lc_t2_ A raster object representing the land cover at time 2.
#' @param lookup_traj_reclass A data frame containing the reclassification rules.
#' @param lookup_trajectory_complete A data frame containing the lookup table for trajectories.
#' @param trajectory_column_name A string representing the column name for the trajectory in the lookup table.
#'
#' @return
#' A raster object representing the trajectory map.
#'
#' @examples
#' \dontrun{
#' # assuming we have two raster objects "lc_t1", "lc_t2",
#' #a reclassification table "lookup_traj_reclass",
#' # and a lookup table "lookup_trajectory_complete"
#' my_trajectory_map <- calc_trajectory_map(lc_t1_, lc_t2_, lookup_traj_reclass,
#' lookup_trajectory_complete,
#' trajectory_column_name = "trajectory")
#' }
#'
#' @export
#' @importFrom terra classify levels concats addCats as.factor
#' @importFrom dplyr select_if select left_join row_number mutate
#' @importFrom tibble tibble

calc_trajectory_map <-
  function(lc_t1_,
           lc_t2_,
           lookup_traj_reclass,
           lookup_trajectory_complete,
           trajectory_column_name = "trajectory") {
    # Reclassify the rasters using the reclassification table
    lc_t1_reclass <-
      reclass_raster_to_categories(raster_map = lc_t1_, reclass_table = lookup_traj_reclass)
    lc_t2_reclass <-
      reclass_raster_to_categories(raster_map = lc_t2_, reclass_table = lookup_traj_reclass)

    # Concatenate the reclassified rasters
    concats_result <- concats(lc_t1_reclass, lc_t2_reclass)

    # Extract the first level from the "concats_result" and convert it to a data frame.
    # Rename the columns as "ID" and "ID_traj".
    lookup_concats <- levels(concats_result)[[1]] %>%
      data.frame() %>%
      dplyr::rename("ID" = 1, "ID_traj" = 2)

    # Create a lookup table for trajectories by subsetting "combinations" for "ID_traj" and "trajectory".
    lookup_traj <-
      lookup_trajectory_complete[c("ID_traj", trajectory_column_name)]

    # Perform a left join operation on "lookup_concats" and "lookup_traj" using "ID_traj" as the key.
    # Remove columns "ID" and "ID_traj" from the result.
    lookup_traj <-
      dplyr::left_join(lookup_concats, lookup_traj, by = "ID_traj") %>%
      dplyr::select(-c("ID", "ID_traj"))

    # Add categorical information to "concats_result" based on the "lookup_traj".
    map_trajectory <- addCats(concats_result, lookup_traj)

    lookup_traj_short <- lookup_traj %>% unique %>% mutate(ID = dplyr::row_number(), .before = 1)

    names(map_trajectory) <- trajectory_column_name

    reclass_mat <- cats(map_trajectory)[[1]] %>%
      dplyr::select(ID,!!trajectory_column_name) %>%
      left_join(lookup_traj_short, by = trajectory_column_name) %>%
      dplyr::select(-!!trajectory_column_name) %>%
      as.matrix()

    map_trajectory <-
      terra::classify(map_trajectory, reclass_mat) %>%
      terra::as.factor()

    levels(map_trajectory)<- lookup_traj_short

    return(map_trajectory)
  }


# plot_bar_trajectory -----------------------------------------------------

#' Plot a Bar Chart of Trajectories
#'
#' This function creates a bar plot from a data frame with custom colors based on category names.
#' Categories containing "loss", "recovery", or "stable" are colored salmon, lightgreen, or lightblue, respectively.
#' All other categories are colored grey.
#'
#' @param df A data frame containing the data to plot. The first column should be the categories and the second column should be the numeric values.
#' @return A ggplot2 object representing the bar plot.
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip theme_minimal theme labs element_text
#' @importFrom stringr str_detect
#' @export
#' @examples
#' \dontrun{
#' plot_bar_trajectory(table_traj_area)
#' }
plot_bar_trajectory <- function(df) {
  # Get the column names
  cat_col <- names(df)[1]
  val_col <- names(df)[2]

  # Convert first column to factor to maintain the order in the plot
  df[[cat_col]] <- factor(df[[cat_col]], levels = df[[cat_col]])

  # Create a function to assign colors based on category names
  assign_colors <- function(category) {
    category <- tolower(category)  # Convert to lower case
    if (stringr::str_detect(category, "loss")) {
      return("salmon")
    } else if (stringr::str_detect(category, "recovery")) {
      return("lightgreen")
    } else if (stringr::str_detect(category, "stable")) {
      return("lightblue")
    } else {
      return("grey")
    }
  }

  # Apply the function to the first column to create the color vector
  color_vector <- sapply(df[[cat_col]], assign_colors)

  # Create the plot
  barplot_traj <-
    ggplot2::ggplot(df, ggplot2::aes(
      x = reorder(.data[[cat_col]], .data[[val_col]]),
      y = .data[[val_col]],
      fill = .data[[cat_col]]
    )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +  # Flip the axes
    ggplot2::scale_fill_manual(values = color_vector) +  # Use manual color scale
    ggplot2::theme_minimal() +  # Use minimal theme
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 12),
      # Increase y-axis label size
      axis.text.x = ggplot2::element_text(size = 12)   # Increase x-axis label size
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Area",
      fill = "Category",
      title = NULL
    ) +
    ggplot2::theme(legend.position = "none")

  return(barplot_traj)
}


# lcc_trajectory_by_pu ----------------------------------------------------

#' Land Cover Change Trajectory by Planning Unit
#'
#' This function filters a cross-tabulation table based on a specified planning unit and generates a bar plot of the land cover change trajectory.
#'
#' @param crosstab_tbl A data frame containing cross-tabulation data.
#' @param pu_column A character string specifying the column name of the planning unit in the data frame.
#' @param pu_name A character string specifying the name of the planning unit to filter by.
#' @return A list containing the filtered cross-tabulation table and the land cover change trajectory bar plot.
#' @importFrom dplyr filter select
#' @importFrom rlang sym
#' @export
lcc_trajectory_by_pu <- function(crosstab_tbl, pu_column, pu_name){
  # Error checking
  if (!inherits(crosstab_tbl, "data.frame")) stop("crosstab_tbl must be a data frame.")
  if (!is.character(pu_column)) stop("pu_column must be a character string.")


  # Check if the required columns exist in the data frame
  if (!pu_column %in% names(crosstab_tbl)) stop(paste("The data frame does not contain the column:", pu_column))
  if (!"Freq" %in% names(crosstab_tbl)) stop("The data frame does not contain the column: Freq")

  # Filter the crosstab table based on planning unit and remove the planning unit column
  traj_tbl_pu <- crosstab_tbl %>%
    dplyr::filter(!!sym(pu_column) %in% pu_name) %>%
    dplyr::select(-!!sym(pu_column))

  plot_traj_pu <- plot_bar_trajectory(traj_tbl_pu)

  # Return a list containing the summary table and the  land cover change trajectory
  return(list(traj_tbl_pu = traj_tbl_pu, plot_traj_pu = plot_traj_pu))
}


# reclass raster to category ----------------------------------------------

#' Reclassify a Raster to Categories
#'
#' This function reclassifies a raster using a provided reclassification table.
#'
#' @param raster_map A raster object to be reclassified.
#' @param reclass_table A data frame containing the reclassification rules. The first column must be the existing
#' values (ID_check) and the second column should be the new values (reclass_mat).
#'
#' @return
#' A reclassified factor raster.
#'
#' @examples
#' \dontrun{
#' # assuming we have a raster object "my_raster" and a reclassification table "my_table"
#' my_reclassed_raster <- reclass_raster_to_categories(my_raster, my_table)
#' }
#'
#' @export
#' @importFrom terra classify droplevels as.factor cats
#' @importFrom dplyr select_if rename left_join select
#' @importFrom purrr pluck
#'
reclass_raster_to_categories <- function(raster_map, reclass_table){
  # Retrieve unique IDs from the raster and rename column to 'Value'
  ID_check <- terra::droplevels(raster_map) |> terra::cats() |> pluck(1) |> select(1)

  colnames(reclass_table)[1] <- colnames(ID_check)

  # Join the reclassification table with the ID_check,
  # keep only the numeric columns and convert to matrix
  reclass_mat <- left_join(ID_check, reclass_table, by=colnames(ID_check)) %>% select_if(is.numeric) %>% as.matrix()

  # Reclassify the raster using the reclassification matrix and convert to factor
  raster_map_reclass <- classify(raster_map, reclass_mat) %>% as.factor()

  return(raster_map_reclass)
}


# add_legend_to_categorical_raster ----------------------------------------

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



# rasterise_multi_polygon -------------------------------------------------

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



# plot_planning_unit ------------------------------------------------------

#' Create a plot of a spatial planning unit with labels.
#'
#' This function creates a plot using ggplot2 where each planning unit is
#' represented as a polygon filled with light green color. The unit names
#' are added as text labels that repel each other to minimize overlap.
#'
#' @param planning_unit A sf object representing the planning units.
#' @param map_label A string representing the column name to use for labels. If empty, the second column of planning_unit is used.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot geom_sf theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @importFrom sf st_is
#' @importFrom rlang !! sym
#' @export
plot_planning_unit <- function(planning_unit, map_label = NULL) {
  # Check if planning_unit is a sf object
  if(!inherits(planning_unit, "sf")) {
    stop("planning_unit must be an sf object")
  }

  # Check if planning_unit is a MULTIPOLYGON
  if(!any(st_is(planning_unit, "MULTIPOLYGON"))) {
    stop("planning_unit must be a MULTIPOLYGON")
  }

  # Check if planning_unit has a column named "geometry"
  if(!"geometry" %in% names(planning_unit)) {
    stop("planning_unit must have a column named 'geometry'")
  }

  # If map_label is not provided, use the second column
  if(is.null(map_label)) {
    map_label <- names(planning_unit)[2]
  }

  # Check if map_label is a string that is present among the names of the sf polygon attribute table
  if(!is.character(map_label) || length(map_label) != 1 || !(map_label %in% names(planning_unit))) {
    stop("map_label must be a string that is present among the names of the sf polygon attribute table")
  }

  ggplot() +
    geom_sf(data = planning_unit, fill = "lightgreen", color = "black", size = 0.2) +
    ggrepel::geom_text_repel(
      data = planning_unit,
      aes(label = !!sym(map_label), geometry = geometry),
      stat = "sf_coordinates",
      color = "grey30",     # text color
      bg.color = "white", # shadow color
      bg.r = 0.15 ,
      box.padding = 2,
      force = 1,
      max.overlaps = Inf
    ) +      # shadow radius) +
    theme_minimal()
}


# plot_lcc_freq_bar -------------------------------------------------------

#' Plot Frequency or Area of Land Cover Change as a Bar Chart
#'
#' This function takes in a data frame and plots the frequency or area of land cover changes as a bar chart.
#' It automatically detects if a column named "Ha" exists, and if so, it plots data using "Ha" as a priority
#' over "Freq". If "Ha" does not exist, it defaults back to using "Freq".
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip labs theme_minimal scale_y_continuous
#' @importFrom stringr str_wrap
#' @importFrom scales comma
#' @importFrom viridis scale_fill_viridis
#' @importFrom dplyr select_if
#' @importFrom stats reorder
#'
#' @param lcc_table A data frame containing at least two columns of characters and one column of numeric values.
#' @param col_T1 First column of characters. If NULL, the function will assign the first column containing characters.
#' @param col_T2 Second column of characters. If NULL, the function will assign the second column containing characters.
#' @param Freq Numeric column representing frequencies or areas. If NULL, the function will assign "Ha" if present,
#' otherwise the first numeric column.
#'
#' @return A ggplot object showing the frequency or area of land cover changes as a bar chart.
#'
#' @examples
#' \dontrun{
#' plot_lcc_freq_bar(lcc_table = luc_top_10, col_T1 = NULL, col_T2 =NULL, Freq =NULL)
#' }
#' @export

plot_lcc_freq_bar <- function(lcc_table, col_T1 = NULL, col_T2 = NULL, Freq = NULL) {

  # If col_T1 and col_T2 are NULL, find the first and second character columns
  if(is.null(col_T1) | is.null(col_T2)) {
    char_cols <- names(select_if(lcc_table, is.character))
    if(is.null(col_T1)) {
      col_T1 <- char_cols[1]
    }
    if(is.null(col_T2)) {
      col_T2 <- char_cols[2]
    }
  }

  # If both "Ha" and "Freq" are present, use "Ha"
  if("Ha" %in% names(lcc_table)) {
    Freq <- "Ha"
  }
  # If Freq is NULL and "Ha" is not present, find the first numeric column
  else if(is.null(Freq)) {
    Freq <- names(select_if(lcc_table, is.numeric))[1]
  }

  # Create a new combined label by concatenating the two character columns
  lcc_table[["label"]] <- apply(lcc_table[c(col_T1, col_T2)], 1, paste, collapse = " to ")

  # Wrap the text to a maximum width of 30 characters
  lcc_table[["label"]] <- str_wrap( lcc_table[["label"]], width = 30)

  # Plot the data
  p <- ggplot(lcc_table, aes(x=reorder(label, !!sym(Freq)), y= !!sym(Freq), fill= !!sym(Freq))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "Land Cover Change", y = Freq, fill = Freq) +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    viridis::scale_fill_viridis(discrete = FALSE, direction = -1, guide = "none", labels = comma)

  return(p)
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




# calc_res_conve_factor_to_ha ---------------------------------------------

#' Calculate Resolution Conversion Factor To Hectares
#'
#' This function calculates the conversion factor of a raster map resolution to hectares,
#' depending on the coordinate reference system (CRS) of the raster.
#' Raster maps with projection in meter or degree units are supported.
#'
#' @param raster_input A terra::SpatRaster object.
#' @return A numerical value representing the conversion factor of the raster map resolution to hectares.
#' @importFrom terra crs res
#' @export
calc_res_conv_factor_to_ha <- function(raster_input) {

  crs <- terra::crs(raster_input, proj=TRUE) # Get the CRS of the raster

  # Check if the CRS is in meter unit
  if (grepl("+units=m", crs)) {
    message("Raster map has a projection in metre unit.")
    conversion_factor <- terra::res(raster_input)[1] * terra::res(raster_input)[2] / 10000
    message(paste("Raster map has ", conversion_factor, " Ha spatial resolution. Pre-QuES will automatically generate data in Ha unit."))

    # Check if the CRS is in degree unit
  } else if (grepl("+proj=longlat", crs)) {
    message("Raster map has a projection in degree unit.")
    conversion_factor <- terra::res(raster_input)[1] * terra::res(raster_input)[2] * (111319.9 ^ 2) / 10000
    message(paste("Raster map has ", conversion_factor, " Ha spatial resolution. Pre-QuES will automatically generate data in Ha unit."))

    # If the CRS is neither in meter nor degree unit, throw an error
  } else {
    stop("Projection of the raster map is unknown")
  }

  return(conversion_factor)
}


# calc_lc_freq ------------------------------------------------------------

#' Calculate land cover frequency for multiple raster layers
#'
#' This function takes multiple raster layers as input and returns a
#' frequency table for each layer, sorted by the count of the last raster layer in descending order.
#' An input of a terra's rast object is allowed.
#'
#' @param raster_list list of raster layers or a single raster layer.
#'
#' @return A dataframe of frequency tables.
#'
#' @importFrom terra compareGeom freq levels time
#' @importFrom dplyr left_join select arrange desc rename
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#'
#' # Create a vector of raster file names
#' lc_maps <- c("kalbar_LC11.tif", "kalbar_LC20.tif") %>%
#'   # Apply LUMENSR_example function to each file in the vector
#'   map(~ LUMENSR_example(.x)) %>%
#'   # Convert each file to a raster object
#'   map(~ terra::rast(.x)) %>%
#'   # Add a legend to each raster object using a provided lookup table
#'   map(~ add_legend_to_categorical_raster(raster_file = .x, lookup_table = lc_lookup_klhk))
#'
#' # Calculate the frequency table for each raster object in the list
#' freq_table <- calc_lc_freq(lc_maps)
#'
#' # Print the resulting frequency table
#' print(freq_table)
#' }
calc_lc_freq <- function(raster_list) {

  # Check if input is a single raster layer
  if (class(raster_list)[1] == "SpatRaster") {
    raster_list <- list(raster_list)
  } else if (!is.list(raster_list)) {
    stop("Input must be a list of raster layers or a single raster layer")
  }

  # Check if all rasters have the same extent and CRS
  if (length(raster_list) > 1) {
    for (i in 2:length(raster_list)) {
      if (!terra::compareGeom(raster_list[[1]], raster_list[[i]])) {
        stop("All rasters must have the same extent and projection system")
      }
    }
  }

  # Prepare an empty list to store frequency tables
  freq_tables <- list()

  # Loop over all raster layers in the list
  for (i in 1:length(raster_list)) {
    # Check if raster has attributes
    if (is.null(terra::levels(raster_list[[i]]))) {
      warning(paste0("Raster ", i, " has no attributes"))
    }

    # Get frequency table
    freq <- terra::freq(raster_list[[i]])

    # Rename 'count' column to be specific for each raster
    names(freq)[names(freq) == "count"] <- paste0(names(raster_list[[i]]), "_count")

    # Store frequency table in the list
    freq_tables[[i]] <- freq
  }

  # Combine frequency tables into one dataframe
  freq_df <- freq_tables[[1]]
  if (length(freq_tables) > 1) {
    for (i in 2:length(freq_tables)) {
      freq_df <- dplyr::left_join(freq_df, freq_tables[[i]], by = c("layer", "value"))
    }
    freq_df <- dplyr::select(freq_df, -layer)
  }

  # Sort by the count of the last raster layer in descending order
  freq_df <- dplyr::arrange(freq_df, dplyr::desc(freq_df[[ncol(freq_df)]]))
  freq_df <- dplyr::rename(freq_df, `Jenis tutupan lahan` = value)

  # Check if all SpatRaster objects have a time attribute
  all_times_present <- all(sapply(raster_list, function(x) !is.null(time(x))))
  if (all_times_present) {
    # Loop over raster_list
    for (i in seq_along(raster_list)) {
      # Get the time attribute as a string
      time_i <- as.character(time(raster_list[[i]]))
      # Rename the corresponding column of freq_df
      names(freq_df)[i+1] <- time_i
    }
    return(freq_df)
  } else {
    return("Not all SpatRaster objects in the list have a time attribute")
  }
  return(freq_df)
}



# abbreviate by column ----------------------------------------------------

#' Replace Column Values with Shorter Version
#'
#' This function shortens the character column values in a data frame by removing vowels after the first character,
#' and also provides an option to disable this vowel removal. It replaces spaces with underscores and removes characters after a slash.
#' If no column names are provided, the function attempts to find and use the first character column in the data frame.
#'
#' @param df A data frame.
#' @param col_names A character vector specifying the names of the columns to be abbreviated.
#' If NULL (default), the function attempts to use the first character column.
#' @param remove_vowels A logical value indicating whether to remove vowels from column values after the first character. Default is FALSE.
#' @importFrom textclean replace_non_ascii
#' @return A data frame with specified columns abbreviated.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   col1 = c("Hutan lahan kering sekunder / bekas tebangan", "Savanna / Padang rumput"),
#'   col2 = c("Hutan lahan kering sekunder", "Savanna"),
#'   stringsAsFactors = FALSE
#' )
#' abbreviate_by_column(df, c("col1", "col2"), remove_vowels=TRUE)
abbreviate_by_column <- function(df, col_names = NULL, remove_vowels= FALSE) {
  # Check if df is a data frame
  if(!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  # Check if df has at least one column
  if(ncol(df) < 1) {
    stop("df must have at least one column")
  }

  # If col_names is NULL, find the first character column
  if(is.null(col_names)) {
    col_names <- names(df)[which(sapply(df, is.character))[1]]
  }

  # Check if the provided col_names exist in df
  if(!all(col_names %in% names(df))) {
    stop("Some column names provided are not columns in df")
  }

  # Define the abbreviation function
  abbreviate_string <- function(input_string, drop_vowels = remove_vowels) {

    # Remove characters after the slash, if any
    string <- textclean::replace_non_ascii(input_string)
    string <- strsplit(string," / ")[[1]][1]

    if(isTRUE(drop_vowels)){
      # Replace spaces with underscores
      string <- gsub(" ", "_", string)

      # Split string into words
      words <- strsplit(string, "_")[[1]]

      # Abbreviate each word by removing the vowels (but keep the first character even if it's a vowel)
      words <- sapply(words, function(word) {
        ifelse(grepl("^[aeiouAEIOU]", word),
               paste0(substr(word, 1, 1), gsub("[aeiouAEIOU]", "", substr(word, 2, nchar(word)))),
               gsub("[aeiouAEIOU]", "", word)
        )
      })

      # Combine words back into a single string
      string <- paste(words, collapse = "_")
    }

    return(string)
  }

  # Apply the abbreviation function to the selected columns
  for (col_name in col_names) {
    df[[col_name]] <- unlist(lapply(df[[col_name]], abbreviate_string))
  }

  return(df)
}



# plot_lc_freq ------------------------------------------------------------

#' Land Cover Frequency Plot
#'
#' This function generates a side by side bar plot comparing the frequency of land cover types in two different years.
#'
#' @param lc_table A data frame containing land cover types and their frequency in two different years.
#' @param column_lc_type The column name of land cover types in lc_table.
#' @param column_T1 The column name of the frequency data for Timepoint 1 in lc_table.
#' @param column_T2 The column name of the frequency data for Timepoint 2 in lc_table.
#'
#' @return A ggplot object representing the side by side bar plot.
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_reverse coord_cartesian labs theme_minimal theme geom_blank theme_void scale_x_continuous margin
#' @importFrom forcats fct_reorder
#' @importFrom cowplot plot_grid
#' @importFrom stringr str_wrap
#' @importFrom tidyr replace_na pivot_longer
#' @importFrom rlang :=
#'
#' @examples
#'  \dontrun{
#' plot_lc_freq(lc_table = lc_tbl,
#'              column_lc_type = "Jenis tutupan lahan",
#'              column_T1 = "NTT_LC90_count",
#'              column_T2 = "NTT_LC20_count")
#' }

plot_lc_freq <- function(lc_table, column_lc_type, column_T1, column_T2) {
  # Validate inputs
  stopifnot(is.data.frame(lc_table))
  stopifnot(is.character(column_lc_type))
  stopifnot(is.character(column_T1))
  stopifnot(is.character(column_T2))

  # Replace NA values with 0
  lc_table <- lc_table %>%
    mutate(!!sym(column_T1) := replace_na(!!sym(column_T1), 0),
           !!sym(column_T2) := replace_na(!!sym(column_T2), 0))

  # Reshape data to long format. This structure is more suitable for plotting.
  lc_table_long <- lc_table %>%
    pivot_longer(cols = c(column_T1, column_T2),
                 names_to = "Year",
                 values_to = "Count")

  # Define the maximum range for the x-axis
  max_range <- max(lc_table_long$Count, na.rm = TRUE)

  # Reorder the factor levels of Land_Cover_Type based on the count from Timepoint 1 data
  lc_table_T1 <- lc_table %>%
    mutate(!!column_lc_type := fct_reorder(!!sym(column_lc_type), !!sym(column_T1)))

  # Create lc_table_long with new level order
  lc_table_long <- lc_table_T1 %>%
    pivot_longer(cols = c(column_T1, column_T2),
                 names_to = "Year",
                 values_to = "Count")

  # Wrap the text of Land_Cover_Type to make it fit into two lines
  lc_table_long[[column_lc_type]] <- factor(str_wrap(as.character(lc_table_long[[column_lc_type]]), width = 30))


  # Create the Timepoint 1 plot with positive values
  plot1 <- ggplot(lc_table_long[lc_table_long$Year == column_T1,],
                  aes(x = Count, y = .data[[column_lc_type]])) +
    geom_bar(stat = "identity", fill = "lightblue") +
    scale_x_reverse(breaks = seq(0, max_range, by = 500000),
                    labels = function(x) format(abs(x), big.mark = ",", scientific = FALSE)) +
    coord_cartesian(xlim = c(max_range,0)) +
    labs(x = paste("Count -", column_T1), y = "") +
    theme_minimal() +
    theme(plot.margin = margin(5.5, 50, 5.5, 5.5),
          axis.text.y = element_blank())

  # Create the Timepoint 2 plot with negative values
  plot2 <- ggplot(lc_table_long[lc_table_long$Year == column_T2,],
                  aes(x = Count, y = .data[[column_lc_type]])) +
    geom_bar(stat = "identity", fill = "salmon") +
    labs(x = paste("Count -", column_T2), y = "") +
    scale_x_continuous(limits = c(0, max_range),
                       breaks = seq(0, max_range, by = 500000),
                       labels = function(x) format(abs(x), big.mark = ",", scientific = FALSE)) +
    theme_minimal() +
    theme(plot.margin = margin(5.5, 5.5, 5.5, 50),
          axis.text.y = element_blank())

  # Create the label plot. This is the middle plot which just shows the land cover types.
  lc_label <- ggplot(lc_table_long, aes(y = .data[[column_lc_type]])) +
    geom_blank() +
    theme_void() +
    theme(axis.text.y = element_text(angle = 0, hjust = 0.5))

  # Arrange the plots side by side using the cowplot package
  final_plot <- plot_grid(plot1, lc_label, plot2, align = "h", nrow = 1, rel_widths = c(1, 0.2, 1))

  return(final_plot)
}





# lcc_summary_by_pu -------------------------------------------------------
#' Summarize land cover changes by planning unit
#'
#' This function provides a summary of land cover changes for specified planning units. It returns a sankey plot and the top land cover changes.
#'
#' @param crosstab_tbl A data frame containing at least 2 columns of land cover types (could be character, numeric, or factor) and a "Freq" column containing numeric values.
#' @param pu_column A character string representing the column name of the planning unit in `crosstab_tbl`.
#' @param pu_name A character string representing the name of the planning unit.
#' @param sankey_area_cutoff A numeric value indicating the minimum area of changes to be displayed in the Sankey plot.
#' @param n_top_lcc An integer representing the number of top land cover changes to be displayed. Default is 10.
#' @return A list containing the Sankey plot (`sankey_pu`) and a data frame of the top land cover changes (`luc_top_pu`).
#' @importFrom dplyr filter select
#' @importFrom utils head
#' @importFrom rlang sym
#' @importFrom stats xtabs
#' @export
lcc_summary_by_pu <- function(crosstab_tbl, pu_column, pu_name, sankey_area_cutoff, n_top_lcc = 10){
  # Error checking
  if (!inherits(crosstab_tbl, "data.frame")) stop("crosstab_tbl must be a data frame.")
  if (!is.character(pu_column)) stop("pu_column must be a character string.")
  if (!is.numeric(sankey_area_cutoff)) stop("sankey_area_cutoff must be a numeric value.")
  if (!is.numeric(n_top_lcc)) stop("n_top_lcc must be an integer.")

  # Check if the required columns exist in the data frame
  if (!pu_column %in% names(crosstab_tbl)) stop(paste("The data frame does not contain the column:", pu_column))
  if (!"Freq" %in% names(crosstab_tbl)) stop("The data frame does not contain the column: Freq")

  # Filter the crosstab table based on planning unit and remove the planning unit column
  filter_crosstab <- crosstab_tbl |>
    dplyr::filter(!!sym(pu_column) %in% pu_name) |>
    dplyr::select(-!!sym(pu_column))



  filter_crosstab |>
    group_by_at(c(1,2)) |>
    summarise(Freq = sum(Freq), Ha = sum(Ha)) |>
    ungroup() |>
    tidyr::pivot_wider(names_from = 1, id_cols = 2, values_from = "Ha")

  # Calculate the maximum area from 'Freq' or 'Ha' column
  max_area <- max(filter_crosstab$Freq, filter_crosstab$Ha)
  # Check if sankey_area_cutoff is larger than max_area
  if (sankey_area_cutoff > max_area) {
    warning("The value of sankey_area_cutoff is larger than any area in the dataset. Setting it to the top 10 changes")
    # Create a Sankey plot based on the filtered crosstab table
    sankey_pu <- filter_crosstab |>
      head(n = 10) |>
      create_sankey(area_cutoff = 0, change_only = FALSE)
  } else {
    # Create a Sankey plot based on the filtered crosstab table
    sankey_pu <- filter_crosstab |>
      create_sankey(area_cutoff = sankey_area_cutoff, change_only = FALSE)
  }



  # Calculate the top land cover changes based on the filtered crosstab table
  luc_top_pu <- filter_crosstab |>
    calc_top_lcc(n_rows = n_top_lcc)

  crosstab_xtab <- filter_crosstab |> dplyr::select(1,2,3)

  crosstab_pu <- xtabs(Freq ~ ., data = crosstab_xtab)
  # Return a list containing the Sankey plot and the top land cover changes
  return(list(sankey_pu = sankey_pu, luc_top_pu = luc_top_pu, crosstab_pu = crosstab_pu))
}



# create_sankey -----------------------------------------------------------

#' Create a Sankey Diagram
#'
#' This function takes a frequency table (crosstab) as input and creates a Sankey diagram.
#' If a column named "Ha" exists in the frequency table, it will be used for filtering and sorting,
#' otherwise it will fall back to using the "Freq" column.
#'
#' @author Dony Indiarto
#' @param freq_table A frequency table containing land cover T1, T2, T3, etc..
#' and a Frequency column (numeric). If a column named "Ha" exists, it will be used instead of "Freq".
#' @param area_cutoff Minimum number of pixels of land use land cover frequency to include.
#' @param change_only Logical flag, if TRUE exclude persistent land cover.
#' @return A Sankey plot.
#' @importFrom dplyr mutate_if rowwise filter ungroup mutate across cur_column n_distinct c_across
#' @importFrom rlang sym !!
#' @importFrom networkD3 sankeyNetwork
#' @export
#' @examples
#' \dontrun{
#'   synthetic_data <- data.frame(
#'     "1990" = rep(c("Forest", "Urban", "Agriculture", "Water"), each = 4),
#'     "2020" = rep(c("Forest", "Urban", "Agriculture", "Water"), 4),
#'     Freq = sample(1:100, 16)
#'   ) %>%
#'     dplyr::arrange("1990")
#'
#'   create_sankey(freq_table = synthetic_data, area_cutoff = 0, change_only = FALSE)
#' }
create_sankey <- function(freq_table, area_cutoff = 10000, change_only = FALSE) {

  # Check if "Freq" column exists and is numeric
  if (!"Freq" %in% colnames(freq_table)) {
    stop("The 'Freq' column does not exist in the data frame.")
  }

  if (!is.numeric(freq_table$Freq)) {
    stop("The 'Freq' column should contain numeric values.")
  }

  # Check if "Ha" column exists
  if ("Ha" %in% colnames(freq_table)) {
    # Drop "Freq" column
    freq_table <- freq_table[ , !names(freq_table) %in% "Freq"]
    value_col <- "Ha"
  } else {
    value_col <- "Freq"
  }

  if(change_only){
    df_filtered <- freq_table %>%
      mutate_if(is.factor, as.character) %>%
      rowwise() %>%
      filter(n_distinct(c_across(-length(freq_table))) > 1) %>%
      ungroup() %>%
      filter((!!sym(value_col)) > area_cutoff)
  } else {
    df_filtered <- freq_table %>%
      dplyr::filter((!!sym(value_col)) > area_cutoff)
  }

  # Error handling: if dataframe is empty after filtering
  if(nrow(df_filtered) == 0){
    stop("No data left after filtering, please check your inputs.")
  }

  # Get column names (years) without the 'X' prefix and 'Freq'
  colnames_ <- sub("^X", "", names(df_filtered)[-length(names(df_filtered))])

  # Use lapply to paste each value with the corresponding year
  df_modified <- df_filtered
  df_modified[-length(names(df_modified))] <- lapply(seq_along(colnames_), function(i)
    paste(df_filtered[[i]], "-", colnames_[i], sep = ""))

  # # Apply the suffixes to the selected columns
  # df_filtered <- df_filtered %>%
  #   mutate(across(-length(df_filtered), ~paste(., paste0("_T", which(names(df_filtered) == cur_column())), sep = "")))

  sankey_data <- df_modified %>%
    prepare_sankey(col_order = setdiff(colnames(df_modified), value_col), value_col = value_col)

  # Add a 'group' column to the 'links' and 'nodes' data frames
  sankey_data$links <- sankey_data$links %>% mutate(group = sankey_data$nodes$name[source + 1])
  sankey_data$nodes <- sankey_data$nodes %>% mutate(group = name)

  # Create the Sankey plot
  sankey_plot <- sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes, Source = "source",
                               Target = "target", Value = "value", NodeID = "name",
                               fontSize = 20, nodeWidth = 30, LinkGroup = "group", NodeGroup = "group"#,colourScale = color_scale
  )

  return(sankey_plot)
}




# create_crosstab ---------------------------------------------------------

#' Create Crosstab From Raster List
#'
#' This function takes a list of raster files, creates a stack, and returns a frequency table
#' (crosstab) of the stack. The frequency table is filtered to remove rows where the frequency is zero.
#'
#' @param land_cover A list of 'SpatRaster' objects.
#' @param zone A SpatRaster' object of planning unit/zone
#'
#' @return A data frame representing the crosstab of the input rasters.
#'
#' @importFrom terra rast crosstab time
#' @importFrom dplyr arrange desc
#' @importFrom purrr is_vector
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na
#'
#' @examples
#' \dontrun{
#'   # Read in the legend for raster data
#'   subset_legend <- LUMENSR::lc_lookup_klhk
#'
#'   # Read raster files
#'   raster_files <- c("kalbar_LC11.tif", "kalbar_LC15.tif", "kalbar_LC20.tif") %>%
#'     map(LUMENSR_example) %>%
#'     map(rast)
#'
#'   # Loop through raster files
#'   land_cover <- map(raster_files,
#'                             ~apply_lookup_table(raster_file = .x,
#'                                                 lookup_lc = subset_legend))
#'
#'   # Turn raster files into a frequency table
#'   crosstab_result <- create_crosstab(land_cover)
#' }
#'
#' @export
#'
create_crosstab <- function(land_cover, zone) {
  # Check if land_cover is a list of 'SpatRaster' objects
  if (!all(sapply(land_cover, function(x) class(x) == "SpatRaster"))) {
    stop("land_cover must be a list of 'SpatRaster' objects.")
  }

  # Check if all rasters in the list have levels (categories)
  has_levels <- sapply(land_cover, function(x) !is.null(terra::levels(x)))
  if(!all(has_levels)) {
    warning("Some rasters do not contain levels (categories).")
  }

  # Check if all SpatRaster objects have a time attribute
  all_times_present <- all(sapply(land_cover, function(x) !is.null(time(x))))
  if (!all_times_present) {
    stop("All SpatRaster objects must have a time attribute.")
  }

  # Rename layers based on year
  names(land_cover) <- as.character(time(land_cover))
  # Create a frequency table using crosstab and convert to a data frame
  # Check if 'zone' object exists
  # Check if 'zone' object exists
  if (!missing(zone)) {

    # Check if 'zone' and 'land_cover' have the same projection, extent and resolution
    if (compareGeom(land_cover, zone ,stopOnError = TRUE)) {

      crosstab_square <- terra::crosstab(c(land_cover, zone))

    } else {

      # Raise an error and stop execution
      stop("Zone exists but does not have the same projection, extent, and resolution as land_cover.")
    }
  } else {
    crosstab_square <- terra::crosstab(land_cover, useNA = TRUE, digits = 3)

  }

  crosstab_long <- crosstab_square %>%
    as.data.frame() %>%
    dplyr::arrange(desc(Freq)) %>% # order by descending Freq
    drop_na()

  # Rename columns to remove 'X' at the beginning
  names(crosstab_long) <- gsub("^X", "", names(crosstab_long))

  # Filter out rows where Freq is not equal to 0
  crosstab_long <- crosstab_long[crosstab_long$Freq != 0, ]
  # Return the result
  return(list(crosstab_square = crosstab_square, crosstab_long = crosstab_long))
}



# calc_top_lcc ----------------------------------------------------------

#' Get the top n rows of the input data frame
#'
#' This function filters rows that have more than one distinct value
#' across all columns except the last one, and then selects the top n rows
#' based on the value of the last column. If a column named "Ha" exists in the data frame,
#' it will be used for sorting the rows, otherwise, it will fall back to the "Freq" column.
#'
#' @param crosstab_result A data frame of a frequency table (crosstab) of two or more land cover maps.
#' crosstab_result must have a column named "Freq", representing the number of pixels / area for a given land cover change.
#' If a column named "Ha" exists, it will be used instead of "Freq".
#' @param n_rows The number of rows to select.
#'
#' @return A data frame containing the top n rows (top n changes).
#'
#' @importFrom dplyr mutate_if rowwise filter ungroup top_n c_across
#' @export
calc_top_lcc <- function(crosstab_result, n_rows) {
  # Check if crosstab_result is a data frame
  if(!is.data.frame(crosstab_result)) {
    stop("crosstab_result must be a data frame")
  }

  # Check if crosstab_result has a column named "Freq"
  if(!"Freq" %in% names(crosstab_result)) {
    stop("crosstab_result must have a column named 'Freq'")
  }

  # Check if 'Freq' column contains numeric values
  if(!is.numeric(crosstab_result$Freq)) {
    stop("'Freq' column must contain numeric values")
  }

  # Check if n_rows is a single number
  if(!is.numeric(n_rows) || length(n_rows) != 1) {
    stop("n_rows must be a single number")
  }

  # Check if "Ha" column exists
  if ("Ha" %in% names(crosstab_result)) {
    # Drop "Freq" column
    crosstab_result <- crosstab_result[ , !names(crosstab_result) %in% "Freq"]
    value_col <- "Ha"
  } else {
    value_col <- "Freq"
  }

  crosstab_result %>%               # Start with the 'crosstab_result' data frame.
    mutate_if(is.factor, as.character) %>%   # For each column, if it is a factor, convert it to character type.
    rowwise() %>%              # Change the operation mode to row-wise. This is useful for operations that need to be performed on each row individually.
    filter(n_distinct(c_across(-length(crosstab_result))) > 1) %>%  # Filter rows that have more than one distinct value across all columns except the last one.
    ungroup() %>%              # Remove the row-wise grouping.
    top_n(n_rows, wt = !!sym(value_col)) %>% dplyr::arrange(desc(!!sym(value_col)))   # Select the top n_rows by the selected column value.
}


# prepare_sankey ----------------------------------------------------------
#' Prepare data for creating a Sankey plot with multiple transitions
#'
#' This function prepares a data frame for creating a Sankey plot with multiple
#' transitions. The Sankey plot represents the flow from one set of values to
#' another, where the width of the flow is proportional to its quantity.
#'
#' @author Dony Indiarto
#'
#' @param df The data frame containing the data.
#' @param col_order A vector of column names in the order of the transitions.
#' @param value_col The name of the column to use as the values of the links.
#' @return A list containing two data frames: 'nodes' and 'links'.
prepare_sankey <- function(df, col_order, value_col) {

  # Create the nodes data frame
  # The nodes are all the unique values from the columns specified in col_order
  nodes <- data.frame(name = unique(as.character(unlist(df[col_order]))))

  # Initialize the links data frame
  links <- NULL

  # Iterate over the column names in col_order
  # For each pair of consecutive columns, create a new set of links
  for(i in 1:(length(col_order)-1)) {

    # Create the new links data frame
    # 'source' is the index in the nodes data frame of the value from the current column
    # 'target' is the index in the nodes data frame of the value from the next column
    # 'value' is the value from the value_col column
    # Note: we subtract 1 from the indices because networkD3 uses 0-based indexing
    new_links <- data.frame(
      source = match(df[[col_order[i]]], nodes$name) - 1,
      target = match(df[[col_order[i+1]]], nodes$name) - 1,
      value = df[[value_col]]
    )

    # Add the new links to the links data frame
    links <- rbind(links, new_links)
  }

  # Return the nodes and links data frames
  return(list(nodes = nodes, links = links))
}


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

# Write raster to a directory ---------------------------------------------

#' Write SpatRaster Objects to a Directory
#'
#' This function writes any number of SpatRaster objects to a specified a directory.
#'
#' @param raster_objects Named list of SpatRaster objects to write.
#' @param a_dir directory for saving the SpatRaster objects.
#' @export
#' @importFrom terra writeRaster
#'
#' @examples
#' \dontrun{
#' library(terra)
#' # assuming rasters is a named list of SpatRaster objects
#' a_dir<- tempdir()
#' write_rasters_to_tempdir(rasters, a_dir)
#' }
write_rasters_to_adir<- function(raster_objects, a_dir) {
  # Initialize a list to store the paths of the written raster files
  raster_paths <- list()

  # Iterate over each SpatRaster object
  for (name in names(raster_objects)) {
    # Define the file path
    file_path <- file.path(a_dir, paste0(name, ".tif"))

    # Write the SpatRaster object to the a directory
    terra::writeRaster(raster_objects[[name]], file_path,  overwrite=TRUE)

    # Append the file path to the list of paths
    raster_paths[[name]] <- file_path
  }

  return(raster_paths)
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


# Create a Color Lookup Table for Forest Change Trajectories  ----------------------

#' Create a Color Lookup Table for FForest Change Trajectories
#'
#' This function generates a tibble that maps forest change trajectories to their corresponding color codes.
#' The function will return a tibble containing two columns: 'def' for forest definitions and 'hex_color' for color codes.
#'
#' @return A tibble containing forest definitions and their corresponding color codes
#' @export
#' @importFrom tibble tibble
color_forest_trajectories <- function() {
  # Define the tibble with forest definitions and their corresponding colors
  color_lookup_def <- tibble(
    def = c(
      "Stable forest",
      "Forest degradation",
      "Deforestation",
      "Reforestation",
      "Other"
    ),
    color_palette = c("#006400", "#FFD700", "#FF4500", "#90EE90" , "#808080")
  )

  # Return the color lookup table
  return(color_lookup_def)
}


#' Create a Color Lookup Table for Trajectory Categories
#'
#' This function generates a tibble that maps trajectory categories to their corresponding color codes.
#' The function returns a tibble containing two columns: 'trajectory' for the categories and 'hex_color' for the color codes.
#'
#' @return A tibble containing trajectory categories and their corresponding color codes
#' @export
#' @importFrom tibble tibble
color_landuse_trajectories <- function() {
  # Create the tibble with trajectory categories and their corresponding colors
  color_lookup_trajectory <- tibble(
    trajectory = c(
      "Stable natural forest",
      "Recovery to forest",
      "Loss to logged-over forest",
      "Other",
      "Recovery to tree cropping",
      "Loss to bare land and abandoned",
      "Loss to cropland",
      "Recovery to agroforest",
      "Loss to infrastructure"
    ),
    color_palette = c(
      "#228B22",  # Green for stable natural forest
      "#ADFF2F",  # Light Green for forest recovery
      "#FF8424",  # Goldenrod for logged-over forest loss
      "#808080",  # Grey for other
      "#8B4513",  # Saddle Brown for tree cropping
      "#FE6AB2",  # Crimson for bare land and abandoned
      "#FFFE00",  # Orange Red for cropland loss
      "#9E00B3",  # Pale Violet Red for agroforest recovery
      "#FF0000"   # Dark Red for infrastructure loss
    )
  )

  # Return the color lookup table
  return(color_lookup_trajectory)
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


# Shiny App Related -------------------------------------------------------

#' Prepare Land Cover Data
#'
#' This function prepares land cover data by reading a raster file, adding a legend,
#' and setting the raster name.
#'
#' @param lc_path Character string. Path to the land cover raster file.
#' @param time_point Numeric. The year of the land cover data.
#' @param lc_lookup Data frame. Lookup table for land cover classes.
#' @param original_name Character string. Original file name of the land cover data.
#'
#' @return SpatRaster object. Prepared land cover raster with added legend.
#'
#' @importFrom terra rast
#' @importFrom tools file_path_sans_ext
#'
#' @export
prepare_lc_data <- function(lc_path, time_point, lc_lookup, original_name) {
  lc_raster <- terra::rast(lc_path)
  lc_raster <- add_legend_to_categorical_raster(lc_raster, lookup_table = lc_lookup, year = time_point)

  # Use the original file name (without extension) to name the raster layer
  names(lc_raster) <- tools::file_path_sans_ext(original_name)

  return(lc_raster)
}

#' Check and Harmonise Land Cover and Administrative Geometries
#'
#' This function checks the geometric consistency between two land cover rasters
#' and an administrative raster. If inconsistencies are found, it harmonizes
#' the geometries to match the first land cover raster (lc_t1).
#'
#' @param lc_t1 SpatRaster. Land cover raster for time point 1 (reference geometry).
#' @param lc_t2 SpatRaster. Land cover raster for time point 2.
#' @param admin SpatRaster. Administrative zones raster.
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item lc_t1: The original lc_t1 SpatRaster (unchanged).
#'     \item lc_t2: The lc_t2 SpatRaster, potentially resampled to match lc_t1.
#'     \item admin: The admin SpatRaster, potentially resampled to match lc_t1.
#'   }
#'
#' @details
#' The function compares the geometries of lc_t2 and admin to lc_t1 using
#' terra::compareGeom(). If inconsistencies are found, it resamples the
#' inconsistent rasters to match lc_t1 using the "near" method.
#'
#' @note
#' This function issues warnings when inconsistencies are detected and
#' messages about the harmonization process.
#'
#' @importFrom terra compareGeom resample
#'
#' @export
check_and_harmonise_geometries <- function(lc_t1, lc_t2, admin) {
  harmonised_layers <- character()

  # Check lc_t2 against lc_t1
  if (!terra::compareGeom(lc_t1, lc_t2, stopOnError = FALSE)) {
    warning("Inconsistent geometry detected for lc_t2. Harmonizing...")
    lc_t2 <- terra::resample(lc_t2, lc_t1, method = "near")
    harmonised_layers <- c(harmonised_layers, "lc_t2")
  }

  # Check admin against lc_t1
  if (!terra::compareGeom(lc_t1, admin, stopOnError = FALSE)) {
    warning("Inconsistent geometry detected for admin. Harmonizing...")
    admin <- terra::resample(admin, lc_t1, method = "near")
    harmonised_layers <- c(harmonised_layers, "admin")
  }

  if (length(harmonised_layers) > 0) {
    message("Harmonization complete. The following layers were harmonised to match lc_t1:")
    message(paste("-", harmonised_layers, collapse = "\n"))
  } else {
    message("All input geometries are consistent.")
  }

  return(list(lc_t1 = lc_t1, lc_t2 = lc_t2, admin = admin))
}

#' Run Pre-QuES Analysis
#'
#' This function performs the main Pre-QuES (Quantifying Ecosystem Services) analysis,
#' including land cover change and trajectory analysis.
#'
#' @param lc_t1_input List. Land cover data for time point 1.
#' @param lc_t2_input List. Land cover data for time point 2.
#' @param admin_z_input SpatRaster or list. Administrative zones data.
#' @param lc_lookup_input List. Land cover lookup table input.
#' @param zone_lookup_input Data frame or list. Zone lookup table input.
#' @param trajectory_lookup_input List. Trajectory rules input.
#' @param time_points List. Time points for analysis (t1 and t2).
#' @param output_dir Character string. Directory to save output files.
#' @param progress_callback Function. Callback function to report progress (optional).
#'
#' @return List containing output_pre_ques, output_pre_ques_traj, and output_pre_ques_def.
#'
#' @importFrom terra rast
#' @importFrom tools file_path_sans_ext
#'
#' @export
run_preques_analysis <- function(lc_t1_input, lc_t2_input, admin_z_input,
                                 lc_lookup_input, zone_lookup_input, trajectory_lookup_input,
                                 time_points, output_dir, progress_callback = NULL) {
  # Run ques-b for lc
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  # Load lookup tables
  lc_lookup <- if(!is.null(lc_lookup_input) && file.exists(lc_lookup_input$datapath)) {
    read.csv(lc_lookup_input$datapath)
  } else {
    stop("Invalid or missing Land Use/Cover Lookup Table")
  }

  lookup_zone <- if(!is.null(zone_lookup_input)) {
    if(inherits(zone_lookup_input, "data.frame")) {
      zone_lookup_input
    } else if(file.exists(zone_lookup_input$datapath)) {
      read.csv(zone_lookup_input$datapath)
    } else {
      NULL
    }
  } else {
    NULL
  }

  lookup_trajectory <- if(!is.null(trajectory_lookup_input) && file.exists(trajectory_lookup_input$datapath)) {
    read.csv(trajectory_lookup_input$datapath)
  } else {
    stop("Invalid or missing Trajectory Rules file")
  }

  # Prepare land cover data
  prepare_lc_data <- function(lc_input, time_point) {
    if(is.null(lc_input) || !file.exists(lc_input$datapath)) {
      stop(paste("Invalid or missing land cover data for time point", time_point))
    }
    lc_raster <- terra::rast(lc_input$datapath)
    lc_raster <- add_legend_to_categorical_raster(lc_raster, lookup_table = lc_lookup, year = time_point)
    names(lc_raster) <- tools::file_path_sans_ext(lc_input$name)
    return(lc_raster)
  }

  lc_data <- list(
    t1 = prepare_lc_data(lc_t1_input, time_points[["t1"]]),
    t2 = prepare_lc_data(lc_t2_input, time_points[["t2"]])
  )

  if (!is.null(progress_callback)) progress_callback(0.2, "Land cover data prepared")


  # Prepare administrative zones data
  if(!inherits(admin_z_input, "SpatRaster")){
    admin_z <- terra::rast(admin_z_input$datapath) %>%
      add_legend_to_categorical_raster(lookup_table = lookup_zone)
    names(admin_z) <- tools::file_path_sans_ext(admin_z_input$name)
  } else {
    admin_z <- admin_z_input
  }



  if (!is.null(progress_callback)) progress_callback(0.3, "Planning unit data prepared")

  harmonised_rasters <-
    check_and_harmonise_geometries(lc_t1 = lc_data$t1, lc_t2 = lc_data$t2, admin = admin_z)

  lc_data$t1 <- harmonised_rasters$lc_t1
  lc_data$t2 <- harmonised_rasters$lc_t2
  admin_z <- harmonised_rasters$admin
 
  # Run main Pre-QuES analysis
  output_pre_ques <- ques_pre(
    lc_data$t1, lc_data$t2, admin_z,
    convert_to_Ha = TRUE
  )

  if (!is.null(progress_callback)) progress_callback(0.5, "Main Pre-QuES analysis completed")
 
  # Run trajectory analysis
  output_pre_ques_traj <- ques_pre_trajectory(
    lc_data$t1, lc_data$t2, admin_z, lc_lookup, lookup_trajectory,
    trajectory_column_name = "trajectory",
    convert_to_Ha = TRUE
  )

  output_pre_ques_def <- ques_pre_trajectory(
    lc_data$t1, lc_data$t1, admin_z, lc_lookup, lookup_trajectory,
    trajectory_column_name = "def",
    convert_to_Ha = TRUE
  )

  if (!is.null(progress_callback)) progress_callback(0.7, "Pre-QuES Trajectory analysis completed")
  
  # End of the script
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  log_params <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    session_log = format_session_info_table()
  )
  
  # Generate and save outputs
  generate_outputs(output_pre_ques, output_pre_ques_traj, output_pre_ques_def,
                   output_dir, lc_data, admin_z, time_points, log_params)

  if (!is.null(progress_callback)) progress_callback(1, "Outputs generated and saved")

  return(list(
    output_pre_ques = output_pre_ques,
    output_pre_ques_traj = output_pre_ques_traj,
    output_pre_ques_def = output_pre_ques_def
  ))
}

#' Generate Outputs for Pre-QuES Analysis
#'
#' This function generates and saves various outputs from the Pre-QuES analysis,
#' including CSV files and raster maps.
#'
#' @param output_pre_ques List. Output from main Pre-QuES analysis.
#' @param output_pre_ques_traj List. Output from trajectory analysis.
#' @param output_pre_ques_def List. Output from deforestation analysis.
#' @param output_dir Character string. Directory to save output files.
#' @param lc_data List. Land cover data for both time points.
#' @param admin_z SpatRaster. Administrative zones data.
#' @param time_points List. Time points used in the analysis.
#'
#' @importFrom terra writeRaster cats
#' @importFrom utils write.csv
#'
#' @export
generate_outputs <- function(output_pre_ques, output_pre_ques_traj, output_pre_ques_def,
                             output_dir, lc_data, admin_z, time_points, log_params) {
  # Create output directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Export the Pre-QuES land use change transition table
  write.csv(output_pre_ques$landscape_level$crosstab_long,
            file.path(output_dir, "PreQuES_luc_change_transition_table.csv"),
            row.names = FALSE)

  # Export Change Trajectory lookup table and raster
  cats(output_pre_ques_traj$landscape_level$luc_trajectory_map)[[1]] %>%
    rename(ChangeTrajectory = trajectory) %>%
    write.csv(file.path(output_dir, "PreQuES_ChangeTrajectory_lookup.csv"),
              row.names = FALSE)
  writeRaster(output_pre_ques_traj$landscape_level$luc_trajectory_map,
              file.path(output_dir, "PreQuES_ChangeTrajectory_map.tif"),
              overwrite = TRUE)

  # Export Forest Change Trajectory lookup table and raster
  cats(output_pre_ques_def$landscape_level$luc_trajectory_map)[[1]] %>%
    rename(ForestChangeTrajectory = def) %>%
    write.csv(file.path(output_dir, "PreQuES_ForestChangeTrajectory_lookup.csv"),
              row.names = FALSE)
  writeRaster(output_pre_ques_def$landscape_level$luc_trajectory_map,
              file.path(output_dir, "PreQuES_ForestChangeTrajectory_map.tif"),
              overwrite = TRUE)

  # Generate Pre-QuES report
  generate_preques_report(output_pre_ques, output_pre_ques_traj, output_pre_ques_def,
                          output_dir, lc_data, admin_z, time_points, log_params)
}

#' Generate Pre-QuES Report
#'
#' This function generates a report for the Pre-QuES analysis using R Markdown.
#'
#' @param output_pre_ques List. Output from main Pre-QuES analysis.
#' @param output_pre_ques_traj List. Output from trajectory analysis.
#' @param output_pre_ques_def List. Output from deforestation analysis.
#' @param output_dir Character string. Directory to save the report.
#' @param lc_data List. Land cover data for both time points.
#' @param admin_z SpatRaster. Administrative zones data.
#' @param time_points List. Time points used in the analysis.
#'
#' @importFrom rmarkdown render
#'
#' @export
generate_preques_report <- function(output_pre_ques, output_pre_ques_traj, output_pre_ques_def,
                                    output_dir, lc_data, admin_z, time_points, log_params) {
  # Set up temporary directory for report generation
  temp_dir <- tempdir()
  # Write raster objects to temporary directory
  dir_landuse <- list(
    t1 = write_rasters_to_adir(lc_data$t1, a_dir = temp_dir),
    t2 = write_rasters_to_adir(lc_data$t2, a_dir = temp_dir)
  )

  dir_zone <- write_rasters_to_adir(admin_z, a_dir = temp_dir)

  # Write trajectory maps to temporary directory
  dir_traj_map <- write_rasters_to_adir(
    output_pre_ques_traj$landscape_level$luc_trajectory_map,
    a_dir = temp_dir)
  dir_def_map <- write_rasters_to_adir(
    output_pre_ques_def$landscape_level$luc_trajectory_map,
    a_dir = temp_dir)

  # Save Pre-QuES output as RDS files
  saveRDS(output_pre_ques, file = file.path(temp_dir, "LUMENS_ques_pre_output.rds"))
  saveRDS(output_pre_ques_traj, file = file.path(temp_dir, "LUMENS_ques_pre_traj_output.rds"))
  saveRDS(output_pre_ques_def, file = file.path(temp_dir, "LUMENS_ques_pre_def_output.rds"))

  # Copy report template and functions to temporary directory
  if (file.exists("../report_template/ques_pre.Rmd")){
    ques_pre_report_path <- "../report_template/ques_pre.Rmd"
    helper_functions_path <- "../rscript/functions_ques_pre.R"
  } else {
    ques_pre_report_path <- "03_preques/report_template/ques_pre.Rmd"
    helper_functions_path <- "03_preques/rscript/functions_ques_pre.R"
  }
  
  file.copy(ques_pre_report_path,
            to = file.path(temp_dir, "PreQuES_report.Rmd"), overwrite = TRUE)
  file.copy(helper_functions_path,
            to = file.path(temp_dir, "functions_ques_pre.R"), overwrite = TRUE)

  # Prepare parameters for report rendering
  report_params <- list(
    dir_lc_t1_ = basename(dir_landuse$t1[[1]]),
    dir_lc_t2_ = basename(dir_landuse$t2[[1]]),
    dir_admin_ = basename(dir_zone[[1]]),
    dir_ques_pre = "LUMENS_ques_pre_output.rds",
    dir_traj_map_ = basename(dir_traj_map[[1]]),
    dir_def_map_ = basename(dir_def_map[[1]]),
    dir_ques_pre_traj = "LUMENS_ques_pre_traj_output.rds",
    dir_ques_pre_def = "LUMENS_ques_pre_def_output.rds",
    cutoff_landscape = 100,
    cutoff_pu = 0,
    log_params= log_params
  )

  # Render the R Markdown report
  if (rmarkdown::pandoc_available()==FALSE){
  Sys.setenv(RSTUDIO_PANDOC=paste0(getwd(), "/pandoc")) 
}
  
  rmarkdown::render(
    input = file.path(temp_dir, "PreQuES_report.Rmd"),
    output_file = "PreQuES_report.html",
    output_dir = output_dir,
    params = report_params
  )
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

#' Process Planning Unit Input
#'
#' This function processes the planning unit input, handling both raster and shapefile inputs.
#'
#' @param zone_type Character string. Type of zone input ("raster" or "shapefile").
#' @param zone_input List or SpatRaster. The input zone data.
#' @param lc_t1_raster SpatRaster. The land cover raster for time point 1.
#'
#' @return List containing zone_raster (SpatRaster) and lookup_zone (data frame or NULL).
#'
#' @importFrom terra rast resample
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr rename
#'
#' @export
process_planning_unit <- function(zone_type, zone_input, lc_t1_raster) {
  if (zone_type == "raster") {
    zone_raster <- as.factor(terra::rast(zone_input$datapath))
    lookup_zone <- NULL
  } else {
    sf_object <- read_shapefile(zone_input)

    if (is.null(sf_object)) {
      stop("Failed to read shapefile. Please check your input.")
    }

    # Rename columns
    sf_object <- sf_object %>%
      dplyr::rename(Value = 1, planning_unit = 2)

    # Create lookup table from shapefile attributes
    lookup_zone <- sf::st_drop_geometry(sf_object)

    lc_t1_res <- terra::res(lc_t1_raster)
    zone_raster <- rasterise_multipolygon(sf_object, raster_res = lc_t1_res, field = "Value")
  }

  # Ensure the planning unit raster matches the LC T1 raster
  zone_raster <- terra::resample(zone_raster, lc_t1_raster, method = "near")
  return(list(zone_raster = zone_raster, lookup_zone = lookup_zone))
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
  platform_os <- paste(si$platform, "|", si[[6]])

  # Extract locale info
  locale_info <- strsplit(si[[3]], ";")[[1]]
  locale_info <- paste(locale_info, collapse = "<br>")

  # Extract .libpaths
  lib_paths <- .libPaths() |> paste( collapse = "<br>")

  # Combine all info into a single tibble
  session_summary <- tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details = c(r_version, platform_os, lib_paths, locale_info)
  )



  return(session_summary)
}

