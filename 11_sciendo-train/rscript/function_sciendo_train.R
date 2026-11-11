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

### Required Library ####
#' Install Required Library
#' 
#' Checks if a list of required packages are installed and loaded.
#'
#' @param package1 list of 
#' @param ... parameters to be passed to vector of packages
#'
#' @return None. This function is called for its side effects.
#' @export
#'
#' @examples
install_load <- function (package1, ...)  {
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  for (package in packages) {
    # if package is installed locally, load
    if (package %in% rownames(installed.packages()))
      do.call('library', list(package))
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}


#' Spatially Sync Rasters
#' 
#' Aligns ("syncs") a Raster to a reference Raster.
#'
#' @param unsynced A Raster object to be aligned to the reference raster
#' @param reference A Raster object to be used as the reference for syncing. Syncing will use the reference's projection, resolution, and extent
#' @param method Method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor) or 'bilinear' (bilinear interpolation)
#' @param size_only 
#' @param raster_size 
#' @param verbose verbose=TRUE gives feedback on the process (UNSUPPORTED AT PRESENT)
#' @param ... parameters to be passed to writeRaster
#'
#' @return Returns a RasterLayer, RasterBrick or RasterStack object synced to the reference raster object.
#' @importFrom raster projection res bbox rotate projectExtent setExtent resample extend extent crop
#' 
#' @export
spatial_sync_raster <- function(unsynced, reference, method="ngb", size_only=FALSE, raster_size, verbose=FALSE, ...) {
  if(!size_only) {
    new_projection=projection(reference)
    old_projection=projection(unsynced)
    
    new_res=res(reference)
    old_res=res(unsynced)
    
    # Check for rotation
    new_extent=bbox(reference)
    old_extent=bbox(unsynced)
    
    if((new_extent[1,1] < 0 && old_extent[1,1] >=0) || (new_extent[1,1] >= 0 && old_extent[1,1] <0)) {
      if(verbose) { message ("Rotating...") }
      unsynced_rotated=rotate(unsynced)
    } else
    {
      unsynced_rotated=unsynced
    }
    
    if(new_projection!=old_projection | new_res[1] != old_res[1] | new_res[2] != old_res[2])
    {
      pr_extent=projectExtent(unsynced_rotated, new_projection)
      # We need to fix the extent
      pr_extent <- setExtent(pr_extent,extent(reference))
      res(pr_extent)=res(reference)
      if(new_projection!=old_projection)
      {
        if(verbose) { message("Projecting and resampling...") }
        pr <- projectRaster(unsynced_rotated, pr_extent,method=method)
      } else
      {
        if(verbose) { message("Same projection, resampling only...") }
        pr <- raster::resample(unsynced_rotated, pr_extent,method=method)
      }
    } else
    {
      if(verbose) { message("Same projection and pixel size...") }
      pr=unsynced_rotated
    }
    
    if(verbose) { message("Expanding...") }
    expanded_raster=extend(pr,reference)
    if(verbose) { message("Cropping...") }
    synced_raster=crop(expanded_raster,reference)
    
    # This in theory shouldn't be neccesasary...
    if(verbose) { message("Fixing extents...") }
    extent(synced_raster)=extent(reference)
  } else {
    #		if(missing(raster_size))
    #		{
    #			stop("For size_only=TRUE you must set the raster_size as c(ncol,nrow)")
    #		} 
    
    unsynced_ncol=ncol(unsynced)
    unsynced_nrow=nrow(unsynced)
    
    # Eventually we should preserve the pixel size		
    unsynced_ulx=(raster_size[[1]]-unsynced_ncol)/2
    unsynced_uly=(raster_size[[2]]-unsynced_nrow)/2
    
    extent(unsynced)=extent(unsynced_ulx,unsynced_ulx+unsynced_ncol,unsynced_uly,unsynced_uly+unsynced_nrow)
    full_extent=extent(0,raster_size[[1]],0,raster_size[[2]])
    
    synced_raster=extend(unsynced,full_extent)
    extent(synced_raster)=full_extent
    res(synced_raster)=c(1,1)
  }
  
  return(synced_raster)
}


#' Generate Dummy Cross-tabulate
#' 
#' Cross-tabulate two data.frame objects to create a contingency table.
#'
#' @param landcover List. Land cover lookup table input.
#' @param zone Data frame or list. Zone lookup table input.
#'
#' @return A table or data.frame
#' @export
generate_dummy_crosstab <- function(landcover, zone){
  if(!is.data.frame(landcover)) {
    stop("Land cover is not a data frame")
  }
  
  if(!is.data.frame(zone)) {
    stop("Zone is not a data frame")
  }
  
  n_lc <- nrow(landcover)
  n_pu <- nrow(zone)
  
  dummy1 <- data.frame(nPU = zone[,1], divider = n_lc*n_lc)
  dummy1 <- expandRows(dummy1, 'divider')
  
  dummy2 <- data.frame(nT1 = landcover[,1], divider = n_lc)
  dummy2 <- expandRows(dummy2, 'divider')
  dummy2 <- data.frame(nT1 = rep(dummy2$nT1, n_pu))
  
  dummy3 <- data.frame(nT2 = rep(rep(landcover[,1], n_lc), n_pu))
  
  lucDummy <- cbind(dummy1, dummy2, dummy3)
  colnames(lucDummy) <- c('ID_PU', 'ID_LC1', 'ID_LC2')
  return(lucDummy)
}

#' Plot a categorical raster with an optional download button
#'
#' This function creates a categorical map from a raster object using
#' **ggplot2** and **tidyterra**. In HTML outputs (e.g., R Markdown HTML reports),
#' the plot is rendered as an inline image with a **Download PNG** button.
#' In non-HTML outputs (e.g., PDF, Word), only the `ggplot` object is returned.
#'
#' If the raster's category table contains a `color_palette` column with valid
#' hex codes, those colors are used for plotting. Otherwise, a default palette
#' is applied. The plot legend is automatically formatted for readability.
#'
#' @param raster_object A [`SpatRaster`][terra::SpatRaster] object containing
#'   categorical data. Should include category labels, and optionally a
#'   `color_palette` column in `cats(raster_object)`.
#' @param filename A string giving the default filename (with extension) for
#'   the downloaded PNG in HTML output. Defaults to `"raster_plot.png"`.
#' @param dpi An integer giving the resolution (dots per inch) for the saved
#'   PNG image in HTML output. Defaults to `300`.
#'
#' @return 
#' - If the output format is **HTML**: an [htmltools::tagList] containing the 
#'   rendered raster plot and a styled download button.
#' - If the output format is **non-HTML** (PDF, Word, etc.): a `ggplot` object
#'   that can be further modified or printed.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' r <- rast(matrix(sample(1:3, 100, TRUE), 10, 10))
#' cats(r) <- data.frame(ID = 1:3, class = c("Forest", "Agriculture", "Urban"))
#'
#' # Returns ggplot in non-HTML output
#' plot_categorical_raster(r)
#'
#' # In HTML output, adds download button
#' plot_categorical_raster(r, filename = "landcover_map.png", dpi = 200)
#' }
#'
#' @export
EOX_ATTRIBUTION <- "EOxCloudless - https://cloudless.eox.at by EOX IT Services GmbH (Contains modified Copernicus Sentinel data 2025)"
EOX_TILE_URL <- "https://tiles.maps.eox.at/wmts/1.0.0/s2cloudless-2025_3857/default/g/{z}/{y}/{x}.jpg"

BASEMAP_PROVIDERS <- list(
  eox = list(
    id = "eox",
    name = "Satelit",
    sublabel = "Sentinel-2 Cloudless 2025",
    url = EOX_TILE_URL,
    attribution = EOX_ATTRIBUTION,
    maxZoom = 18
  )
)

#' Interactive Categorical Raster Map with EOX Cloudless 2025 Satellite Basemap
#'
#' Builds an interactive mapview / leaflet map for a categorical raster,
#' applying custom or predefined colors, layer checklist control, and extent reset.
#'
#' @param cat_raster A `SpatRaster` object.
#' @param cat_table A data frame with at least two columns: ID and Category.
#' @param layer_title Character string for the layer name in mapview and legend.
#' @return A `mapview` object with an added legend and satellite basemap.
#' @importFrom mapview mapview
#' @importFrom leaflet colorFactor addLegend clearTiles addTiles addLayersControl addEasyButton easyButton JS fitBounds
#' @export
plot_categorical_raster_mapview <- function(cat_raster, cat_table,
                                            layer_title = "Layer") {
  names(cat_table)[1] <- "ID"
  names(cat_table)[2] <- "Category"
  cat_table$ID <- as.numeric(cat_table$ID)
  
  unique_values    <- unique(values(cat_raster, na.rm = TRUE))
  cat_tbl_filtered <- cat_table[cat_table$ID %in% unique_values, ]
  
  reclass_from   <- cat_tbl_filtered$ID
  reclass_to     <- seq_along(cat_tbl_filtered$ID)
  reclass_matrix <- cbind(reclass_from, reclass_to)
  
  cat_reclass <- classify(cat_raster, reclass_matrix, others = NA)
  cat_factor  <- as.factor(cat_reclass)
  
  levels_df <- data.frame(
    ID       = reclass_to,
    Category = factor(cat_tbl_filtered$Category,
                      levels = cat_tbl_filtered$Category)
  )
  
  levels(cat_factor) <- levels_df
  activeCat(cat_factor) <- "Category"
  
  predefined_colors <- c(
    "#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", "#911EB4", "#46F0F0", "#F032E6",
    "#BCF60C", "#FABEBE", "#008080", "#E6BEFF", "#9A6324", "#FFFAC8", "#800000", "#AAFFC3",
    "#808000", "#FFD8B1", "#000075", "#A9A9A9", "#231F20", "#1170AA", "#A2C8EC", "#FFBC79",
    "#C85200", "#FF9DA7", "#D55E00", "#59A14F", "#EDC948", "#B07AA1", "#9C755F", "#BAB0AC",
    "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666",
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
    "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
    "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F",
    "#2F4F4F", "#556B2F", "#A0522D", "#191970", "#006400", "#8B0000", "#4B0082", "#008B8B",
    "#00008B", "#FF4500", "#FF8C00", "#FFD700", "#00FF00", "#00FA9A", "#00FFFF", "#00BFFF",
    "#0000FF", "#FF00FF", "#1E90FF", "#DDA0DD", "#FF1493", "#98FB98", "#708090", "#D2B48C",
    "#00FF7F", "#4682B4", "#D2691E", "#9932CC", "#87CEEB", "#FF69B4", "#CD5C5C", "#E9967A",
    "#8A2BE2", "#2E8B57", "#228B22", "#DC143C"
  )
  
  if ("color_palette" %in% names(cat_tbl_filtered)) {
    map_colors <- cat_tbl_filtered$color_palette
  } else {
    if (nrow(cat_tbl_filtered) > length(predefined_colors)) {
      map_colors <- grDevices::colorRampPalette(predefined_colors)(nrow(cat_tbl_filtered))
    } else {
      map_colors <- predefined_colors[1:nrow(cat_tbl_filtered)]
    }
  }
  
  color_table <- data.frame(value = reclass_to, color = map_colors)
  coltab(cat_factor) <- color_table
  
  # Calculate initial bounds in EPSG:4326 for reset button & fitBounds
  initial_bb <- tryCatch({
    poly_sf <- sf::st_as_sfc(sf::st_bbox(cat_raster))
    poly_4326 <- sf::st_transform(poly_sf, 4326)
    sf::st_bbox(poly_4326)
  }, error = function(e) NULL)
  
  # Initialize base map with EOX Satellite tiles FIRST so raster renders ON TOP
  m_base <- leaflet::leaflet() %>%
    leaflet::addTiles(
      urlTemplate = BASEMAP_PROVIDERS$eox$url,
      attribution = BASEMAP_PROVIDERS$eox$attribution,
      group       = BASEMAP_PROVIDERS$eox$name,
      options     = leaflet::tileOptions(maxZoom = BASEMAP_PROVIDERS$eox$maxZoom)
    )
  
  map_result <- mapview::mapview(
    cat_factor,
    map        = m_base,
    zcol       = "Category",
    maxpixels  = ncell(cat_factor),
    layer.name = layer_title,
    na.color   = "transparent",
    legend     = FALSE
  )
  
  pal <- leaflet::colorFactor(palette = color_table$color, domain = levels_df$Category)
  leaflet_map <- map_result@map
  
  if (!is.null(initial_bb)) {
    leaflet_map <- leaflet_map %>%
      leaflet::fitBounds(
        lng1 = unname(initial_bb$xmin),
        lat1 = unname(initial_bb$ymin),
        lng2 = unname(initial_bb$xmax),
        lat2 = unname(initial_bb$ymax)
      )
  }
  
  leaflet_map <- leaflet_map %>%
    leaflet::addLayersControl(
      baseGroups = BASEMAP_PROVIDERS$eox$name,
      overlayGroups = layer_title,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addLegend(
      position = "topright",
      pal      = pal,
      values   = levels_df$Category,
      title    = layer_title
    )
  
  # Add return to initial zoom level extent button
  if (!is.null(initial_bb)) {
    leaflet_map <- leaflet_map %>%
      leaflet::addEasyButton(
        leaflet::easyButton(
          icon = "fa-globe",
          title = "Kembalikan ke Batas / Zoom Awal (Reset Zoom)",
          onClick = leaflet::JS(sprintf(
            "function(btn, map){ map.fitBounds([[%f, %f], [%f, %f]]); }",
            initial_bb$ymin, initial_bb$xmin, initial_bb$ymax, initial_bb$xmax
          ))
        )
      )
  }
  
  map_result@map <- leaflet_map
  map_result
}


#'
#' @export
plot_categorical_raster <- function(raster_object, filename = "raster_plot.png", dpi = 300) {
  default_palette <- c(
    "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
    "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
    "#86BCB6", "#FFB84D", "#A5C1DC", "#D37295", "#C4AD66",
    "#7B8D8E", "#B17B62", "#8CD17D", "#DE9D9C", "#5A5A5A",
    "#A0A0A0", "#D7B5A6", "#6D9EEB", "#E69F00", "#56B4E9",
    "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
    "#999999", "#E51E10", "#FF7F00", "#FFFF33", "#A65628",
    "#F781BF", "#999933", "#8DD3C7", "#FFFFB3", "#BEBADA",
    "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
    "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#E41A1C"
  )
  
  has_custom_colors <- length(cats(raster_object)) > 0 &&
    !is.null(cats(raster_object)[[1]]) &&
    "color_palette" %in% names(cats(raster_object)[[1]]) &&
    all(grepl("^#[0-9A-Fa-f]{6}$", cats(raster_object)[[1]]$color_palette))
  
  if (has_custom_colors) {
    fill_scale <- scale_fill_manual(
      values = cats(raster_object)[[1]]$color_palette,
      na.value = "transparent",
      labels = scales::label_wrap(30)
    )
  } else {
    fill_scale <- scale_fill_manual(
      values = default_palette,
      na.value = "transparent",
      labels = scales::label_wrap(30)
    )
  }
  
  if (!is.na(time(raster_object))) {
    plot_title <- time(raster_object)
  } else {
    plot_title <- names(raster_object)
  }
  
  n_cats <- if (length(cats(raster_object)) > 0 && !is.null(cats(raster_object)[[1]])) {
    nrow(cats(raster_object)[[1]])
  } else {
    10
  }
  legend_ncol <- if (n_cats > 18) 2 else 1
  
  # Generate the plot
  plot_lc <- ggplot() +
    tidyterra::geom_spatraster(data = raster_object) +
    fill_scale +
    coord_sf(expand = FALSE) +
    theme_bw() +
    labs(title = plot_title, fill = NULL) +
    guides(
      fill = guide_legend(
        title.position = "top",
        ncol = legend_ncol,
        keywidth = grid::unit(0.4, "cm"),
        keyheight = grid::unit(0.35, "cm")
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 8.5),
      legend.position = "right",
      legend.justification = c(0, 0.5),
      legend.box.spacing = grid::unit(0.4, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
  
  if (!knitr::is_html_output()) {
    return(plot_lc)
  }
  
  # Save PNG with dynamic dimensions to prevent squishing and legend cropping
  tf <- tempfile(fileext = ".png")
  plot_width <- if (legend_ncol > 1) 10.5 else 9.0
  plot_height <- max(5.5, min(9.0, 4.0 + (n_cats / legend_ncol) * 0.18))
  ggsave(tf, plot_lc, width = plot_width, height = plot_height, dpi = dpi)
  img_data <- base64enc::dataURI(file = tf, mime = "image/png")
  
  # download button
  htmltools::tagList(
    tags$div(
      style = "margin-bottom:10px;",
      tags$img(
        src = img_data,
        style = "max-width:100%; height:auto; display:block; margin-bottom:5px;"
      ),
      tags$button(
        "Download PNG",
        onclick = sprintf(
          "var link = document.createElement('a'); link.download = '%s';
          link.href = this.previousElementSibling.src; link.click();",
          filename
        ),
        style = "padding:4px 8px; font-size:0.9em;
                 background:#d3d3d3; border-radius:4px;
                 color:#333333; text-decoration:none;
                 border: none; outline: none;"
      )
    )
  )
}


create_list_of_weight_report <- function(woe_report_path, list_woe_report, df_zone, lc_lookup_table) {
  listWoeReport <- list_woe_report
  woe <- list()
  len <- nrow(df_zone)
  
  vec <- c()
  for(j in listWoeReport) {
    number <- as.numeric(gsub("\\.csv$", "", gsub("^.*weight_report", "", j)))
    vec <- c(vec, number)
  }
  
  for(counter in 1:length(vec)){
    i <- vec[counter]
    pu_match <- which(df_zone[[1]] == i)
    pu_name <- if (length(pu_match) > 0) as.character(df_zone[[2]][pu_match[1]]) else paste0("PU ", i)
    woe[[paste0("pu", sprintf("%03d", i))]][['name']] <- pu_name
    woe[[paste0("pu", sprintf("%03d", i))]][['report']] <- listWoeReport[counter] %>%
      read.csv() %>%
      dplyr::select(-any_of(c("X", "X."))) %>%
      dplyr::left_join(lc_lookup_table, by = join_by(Transition_From. == ID_LC)) %>%
      dplyr::rename(LC_FROM = LC) %>%
      dplyr::left_join(lc_lookup_table, by = join_by(Transition_To. == ID_LC)) %>%
      dplyr::rename(LC_TO = LC) %>%
      mutate(
        Transition = paste(LC_FROM, "->", LC_TO),
        Range = paste(Range_Lower_Limit., "<= v <", Range_Upper_Limit.),
        Significant = if_else(Significant == 1, "yes", "no")
      ) %>%
      dplyr::select(Transition, Variable., Range, Possible_Transitions, Executed_Transitions, Weight_Coefficient, Contrast, Significant)
    woe[[paste0("pu", sprintf("%03d", i))]][['unique_transition']] <- woe[[paste0("pu", sprintf("%03d", i))]][['report']] %>% 
      distinct(Transition) %>% unlist() %>% as.vector()
  }
  woe_out <- list(
    woe = woe,
    n_zone = len,
    df_zone = df_zone
  )
  return(woe_out)
}

#' Generate SCIENDO-Train Report
#' 
#' Generates a report for the SCIENDO-Train analysis using R Markdown.
#'
#' @param output List. Output from SCIENDO-Train analysis.
#' @param dir Character string. Directory to save the report.
#' 
#' @importFrom rmarkdown render
#'
#' @export
generate_sciendo_train_report <- function(output, dir) {
  report_params <- list(
    start_time = output$start_time,
    end_time = output$end_time,
    inputs = output$inputs,
    rc_path = output$rc_path,
    woe_report_path = output$woe_report_path,
    rc_egoml_path = output$rc_egoml_path,
    woe_egoml_path = output$woe_egoml_path,
    woe_list = output$woe_list, 
    session_log = output$session_log
  )
  output_file <- paste0("sciendo_train_report_", Sys.Date(), ".html")
  
  rmarkdown::render(
    "../report_template/sciendo_train_report_template_INA.Rmd",
    output_file = output_file,
    output_dir = dir,
    params = report_params
  )
}


executeDINAMICA <- function(params, memory_allocation) {
  # # Find DINAMICA directory if not provided
  # if (is.null(params$dinamica_path) | identical(params$dinamica_path, character(0))) {
  #   program_files <- c("C:/Program Files/", "C:/Program Files (x86)/")
  #   dinamica_dirs <- list.files(program_files, pattern = "^Dinamica EGO", full.names = TRUE)
  #   
  #   if (length(dinamica_dirs) == 0) {
  #     stop("No DINAMICA EGO installation found.")
  #   }
  #   
  #   # Sort directories to use the latest version if multiple are found
  #   dinamica_path <- sort(dinamica_dirs, decreasing = TRUE)[1]
  # }
  
  dinamica_path <- params$dinamica_path
  message(paste("Using DINAMICA EGO installation:", dinamica_path))
  
  # Check if DINAMICA directory exists
  if (!dir.exists(dinamica_path)) {
    stop("Specified DINAMICA EGO directory does not exist.")
  }
  
  # Find DinamicaConsole
  dinamica_exe <- dinamica_path %>% 
    list.files(pattern = "^DinamicaConsole", full.names = TRUE) %>%
    nth(2)
  
  # Check if egoml exists
  if (!file.exists(params$egoml)) {
    stop("Specified egoml does not exist.")
  }
  
  # Prepare DINAMICA command
  # command<-paste('"', dinamica_exe, '" -processors 0 -log-level 4 -memory-allocation-policy 1 "', params$egoml, '"', sep="")
  command <- paste(
    '"', dinamica_exe, 
    '" -processors 0 -log-level 4 -memory-allocation-policy ', 
    memory_allocation, 
    ' "', params$egoml, '"', 
    sep = ""
  )
  
  # Execute DINAMICA
  result <- system(command)
  
  if(result != 0) {
    stop("DINAMICA EGO execution failed. Check DINAMICA EGO installation and parameters.")
  } else {
    message("DINAMICA EGO execution completed successfully.")
  }
}

generate_egoml_transition_matrix <- function(lc1_path, lc2_path, 
                                             zone_path, timestep, output_dir, egoml) {
  mtx_dir <- paste0(output_dir, "/baseline_tpm")
  dir.create(mtx_dir, recursive = TRUE, showWarnings = FALSE, mode = "0777")
  
  # begin writing tag
  con <- xmlOutputDOM(tag="script")
  # add property
  con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-17 12:02:15"))
  con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))
  
  # begin.
  # add functor = LoadCategoricalMap-PU
  con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Regions"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Municipalities"))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', lc1_path, '"', sep=''))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), 0)
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id=paste("v",1,sep="")))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add functor = LoadCategoricalMap-LANDUSE_1
  con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Initial landscape map"))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', lc2_path, '"', sep=''))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), 0)
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id=paste("v",2,sep="")))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add functor = LoadCategoricalMap-LANDUSE_2
  con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Final landscape map"))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', zone_path, '"', sep=''))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), 0)
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id=paste("v",3,sep="")))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add containerfunctor = ForEachRegion
  con$addTag("containerfunctor", attrs=c(name="ForEachRegion"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachRegion"))
  con$addTag("inputport", attrs=c(name="regions", peerid="v3"))
  con$addTag("inputport", attrs=c(name="borderCells"), 0)
  con$addTag("internaloutputport", attrs=c(name="regionManager", id="v4"))
  con$addTag("internaloutputport", attrs=c(name="step", id="v5"))
  
  # add subtag functor for Landuse1
  con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape (Region)"))
  con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v5"))
  con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
  con$addTag("outputport", attrs=c(name="regionalMap", id="v6"))
  con$closeTag("functor")
  
  # add subtag functor for Landuse2
  con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape (Region)"))
  con$addTag("inputport", attrs=c(name="globalMap", peerid="v2"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v5"))
  con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
  con$addTag("outputport", attrs=c(name="regionalMap", id="v7"))
  con$closeTag("functor")
  
  # add subtag functor for DetermineTransitionMatrix
  con$addTag("functor", attrs=c(name="DetermineTransitionMatrix"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Transition Rates"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Calculate the transition rates"))
  con$addTag("inputport", attrs=c(name="initialLandscape", peerid="v6"))
  con$addTag("inputport", attrs=c(name="finalLandscape", peerid="v7"))
  con$addTag("inputport", attrs=c(name="timeSteps"), timestep)
  con$addTag("outputport", attrs=c(name="singleStepMatrix", id="v8"))
  con$addTag("outputport", attrs=c(name="multiStepMatrix", id="v9"))
  con$closeTag("functor")
  
  # add subtag functor for SaveTable
  con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable567"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Single-step transition matrix."))
  con$addTag("inputport", attrs=c(name="table", peerid="v8"))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', mtx_dir, '/single_step.csv"', sep=''))
  con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
  con$addTag("inputport", attrs=c(name="step", peerid="v5"))
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$closeTag("functor")
  
  # add subtag functor for SaveTable
  # con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
  # con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable566"))
  # con$addTag("property", attrs=c(key="dff.functor.comment", value="Multi-step transition matrix."))
  # con$addTag("inputport", attrs=c(name="table", peerid="v9"))
  # con$addTag("inputport", attrs=c(name="filename"), paste('"', transition_dir, '/Multi_step.csv"', sep=''))
  # con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
  # con$addTag("inputport", attrs=c(name="step", peerid="v5"))
  # con$addTag("inputport", attrs=c(name="workdir"), ".none")
  # con$closeTag("functor")
  
  con$closeTag("containerfunctor")  
  
  egoml_mtx_file <- paste0(output_dir, "/", egoml, ".egoml")
  saveXML(con$value(), file = egoml_mtx_file)
  
  return(egoml_mtx_file)
}

run_dinamica_transition_matrix <- function(dinamica_path = NULL, output_dir, egoml, memory_allocation) {
  params <- list()
  params$dinamica_path <- dinamica_path
  params$output_dir <- output_dir
  params$egoml <- egoml
  
  executeDINAMICA(params, memory_allocation)
}

generate_egoml_raster_cube <- function(factor_path, output_dir, egoml) {
  # preparing factors
  listFactors <- factor_path %>% list.files(full.names = TRUE, pattern = ".tif$") %>%
    data.frame(file = ., select = 1)

  factors <- as.character(listFactors$file)
  nFactors <- length(factors)

  aliasFactor<-NULL
  for (a in 1:nFactors) {
    temp <- substr(basename(factors[a]), 1, nchar(basename(factors[a])) - 4)
    aliasFactor <- c(aliasFactor, temp)
  }

  # create raster cube egoml
  # begin writing tag
  con <- xmlOutputDOM(tag = "script")
  # add property
  con$addTag("property",
             attrs = c(key = "dff.date", value = "2016-Oct-17 12:02:15"))
  con$addTag("property",
             attrs = c(key = "dff.version", value = "3.0.17.20160922"))

  # begin.
  # add functor = SaveMap
  con$addTag("functor",
             attrs = c(name = "SaveMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "saveMap1680"))
  con$addTag("inputport", attrs = c(name = "map", peerid = paste("v", nFactors + 1, sep = "")))
  ers_file <- paste0('"', output_dir, '/sciendo_factors.tif"')
  con$addTag(
    "inputport",
    attrs = c(name = "filename"),
    ers_file
  )
  con$addTag("inputport", attrs = c(name = "suffixDigits"), 2)
  con$addTag("inputport", attrs = c(name = "step"), ".none")
  con$addTag("inputport", attrs = c(name = "useCompression"), ".yes")
  con$addTag("inputport", attrs = c(name = "workdir"), ".none")
  con$closeTag("functor")
  # end.

  # begin.
  # add functor = LoadMap
  for (b in 1:nFactors) {
    con$addTag("functor",
               attrs = c(name = "LoadMap"),
               close = FALSE)
    con$addTag("property",
               attrs = c(key = "dff.functor.alias", value = aliasFactor[b]))
    con$addTag("inputport",
               attrs = c(name = "filename"),
               paste('"', factors[b], '"', sep = ""))
    con$addTag("inputport", attrs = c(name = "nullValue"), ".none")
    con$addTag("inputport", attrs = c(name = "loadAsSparse"), ".no")
    con$addTag("inputport", attrs = c(name = "suffixDigits"), 0)
    con$addTag("inputport", attrs = c(name = "step"), ".none")
    con$addTag("inputport", attrs = c(name = "workdir"), ".none")
    con$addTag("outputport", attrs = c(name = "map", id = paste("v", b, sep ="")))
    con$closeTag("functor")
  }
  # end.

  # begin.
  # add containerfunctor = CreateCubeMap
  con$addTag("containerfunctor",
             attrs = c(name = "CreateCubeMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "createCubeMap1678"))
  con$addTag("inputport", attrs = c(name = "cellType"), ".float32")
  con$addTag("inputport", attrs = c(name = "nullValue"), "-9999")
  con$addTag("outputport", attrs = c(name = "map", id = paste("v", nFactors +1, sep = "")))
  # add subtag functor for CreateCubeMap
  for (c in 1:nFactors) {
    con$addTag("functor",
               attrs = c(name = "NumberAndNameMap"),
               close = FALSE)
    con$addTag("property",
               attrs = c(key = "dff.functor.alias", value = aliasFactor[c]))
    con$addTag("inputport", attrs = c(name = "map", peerid = paste("v", c, sep = "")))
    con$addTag("inputport",
               attrs = c(name = "mapName"),
               paste('"', aliasFactor[c], '"', sep = ""))
    con$addTag("inputport", attrs = c(name = "mapNumber"), 0)
    con$closeTag("functor")
  }
  con$closeTag("containerfunctor")
  # end.

  egoml_rc_file <- paste0(output_dir, "/", egoml, ".egoml")
  saveXML(con$value(), file = egoml_rc_file)

  out <- list(
    egoml_rc_file = egoml_rc_file,
    n = nFactors,
    list = listFactors,
    alias = aliasFactor,
    ers = ers_file
  )

  return(out)
}

run_dinamica_raster_cube <- function(dinamica_path = NULL, output_dir, egoml, memory_allocation) {
  params <- list()
  params$dinamica_path <- dinamica_path
  params$output_dir <- output_dir
  params$egoml <- egoml
  
  executeDINAMICA(params, memory_allocation)
  
  # check raster cube file 
  ers_file <- paste0(output_dir, "/sciendo_factors.tif")
  if (!file.exists(ers_file)) {
    stop("Raster cube creation failed! Check DINAMICA EGO log.")
  }
}

generate_egoml_woe_model <- function(aliasFactor, lusim_lc, 
                                     lc1_path, lc2_path, 
                                     zone_path, ers_path,
                                     output_dir, egoml) {
  woe_dir <- paste0(output_dir, "/woe")
  dir.create(woe_dir, recursive = TRUE, showWarnings = FALSE, mode = "0777")
  
  dcf_path <- paste0(woe_dir, "/woe.dcf")
  weight_report_path <- paste0(woe_dir, "/weight_report.csv")
  
  static_var <- aliasFactor %>% 
    data.frame(aliasFactor = .) %>% 
    mutate(
      identifier = paste0('&quot;static_var/', aliasFactor, '&quot; 10 500000 1 5,&#x0A;')
    )
  identifier <- do.call(paste, c(as.list(static_var$identifier), sep="        "))
  
  skeleton <- expand.grid(nT1 = lusim_lc[, 1], nT2 = lusim_lc[, 1])
  skeleton$key <- do.call(paste, c(skeleton[c("nT1", "nT2")], sep = "-&gt;"))
  
  skeleton$transition <- paste("&#x0A;    ", skeleton$key, " [&#x0A;        ", identifier, "    ]", sep = '')
  
  skeletonFinal <- do.call(paste, c(as.list(skeleton$transition), sep = ","))
  skeletonFinal <- paste('[', skeletonFinal, "&#x0A;]", sep = '')
  
  # begin writing tag
  con <- xmlOutputDOM(tag = "script")
  # add property
  con$addTag("property",
             attrs = c(key = "dff.date", value = "2016-Oct-18 12:59:40"))
  con$addTag("property",
             attrs = c(key = "dff.version", value = "3.0.17.20160922"))
  
  # begin.
  # add functor = LoadCategoricalMap
  con$addTag("functor",
             attrs = c(name = "LoadCategoricalMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "Final Landscape"))
  con$addTag("inputport",
             attrs = c(name = "filename"),
             paste0('"', lc2_path, '"'))
  con$addTag("inputport", attrs = c(name = "nullValue"), ".none")
  con$addTag("inputport", attrs = c(name = "loadAsSparse"), ".no")
  con$addTag("inputport", attrs = c(name = "suffixDigits"), 0)
  con$addTag("inputport", attrs = c(name = "step"), "0")
  con$addTag("inputport", attrs = c(name = "workdir"), ".none")
  con$addTag("outputport", attrs = c(name = "map", id = "v1"))
  con$closeTag("functor")
  # end.
  
  
  # begin.
  # add functor = LoadCategoricalMap
  con$addTag("functor",
             attrs = c(name = "LoadCategoricalMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "Initial Landscape"))
  con$addTag("inputport",
             attrs = c(name = "filename"),
             paste0('"', lc1_path, '"'))
  con$addTag("inputport", attrs = c(name = "nullValue"), ".none")
  con$addTag("inputport", attrs = c(name = "loadAsSparse"), ".no")
  con$addTag("inputport", attrs = c(name = "suffixDigits"), 0)
  con$addTag("inputport", attrs = c(name = "step"), "0")
  con$addTag("inputport", attrs = c(name = "workdir"), ".none")
  con$addTag("outputport", attrs = c(name = "map", id = "v2"))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add functor = LoadMap
  con$addTag("functor",
             attrs = c(name = "LoadMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "Static Variables"))
  con$addTag("inputport", attrs = c(name = "filename"), ers_path)
  con$addTag("inputport", attrs = c(name = "nullValue"), ".none")
  con$addTag("inputport", attrs = c(name = "loadAsSparse"), ".no")
  con$addTag("inputport", attrs = c(name = "suffixDigits"), 0)
  con$addTag("inputport", attrs = c(name = "step"), "0")
  con$addTag("inputport", attrs = c(name = "workdir"), ".none")
  con$addTag("outputport", attrs = c(name = "map", id = "v3"))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add functor = LoadCategoricalMap
  con$addTag("functor",
             attrs = c(name = "LoadCategoricalMap"),
             close = FALSE)
  con$addTag("property", attrs = c(key = "dff.functor.alias", value = "Regions"))
  con$addTag("inputport",
             attrs = c(name = "filename"),
             paste0('"', zone_path, '"'))
  con$addTag("inputport", attrs = c(name = "nullValue"), ".none")
  con$addTag("inputport", attrs = c(name = "loadAsSparse"), ".no")
  con$addTag("inputport", attrs = c(name = "suffixDigits"), 0)
  con$addTag("inputport", attrs = c(name = "step"), "0")
  con$addTag("inputport", attrs = c(name = "workdir"), ".none")
  con$addTag("outputport", attrs = c(name = "map", id = "v4"))
  con$closeTag("functor")
  # end.
  # begin.
  
  # begin.
  # add containerfunctor = ForEachRegion
  con$addTag("containerfunctor",
             attrs = c(name = "ForEachRegion"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "forEachRegion"))
  con$addTag("inputport", attrs = c(name = "regions", peerid = "v4"))
  con$addTag("inputport", attrs = c(name = "borderCells"), 0)
  con$addTag("internaloutputport", attrs = c(name = "regionManager", id = "v5"))
  con$addTag("internaloutputport", attrs = c(name = "step", id = "v6"))
  
  # add subtag functor for SaveWeights
  con$addTag("functor",
             attrs = c(name = "SaveWeights"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "saveWeights"))
  con$addTag("inputport", attrs = c(name = "weights", peerid = "v10"))
  con$addTag("inputport",
             attrs = c(name = "filename"),
             paste0('"', dcf_path, '"'))
  con$addTag("inputport", attrs = c(name = "suffixDigits"), 6)
  con$addTag("inputport", attrs = c(name = "step", peerid = "v6"))
  con$addTag("inputport", attrs = c(name = "workdir"), ".none")
  con$closeTag("functor")
  
  # add subtag functor for RegionalizeCategoricalMap
  con$addTag("functor",
             attrs = c(name = "RegionalizeCategoricalMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "Final Landscape (Region)"))
  con$addTag("inputport", attrs = c(name = "globalMap", peerid = "v1"))
  con$addTag("inputport", attrs = c(name = "regionId", peerid = "v6"))
  con$addTag("inputport", attrs = c(name = "keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs = c(name = "regionManager", peerid = "v5"))
  con$addTag("outputport", attrs = c(name = "regionalMap", id = "v7"))
  con$closeTag("functor")
  
  # add subtag functor for RegionalizeCategoricalMap
  con$addTag("functor",
             attrs = c(name = "RegionalizeCategoricalMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "Initial Landscape (Region)"))
  con$addTag("inputport", attrs = c(name = "globalMap", peerid = "v2"))
  con$addTag("inputport", attrs = c(name = "regionId", peerid = "v6"))
  con$addTag("inputport", attrs = c(name = "keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs = c(name = "regionManager", peerid = "v5"))
  con$addTag("outputport", attrs = c(name = "regionalMap", id = "v8"))
  con$closeTag("functor")
  
  # add subtag functor for RegionalizeMap
  con$addTag("functor",
             attrs = c(name = "RegionalizeMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "Static Variables (Region)"))
  con$addTag("inputport", attrs = c(name = "globalMap", peerid = "v3"))
  con$addTag("inputport", attrs = c(name = "regionId", peerid = "v6"))
  con$addTag("inputport", attrs = c(name = "keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs = c(name = "regionManager", peerid = "v5"))
  con$addTag("outputport", attrs = c(name = "regionalMap", id = "v9"))
  con$closeTag("functor")
  
  # add subtag functor for SaveTable
  con$addTag("functor",
             attrs = c(name = "SaveTable"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "saveTable"))
  con$addTag("inputport", attrs = c(name = "table", peerid = "v11"))
  con$addTag("inputport",
             attrs = c(name = "filename"),
             paste0('"', weight_report_path, '"'))
  con$addTag("inputport", attrs = c(name = "suffixDigits"), 2)
  con$addTag("inputport", attrs = c(name = "step", peerid = "v6"))
  con$addTag("inputport", attrs = c(name = "workdir"), ".none")
  con$closeTag("functor")
  
  # add subtag functor for DetermineWeightsOfEvidenceCoefficients
  con$addTag(
    "containerfunctor",
    attrs = c(name = "DetermineWeightsOfEvidenceCoefficients"),
    close = FALSE
  )
  con$addTag(
    "property",
    attrs = c(key = "dff.functor.alias", value = "Weight of Evidence Coefficients")
  )
  con$addTag("inputport", attrs = c(name = "initialLandscape", peerid = "v8"))
  con$addTag("inputport", attrs = c(name = "finalLandscape", peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "ranges", peerid = "v12"))
  con$addTag("inputport", attrs = c(name = "fixAbnormalWeights"), ".no")
  con$addTag("outputport", attrs = c(name = "weights", id = "v10"))
  con$addTag("outputport", attrs = c(name = "report", id = "v11"))
  
  con$addTag("functor",
             attrs = c(name = "NameMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "nameMapCoeff"))
  con$addTag("inputport", attrs = c(name = "map", peerid = "v9"))
  con$addTag("inputport", attrs = c(name = "mapName"), '"static_var"')
  con$closeTag("functor")
  
  con$closeTag("containerfunctor")
  
  # add subtag functor for DetermineWeightsOfEvidenceRanges
  con$addTag(
    "containerfunctor",
    attrs = c(name = "DetermineWeightsOfEvidenceRanges"),
    close = FALSE
  )
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "Weight of Evidence Ranges"))
  con$addTag("inputport", attrs = c(name = "initialLandscape", peerid = "v8"))
  con$addTag("inputport", attrs = c(name = "finalLandscape", peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "skeleton"), skeletonFinal)
  con$addTag("inputport", attrs = c(name = "fixAbnormalWeights"), ".no")
  con$addTag("outputport", attrs = c(name = "ranges", id = "v12"))
  
  con$addTag("functor",
             attrs = c(name = "NameMap"),
             close = FALSE)
  con$addTag("property",
             attrs = c(key = "dff.functor.alias", value = "nameMapRanges"))
  con$addTag("inputport", attrs = c(name = "map", peerid = "v9"))
  con$addTag("inputport", attrs = c(name = "mapName"), '"static_var"')
  con$closeTag("functor")
  
  con$closeTag("containerfunctor")
  con$closeTag("containerfunctor")
  # end.
  
  egoml_woe_file <- paste0(output_dir, "/", egoml, ".egoml")
  saveXML(con$value(), file=egoml_woe_file)
  
  # replace ampersand code character
  egoml_text  <- readLines(egoml_woe_file)
  egoml_text_new  <- gsub(pattern="amp;", replace="", x=egoml_text)
  writeLines(egoml_text_new, con=egoml_woe_file)
  
  out <- list(
    egoml_woe_file = egoml_woe_file,
    weight = woe_dir,
    lc1 = lc1_path,
    lc2 = lc2_path,
    zone = zone_path,
    ers = ers_path
  )
  
  return(out)
}

run_dinamica_woe_model <- function(dinamica_path = NULL, output_dir, egoml, memory_allocation){
  params <- list()
  params$dinamica_path <- dinamica_path
  params$output_dir <- output_dir
  params$egoml <- egoml
  
  executeDINAMICA(params, memory_allocation)
  
  n_woe_report <- paste0(output_dir, "/woe") %>% 
    list.files(full.names=TRUE, pattern="weight_report*") %>%
    length()
  if (n_woe_report == 0) {
    stop("There are no single one of WoE Report! Check DINAMICA EGO log.")
  }
}

run_sciendo_train_process <- function(lc_t1_path, lc_t2_path, zone_path, lc_lookup_table_path,
                                      lc_lookup_table, z_lookup_table, factor_path, time_points,
                                      dinamica_path = NULL, output_dir, memory_allocation, progress_callback = NULL,
                                      lang = "id") {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (!is.null(progress_callback)) progress_callback(0.1, "generate egoml: baseline matrix generation")
  period <- as.numeric(time_points$t2) - as.numeric(time_points$t1)
  egoml_mtx_file <- generate_egoml_transition_matrix(lc_t1_path, lc_t2_path, zone_path, period, output_dir, egoml = "00_sciendo_baseline_tpm")
  run_dinamica_transition_matrix(dinamica_path, output_dir, egoml_mtx_file, memory_allocation)

  # Convert TPM long format to wide format
  macro_dir <- file.path(output_dir, "baseline_tpm_macro")
  tpm_input_dir <- file.path(output_dir, "baseline_tpm")
  
  tryCatch({
    tpm_to_matrix_conversion(
      input_dir = tpm_input_dir,
      lc_lookup_path = lc_lookup_table_path,
      output_dir = macro_dir,
      template_path = "../macro_template/macro_template.xlsm",
      pu_lookup = z_lookup_table,
      lc_t1_path = lc_t1_path,
      zone_path = zone_path,
      time_points = time_points,
      lang = lang
    )
  }, error = function(e) {
    stop("TPM conversion failed: ", e$message)
  })
  
  if (!is.null(progress_callback)) progress_callback(0.2, "generate egoml: raster cube generation")
  out_rc <- generate_egoml_raster_cube(factor_path, output_dir, egoml = "01_sciendo_train_raster_cube")
  
  if (!is.null(progress_callback)) progress_callback(0.5, "run dinamica raster cube")
  run_dinamica_raster_cube(dinamica_path, output_dir, out_rc$egoml_rc_file, memory_allocation)
  
  # generate modified xml file
  pu_df <- z_lookup_table
  
  add_period_pu_classes_to_pam(
    output_dir = output_dir,  
    pu_classes = pu_df,
    time_points = time_points
  )

  if (!is.null(progress_callback)) progress_callback(0.7, "generate egoml: initialize weight of evidence parameters")
  out_woe <- generate_egoml_woe_model(out_rc$alias, lc_lookup_table, 
                                      lc_t1_path, lc_t2_path, zone_path, 
                                      out_rc$ers, output_dir, 
                                      egoml = "02_sciendo_train_woe")
  
  if (!is.null(progress_callback)) progress_callback(0.9, "run dinamica determine weight of evidence")
  run_dinamica_woe_model(dinamica_path, output_dir, out_woe$egoml_woe_file, memory_allocation)
  
  listWoeReport <- out_woe$weight  %>% list.files(full.names=TRUE, pattern="weight_report*")
  df_pu <- z_lookup_table %>% dplyr::rename(ID_PU = 1, PU = 2)
  woe_list <- create_list_of_weight_report(out_woe$weight, listWoeReport, df_pu, lc_lookup_table)
  
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")
  session_log <- format_session_info_table()
  out <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    inputs = list(
      lc_t1_path = lc_t1_path,
      lc_t2_path = lc_t2_path,
      zone_path = zone_path,
      lc_lookup_table_path = lc_lookup_table_path,
      z_lookup_table = z_lookup_table,
      factor_path = factor_path,
      year1 = time_points$t1,
      year2 = time_points$t2,
      output_dir = output_dir
    ),
    rc_path = out_rc$ers,
    woe_report_path = out_woe$weight,
    rc_egoml_path = out_rc$egoml_rc_file,
    woe_egoml_path = out_woe$egoml_woe_file,
    woe_list = woe_list$woe,
    session_log = session_log
  )

  if (!is.null(progress_callback)) progress_callback(1, "generate report")
  generate_sciendo_train_report(output = out, dir = output_dir)
  
  return(out)
}

#' Analyze Multicollinearity in Raster Predictor Layers
#'
#' This function reads raster predictor layers from a folder, samples values, computes
#' Variance Inflation Factor (VIF), and generates a correlation matrix plot.
#'
#' @param folder_path Character. Full path to the folder containing `.tif` raster files.
#' @param sample_size Integer. Number of points to sample from the raster stack. Default is 10,000.
#' @param vif_threshold Numeric. VIF threshold above which variables are recommended for removal. Default is 5.
#' @param seed Integer. Seed for reproducible random sampling. Default is 123.
#' @param verbose Logical. Whether to print VIF results and summary to the console. Default is FALSE.
#'
#' @return A list containing:
#' \describe{
#'   \item{vif_result}{The raw `vifstep` object from the `usdm` package.}
#'   \item{vif_table}{A `data.frame` with variables and their VIF values.}
#'   \item{excluded_vars}{Character vector of variable names recommended for removal (VIF > threshold).}
#'   \item{correlation_matrix}{Correlation matrix (base R `cor`) of the sampled raster values.}
#'   \item{correlation_plot}{A recorded base R plot object of the correlation matrix (use `replayPlot()` to render).}
#' }
#'
#' @import terra
#' @import usdm
#' @import corrplot
#' @importFrom graphics recordPlot
#' @export
#'
#' @examples
#' \dontrun{
#' result <- analyze_multicollinearity("D:/predictors/", verbose = TRUE)
#' head(result$vif_table)
#' }
analyze_multicollinearity <- function(folder_path, sample_size = 10000, vif_threshold = 5, seed = 123, verbose = FALSE) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  if (!requireNamespace("usdm", quietly = TRUE)) stop("Package 'usdm' is required.")
  if (!requireNamespace("corrplot", quietly = TRUE)) stop("Package 'corrplot' is required.")
  
  library(terra)
  library(usdm)
  library(corrplot)
  
  # Read raster stack
  tif_files <- list.files(path = folder_path, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) < 2) {
    stop("Folder predictors must contain at least 2 GeoTIFF (.tif) files for collinearity analysis.")
  }
  rasters <- rast(tif_files)
  names(rasters) <- gsub(".tif", "", basename(tif_files))
  
  # Sample raster values
  set.seed(seed)
  sample_points <- spatSample(rasters, size = sample_size, na.rm = TRUE)
  sample_df <- as.data.frame(sample_points)
  
  # Detect and exclude zero-variance variables
  zero_var <- sapply(sample_df, function(col) {
    v <- stats::var(col, na.rm = TRUE)
    is.na(v) || v == 0
  })
  zero_variance_vars <- names(sample_df)[zero_var]
  if (length(zero_variance_vars) > 0) {
    sample_df <- sample_df[, !zero_var, drop = FALSE]
  }
  
  if (ncol(sample_df) < 2) {
    stop("After removing zero-variance variables, less than 2 variables remain for collinearity analysis.")
  }
  
  # Calculate VIF
  vif_result <- vifstep(sample_df, th = vif_threshold)
  high_vif_vars <- vif_result@excluded
  vif_table <- vif_result@results
  
  # Verbose printing
  if (verbose) {
    message("\n VIF Analysis Results (threshold = ", vif_threshold, "):")
    print(vif_table)
    
    if (length(high_vif_vars) > 0) {
      message("\n Variables recommended for removal (VIF > ", vif_threshold, "):\n", paste(high_vif_vars, collapse = "\n"))
    } else {
      message("\n No variables exceeded VIF threshold.")
    }
  }
  
  # Correlation matrix and plot
  cor_matrix <- cor(sample_df, use = "complete.obs")
  corr_plot <- tryCatch({
    graphics::par(mfrow = c(1, 1))
    n_vars <- ncol(cor_matrix)
    tl_size <- if (n_vars <= 6) 0.85 else if (n_vars <= 10) 0.75 else if (n_vars <= 15) 0.65 else 0.5
    num_size <- if (n_vars <= 6) 0.8 else if (n_vars <= 10) 0.7 else if (n_vars <= 15) 0.55 else 0.4
    corrplot(cor_matrix,
             method = "color",
             type = "full",
             order = "hclust",
             tl.cex = tl_size,
             tl.col = "black",
             tl.srt = 45,
             mar = c(0, 0, 1, 0),
             addCoef.col = "black",
             number.cex = num_size,
             diag = TRUE)
    recordPlot()
  }, error = function(e) NULL)
  
  return(list(
    vif_result = vif_result,
    vif_table = vif_table,
    excluded_vars = high_vif_vars,
    zero_variance_vars = zero_variance_vars,
    correlation_matrix = cor_matrix,
    correlation_plot = corr_plot
  ))
}

#'
#' Generate Standalone Factor Evaluation Report
#'
#' @param factor_path Character string of folder containing factor GeoTIFF files.
#' @param output_dir Directory where the HTML report will be saved.
#' @param vif_threshold Numeric VIF threshold (default 5).
#' @param sample_size Sample size for raster sampling (default 10000).
#' @return File path of the generated HTML report.
#' @export
generate_factor_evaluation_report <- function(factor_path, output_dir, vif_threshold = 5, sample_size = 10000) {
  start_time <- Sys.time()
  
  collinearity_res <- analyze_multicollinearity(
    folder_path = factor_path,
    sample_size = sample_size,
    vif_threshold = vif_threshold,
    verbose = FALSE
  )
  
  end_time <- Sys.time()
  
  report_params <- list(
    factor_path = factor_path,
    vif_threshold = vif_threshold,
    sample_size = sample_size,
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    collinearity_result = collinearity_res
  )
  
  template_candidates <- c(
    "../report_template/sciendo_factor_evaluation_report_template.Rmd",
    "11_sciendo-train/report_template/sciendo_factor_evaluation_report_template.Rmd",
    file.path(getwd(), "11_sciendo-train/report_template/sciendo_factor_evaluation_report_template.Rmd"),
    file.path(getwd(), "report_template/sciendo_factor_evaluation_report_template.Rmd")
  )
  template_path <- NULL
  for (cand in template_candidates) {
    if (file.exists(cand)) {
      template_path <- cand
      break
    }
  }
  if (is.null(template_path)) {
    template_path <- "../report_template/sciendo_factor_evaluation_report_template.Rmd"
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  output_filename <- paste0("sciendo_factor_evaluation_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  
  out_path <- rmarkdown::render(
    input = template_path,
    output_file = output_filename,
    output_dir = output_dir,
    params = report_params,
    quiet = TRUE
  )
  
  return(out_path)
}


#' Add Period and Planning Unit Classes to Raster Metadata File
#' 
#' This function enhances a GDAL PAM (.aux.xml) metadata file by adding Period information
#' and Planning Unit (PU) class information while preserving existing raster metadata. 
#' Both period and PU classes are added as separate XML nodes at the same level as the 
#' original PAMDataset content.
#'
#' @param output_dir Character string specifying the directory path containing the 
#'                  'sciendo_factors.tif' file and where the PAM metadata will be saved.
#' @param pu_classes A data frame or matrix containing PU class definitions where:
#'                  \itemize{
#'                    \item First column contains PU IDs (numeric or character)
#'                    \item Second column contains PU class names (character)
#'                  }
#' @param time_points A data frame or list containing time points with elements:
#'                  \itemize{
#'                    \item t1: Start time (numeric or character that can be coerced to numeric)
#'                    \item t2: End time (numeric or character that can be coerced to numeric)
#'                  }
#' @param overwrite Logical indicating whether to overwrite an existing PAM file
#'                 (default = TRUE). If FALSE and file exists, function will error.
#'
#' @return Invisibly returns TRUE on success. Primarily called for its side effect of
#'         creating/updating the PAM metadata file.
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Checks for required package dependencies
#'   \item Validates input parameters
#'   \item Creates a new XML structure with Root parent node
#'   \item Preserves all existing PAM metadata if present
#'   \item Adds period information under a new <Period> node
#'   \item Adds PU classes under a new <PUClasses> node
#'   \item Saves the combined XML structure with proper formatting
#' }
#'
#' @section File Structure:
#' The resulting XML structure will be:
#' \preformatted{
#' <Root>
#'   <PAMDataset>
#'     <!-- Original raster metadata -->
#'   </PAMDataset>
#'   <Period>
#'     <Value>[calculated period]</Value>
#'   </Period>
#'   <PUClasses>
#'     <Class ID="1" PU="Protected Forest"/>
#'     <!-- Additional classes -->
#'   </PUClasses>
#' </Root>
#' }
#'
#' @examples
#' \donttest{
#' # Prepare sample PU classes data
#' pu_data <- data.frame(
#'   ID = 1:8,
#'   Class = c("Protected Forest", "Production Forest", "Plantation",
#'             "Urban Area", "Wetland Agriculture", "Dryland Agriculture",
#'             "Riverbank Buffer", "Kerinci Sebelat National Park")
#' )
#' 
#' # Prepare time points
#' time_data <- data.frame(t1 = 2000, t2 = 2010)
#'
#' # Add to PAM file (using tempdir() for example)
#' output_path <- tempdir()
#' file.create(paste0(output_path, "/sciendo_factors.tif.aux.xml"))
#' 
#' try(
#'   add_period_pu_classes_to_pam(
#'     output_dir = output_path,
#'     pu_classes = pu_data,
#'     time_points = time_data
#'   )
#' )
#' }
#'
#' @seealso
#' \code{\link[XML]{xmlParse}} for XML parsing functionality
#'
#' @export
#' @importFrom XML newXMLNode xmlParse xmlRoot xmlChildren saveXML
add_period_pu_classes_to_pam <- function(output_dir, pu_classes, time_points, overwrite = TRUE) {
  # Verify XML package availability
  if (!requireNamespace("XML", quietly = TRUE)) {
    stop("Package 'XML' required but not installed. Please install with: install.packages('XML')")
  }
  
  # Validate output directory
  if (!dir.exists(output_dir)) {
    stop("Specified output directory does not exist: ", output_dir)
  }
  
  # Validate pu_classes structure
  if (!is.data.frame(pu_classes) && !is.matrix(pu_classes)) {
    stop("pu_classes must be a data frame or matrix")
  }
  if (ncol(pu_classes) < 2) {
    stop("pu_classes must have at least 2 columns (ID and PU class)")
  }
  
  # Validate time_points structure
  if (!is.data.frame(time_points) && !is.list(time_points)) {
    stop("time_points must be a data frame or list")
  }
  if (!all(c("t1", "t2") %in% names(time_points))) {
    stop("time_points must contain elements named 't1' and 't2'")
  }
  
  # Calculate period
  period <- as.numeric(time_points$t2) - as.numeric(time_points$t1)
  
  # Define file paths
  pam_file <- file.path(output_dir, "sciendo_factors.tif.aux.xml")
  
  # Check overwrite conditions
  if (file.exists(pam_file) && !overwrite) {
    stop("Output file already exists and overwrite=FALSE")
  }
  
  # Create new XML structure
  root <- XML::newXMLNode("Root")
  
  # Add existing PAM content if available
  if (file.exists(pam_file)) {
    tryCatch({
      pam_content <- XML::xmlParse(pam_file)
      pam_dataset <- XML::xmlRoot(pam_content)
      XML::xmlChildren(root) <- XML::xmlChildren(pam_dataset)
    }, error = function(e) {
      warning("Failed to parse existing PAM file, creating new structure: ", e$message)
    })
  }
  
  # Add Period section
  period_node <- XML::newXMLNode("Period", parent = root)
  XML::newXMLNode("Value", period, parent = period_node)
  
  # Add PU classes section
  pu_node <- XML::newXMLNode("PUClasses", parent = root)
  apply(pu_classes, 1, function(row) {
    XML::newXMLNode("Class",
                    attrs = c(
                      ID = as.character(row[1]),
                      PU = as.character(row[2])
                    ),
                    parent = pu_node)
  })
  
  # Save output
  tryCatch({
    XML::saveXML(root, file = pam_file)
    message("Successfully updated PAM metadata file with period and PU classes:\n  ", pam_file)
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to save XML file: ", e$message)
  })
}


#' Convert Long Format Transition Probability Matrices (TPM) to Wide Format Matrix TPM
#'
#' This function processes Transition Probability Matrix (TPM) CSV files, converts them to properly 
#' formatted matrices with land cover labels, and saves them to .xlsm template files with additional 
#' calculations and formatting.
#'
#' @param input_dir Character string specifying the directory containing input CSV files.
#' @param lc_lookup_path Character string specifying the path to the land cover lookup table CSV file.
#' @param output_dir Character string specifying the directory where output Excel files will be saved.
#' @param template_path Character string specifying the path to the .xlsm template file.
#'
#' @details The function performs the following operations:
#' \enumerate{
#'   \item Reads the land cover lookup table to map numeric IDs to land cover classes
#'   \item Processes each TPM CSV file in the input directory:
#'   \itemize{
#'     \item Creates a complete matrix with all possible land cover transitions
#'     \item Calculates diagonal values (persistence probabilities) as 1 minus row sums
#'     \item Applies land cover labels from the lookup table
#'     \item Copies the .xlsm template file for each output
#'     \item Writes the matrix data to the template copy
#'     \item Adds row and column sums with formatting
#'     \item Highlights diagonal cells (persistence probabilities)
#'   }
#'   \item Saves each processed matrix as a .xlsm file (preserving VBA macros)
#' }
#'
#' @return Invisible NULL. The function primarily produces .xlsm files as output.
#'
#' @examples
#' \dontrun{
#' tpm_to_matrix_conversion(
#'   input_dir = "path/to/input/csv/files",
#'   lc_lookup_path = "path/to/lookup_table.csv",
#'   output_dir = "path/to/output/directory",
#'   template_path = "path/to/template.xlsm"
#' )
#' @param time_points Optional list or data frame containing time points with element `t1` for initial year display.
#' @param lang Language code: "id" for Bahasa Indonesia (default), "en" for English.
#'
#' @importFrom openxlsx2 wb_load wb_save wb_add_data wb_add_fill wb_add_font
#' @importFrom openxlsx2 wb_add_border wb_add_numfmt wb_add_formula
#' @importFrom tidyr pivot_wider expand_grid
#' @importFrom dplyr select mutate left_join arrange
#' @importFrom tools file_path_sans_ext
#' @export
tpm_to_matrix_conversion <- function(input_dir, lc_lookup_path, output_dir, template_path, pu_lookup = NULL, lc_t1_path = NULL, zone_path = NULL, time_points = NULL, lang = "id") {
  # Load required libraries
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 package is required. Please install it with: install.packages('openxlsx2')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required. Please install it with: install.packages('dplyr')")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package is required. Please install it with: install.packages('tidyr')")
  }
  
  # Normalize paths
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (basename(output_dir) == "baseline_tpm_macro") {
    output_dir <- dirname(output_dir)
  }
  
  # Locate template file with fallbacks if not found at template_path
  resolved_template <- template_path
  if (!file.exists(resolved_template)) {
    candidates <- c(
      file.path(getwd(), template_path),
      file.path("11_sciendo-train/macro_template/macro_template.xlsm"),
      file.path(getwd(), "11_sciendo-train/macro_template/macro_template.xlsm"),
      file.path("..", "macro_template", "macro_template.xlsm")
    )
    for (cand in candidates) {
      if (file.exists(cand)) {
        resolved_template <- cand
        break
      }
    }
  }
  if (!file.exists(resolved_template)) {
    stop("Template file not found: ", template_path)
  }
  
  # Read LC lookup table (supports file path or data.frame)
  if (is.character(lc_lookup_path)) {
    lc_lookup_table <- read.csv(lc_lookup_path, stringsAsFactors = FALSE)
  } else if (is.data.frame(lc_lookup_path)) {
    lc_lookup_table <- lc_lookup_path
  } else {
    stop("lc_lookup_path must be a file path or data.frame")
  }
  
  # Read PU lookup table if provided
  pu_map <- list()
  if (!is.null(pu_lookup)) {
    pu_df <- NULL
    if (is.character(pu_lookup) && file.exists(pu_lookup)) {
      pu_df <- read.csv(pu_lookup, stringsAsFactors = FALSE)
    } else if (is.data.frame(pu_lookup)) {
      pu_df <- pu_lookup
    }
    if (!is.null(pu_df) && ncol(pu_df) >= 2) {
      colnames(pu_df)[1:2] <- c("ID", "Name")
      pu_map <- setNames(as.character(pu_df$Name), as.character(pu_df$ID))
    }
  }
  
  # Calculate initial baseline class area per PU in hectares if rasters available
  pu_class_areas <- list() # key: paste0(pu_id, "_", lc_id) -> area_ha
  if (!is.null(lc_t1_path) && !is.null(zone_path) && file.exists(lc_t1_path) && file.exists(zone_path)) {
    tryCatch({
      if (requireNamespace("terra", quietly = TRUE)) {
        r_lc <- terra::rast(lc_t1_path)
        r_zone <- terra::rast(zone_path)
        cs_ha <- tryCatch({
          if (!terra::is.lonlat(r_lc)) {
            (terra::xres(r_lc) * terra::yres(r_lc)) / 10000
          } else {
            mean(terra::values(terra::cellSize(r_lc, unit = "ha")), na.rm = TRUE)
          }
        }, error = function(e) 1.0)
        
        if (!terra::same.crs(r_zone, r_lc)) {
          r_zone <- terra::project(r_zone, r_lc, method = "near")
        }
        if (terra::ext(r_zone) != terra::ext(r_lc) || terra::res(r_zone)[1] != terra::res(r_lc)[1]) {
          r_zone <- terra::resample(r_zone, r_lc, method = "near")
        }
        
        ct <- terra::crosstab(c(r_zone, r_lc), long = TRUE)
        if (ncol(ct) >= 3) {
          colnames(ct)[1:3] <- c("PU", "LC", "Count")
          for (k in seq_len(nrow(ct))) {
            key <- paste0(ct$PU[k], "_", ct$LC[k])
            pu_class_areas[[key]] <- round(as.numeric(ct$Count[k]) * cs_ha, 2)
          }
        }
      }
    }, error = function(e) {
      message("Warning: could not calculate raster areas: ", e$message)
    })
  }
  
  macro_dir <- file.path(output_dir, "baseline_tpm_macro")
  dir.create(macro_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(macro_dir, "sciendo_tpm_scenario.xlsm")
  
  # Generate 1-click unblock utility script for Windows Mark of the Web
  unblock_bat_path <- file.path(macro_dir, "activate_macros.bat")
  bat_content <- if (lang == "id") {
    c(
      "@echo off",
      "echo Membuka blokir keamanan Windows (Unblock Mark of the Web) untuk file makro...",
      "powershell -NoProfile -ExecutionPolicy Bypass -Command \"Get-ChildItem -Path '%~dp0' -Filter '*.xlsm' | ForEach-Object { Unblock-File -LiteralPath $_.FullName; Write-Host ('Berhasil membuka blokir: ' + $_.Name) }\"",
      "echo Selesai! File makro siap dibuka di Microsoft Excel.",
      "pause"
    )
  } else {
    c(
      "@echo off",
      "echo Unblocking Windows security (Unblock Mark of the Web) for macro files...",
      "powershell -NoProfile -ExecutionPolicy Bypass -Command \"Get-ChildItem -Path '%~dp0' -Filter '*.xlsm' | ForEach-Object { Unblock-File -LiteralPath $_.FullName; Write-Host ('Successfully unblocked: ' + $_.Name) }\"",
      "echo Done! Macro file is ready to open in Microsoft Excel.",
      "pause"
    )
  }
  writeLines(bat_content, unblock_bat_path)
  
  # Determine Column A header for initial T1 area
  t1_year <- NULL
  if (!is.null(time_points)) {
    t1_key <- grep("^t1$", names(time_points), ignore.case = TRUE, value = TRUE)
    if (length(t1_key) > 0) {
      val <- time_points[[t1_key[1]]]
      if (!is.null(val) && !is.na(val) && nzchar(trimws(as.character(val)))) {
        t1_year <- trimws(as.character(val))
      }
    } else if (is.numeric(time_points) && length(time_points) >= 1) {
      t1_year <- as.character(time_points[1])
    }
  }
  
  col_a_header <- if (!is.null(t1_year)) {
    if (lang == "id") paste0("Luas ", t1_year, " (ha)") else paste0(t1_year, " Area (ha)")
  } else {
    if (lang == "id") "Luas T1 (ha)" else "T1 Area (ha)"
  }
  
  base_label  <- if (lang == "id") "Luas Baseline Tahun Proyeksi (ha)" else "Baseline Projected Years Area (ha)"
  scen_label  <- if (lang == "id") "Luas Skenario Tahun Proyeksi (ha)" else "Scenario Projected Years Area (ha)"
  delta_label <- if (lang == "id") "Perubahan Bersih (ha)" else "Net Change (ha)"
  
  message("Consolidated scenario macro will be saved to: ", output_file)
  
  tpm_dinamica <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(tpm_dinamica) == 0) {
    stop("No CSV files found in input directory: ", input_dir)
  }
  
  # Extract numeric PU IDs to process files in numerical order
  raw_ids <- suppressWarnings(as.numeric(gsub(".*?([0-9]+).*", "\\1", basename(tpm_dinamica))))
  raw_ids[is.na(raw_ids)] <- seq_along(raw_ids)[is.na(raw_ids)]
  ord <- order(raw_ids)
  tpm_dinamica <- tpm_dinamica[ord]
  pu_ids <- raw_ids[ord]
  
  # Pre-compute sheet names and PU names for navigation linking
  all_sheet_names <- character(length(tpm_dinamica))
  all_pu_names <- character(length(tpm_dinamica))
  for (idx in seq_along(tpm_dinamica)) {
    pu_id <- pu_ids[idx]
    pu_name <- if (as.character(pu_id) %in% names(pu_map)) {
      pu_map[[as.character(pu_id)]]
    } else {
      paste("Planning Unit", pu_id)
    }
    all_pu_names[idx] <- pu_name
    
    clean_label <- gsub("['\"\\\\/?*\\[\\]:]", "_", pu_name, perl = TRUE)
    clean_label <- gsub("\\s+", "_", trimws(clean_label))
    prefix <- paste0("PU_", pu_id, "_")
    max_label_len <- max(1, 25 - nchar(prefix))
    short_label <- substr(clean_label, 1, max_label_len)
    all_sheet_names[idx] <- paste0(prefix, short_label)
  }
  
  # Initialize workbook from template
  file.copy(resolved_template, output_file, overwrite = TRUE)
  wb <- openxlsx2::wb_load(output_file)
  
  # Rename initial template sheet to Dashboard
  init_sheets <- wb$get_sheet_names()
  if (length(init_sheets) > 0) {
    wb$set_sheet_names(old = init_sheets[1], new = "Dashboard")
  } else {
    wb$add_worksheet("Dashboard")
  }
  
  dashboard_rows <- list()
  
  for (idx in seq_along(tpm_dinamica)) {
    tpm <- tpm_dinamica[idx]
    pu_id <- pu_ids[idx]
    pu_name <- all_pu_names[idx]
    sheet_name <- all_sheet_names[idx]
    
    # Read TPM CSV
    matrix1 <- read.csv(tpm, stringsAsFactors = FALSE)
    if ("X" %in% colnames(matrix1)) {
      matrix1 <- matrix1[, colnames(matrix1) != "X", drop = FALSE]
    }
    colnames(matrix1) <- gsub("[*.]", "", colnames(matrix1))
    if (ncol(matrix1) >= 3) {
      matrix1 <- matrix1[, 1:3]
      colnames(matrix1) <- c("From", "To", "Rate")
    }
    matrix1$From <- as.numeric(matrix1$From)
    matrix1$To   <- as.numeric(matrix1$To)
    matrix1$Rate <- as.numeric(matrix1$Rate)
    matrix1 <- matrix1[!is.na(matrix1$From) & !is.na(matrix1$To) & !is.na(matrix1$Rate), ]
    
    all_ids <- sort(unique(c(matrix1$From, matrix1$To)))
    if (length(all_ids) == 0) {
      next
    }
    
    complete_grid <- expand.grid(From = all_ids, To = all_ids)
    complete_grid <- merge(complete_grid, matrix1, by = c("From", "To"), all.x = TRUE)
    complete_grid$Rate[is.na(complete_grid$Rate)] <- 0
    
    wide_df <- as.data.frame(tidyr::pivot_wider(
      complete_grid,
      names_from = To,
      values_from = Rate,
      values_fill = 0
    ))
    wide_df <- wide_df[order(wide_df$From), ]
    
    ord_cols <- sort(as.numeric(colnames(wide_df)[-1]))
    wide_df <- wide_df[, c("From", as.character(ord_cols))]
    
    result_matrix <- as.matrix(wide_df[, -1, drop = FALSE])
    rownames(result_matrix) <- wide_df$From
    
    # Calculate diagonal values and normalize rows to exactly 1.0
    for (i in 1:nrow(result_matrix)) {
      result_matrix[i, i] <- 0
      row_sum_off_diag <- sum(result_matrix[i, ])
      if (row_sum_off_diag >= 1) {
        if (row_sum_off_diag > 0) {
          result_matrix[i, ] <- result_matrix[i, ] * (0.999999 / row_sum_off_diag)
        }
        result_matrix[i, i] <- 0.000001
      } else {
        result_matrix[i, i] <- 1 - row_sum_off_diag
      }
      cur_sum <- sum(result_matrix[i, ])
      if (cur_sum > 0) {
        result_matrix[i, ] <- result_matrix[i, ] / cur_sum
      }
      final_sum <- sum(result_matrix[i, ])
      if (abs(final_sum - 1) > .Machine$double.eps) {
        max_idx <- which.max(result_matrix[i, ])
        result_matrix[i, max_idx] <- result_matrix[i, max_idx] + (1 - final_sum)
      }
    }
    
    # Map row and col names to LC labels
    row_ids <- as.numeric(rownames(result_matrix))
    row_labels <- lc_lookup_table$LC[match(row_ids, lc_lookup_table$ID)]
    row_labels[is.na(row_labels)] <- paste("LC", row_ids[is.na(row_labels)])
    rownames(result_matrix) <- row_labels
    
    col_ids <- as.numeric(colnames(result_matrix))
    col_labels <- lc_lookup_table$LC[match(col_ids, lc_lookup_table$ID)]
    col_labels[is.na(col_labels)] <- paste("LC", col_ids[is.na(col_labels)])
    colnames(result_matrix) <- col_labels
    
    n_rows <- nrow(result_matrix)
    n_cols <- ncol(result_matrix)
    
    # Determine initial empirical class area in hectares for each class row
    col_a_vals <- numeric(n_rows)
    for (ri in 1:n_rows) {
      from_id <- row_ids[ri]
      area_key <- paste0(pu_id, "_", from_id)
      col_a_vals[ri] <- if (area_key %in% names(pu_class_areas)) {
        pu_class_areas[[area_key]]
      } else if (length(pu_class_areas) > 0) {
        0.00
      } else {
        1000.00
      }
    }
    
    # Build active transitions intervention panel (Row 6 headers, Row 7+ data)
    active_records <- list()
    for (ri in 1:n_rows) {
      for (ci in 1:n_cols) {
        if (ri != ci && result_matrix[ri, ci] > 0.000001) {
          from_id <- row_ids[ri]
          init_class_area <- col_a_vals[ri]
          base_rate <- round(result_matrix[ri, ci], 6)
          base_area <- round(init_class_area * base_rate, 2)
          
          active_records[[length(active_records) + 1]] <- data.frame(
            From_Class          = row_labels[ri],
            To_Class            = col_labels[ci],
            Baseline_Rate       = base_rate,
            Baseline_Area_ha    = base_area,
            Pct_Adjustment      = 0.0,
            Scenario_Rate       = base_rate,
            Scenario_Area_ha    = base_area,
            Intervention_Status = "Baseline",
            Matrix_Row          = 6 + ri, # Sheet row (Row 6 is header, Row 7+ is data)
            Matrix_Col          = 2 + ci, # Sheet col (B=2 is row header, C=3.. is matrix data)
            stringsAsFactors    = FALSE
          )
        }
      }
    }
    if (length(active_records) > 0) {
      panel_df <- do.call(rbind, active_records)
    } else {
      panel_df <- data.frame(
        From_Class          = character(0),
        To_Class            = character(0),
        Baseline_Rate       = numeric(0),
        Baseline_Area_ha    = numeric(0),
        Pct_Adjustment      = numeric(0),
        Scenario_Rate       = numeric(0),
        Scenario_Area_ha    = numeric(0),
        Intervention_Status = character(0),
        Matrix_Row          = integer(0),
        Matrix_Col          = integer(0),
        stringsAsFactors    = FALSE
      )
    }
    
    # Add PU sheet
    wb$add_worksheet(sheet_name)
    
    # 1. Title and Navigation Hyperlinks (Row 1 & 2)
    pu_title_text <- if (lang == "id") {
      paste0("Unit Perencanaan ", pu_id, ": ", pu_name, " - Matriks Probabilitas Transisi (TPM)")
    } else {
      paste0("Planning Unit ", pu_id, ": ", pu_name, " - Transition Probability Matrix (TPM)")
    }
    wb$add_data(sheet = sheet_name, x = pu_title_text, start_col = 2, start_row = 1)
    wb$add_font(sheet = sheet_name, dims = "B1", size = 13, bold = TRUE, color = openxlsx2::wb_color("navy"))
    
    # Back to Dashboard link at B2
    dash_link_text <- if (lang == "id") "<- Kembali ke Dashboard Utama" else "<- Back to Master Dashboard"
    wb$add_formula(sheet = sheet_name, x = paste0('=HYPERLINK("#\'Dashboard\'!A1", "', dash_link_text, '")'), dims = "B2")
    wb$add_font(sheet = sheet_name, dims = "B2", bold = TRUE, color = openxlsx2::wb_color("blue"), underline = "single")
    
    # Next Planning Unit link at D2 (loops to PU 1 at end)
    next_sheet <- if (idx < length(all_sheet_names)) all_sheet_names[idx + 1] else all_sheet_names[1]
    next_label <- if (idx < length(all_sheet_names)) {
      if (lang == "id") "Lanjut ke Unit Perencanaan Berikutnya ->" else "Go to Next Planning Unit >"
    } else {
      if (lang == "id") paste0("Kembali ke Unit Perencanaan Pertama (PU ", pu_ids[1], ") ->") else paste0("Go to First Planning Unit (PU ", pu_ids[1], ") >")
    }
    wb$add_formula(sheet = sheet_name, x = paste0('=HYPERLINK("#\'', next_sheet, '\'!A1", "', next_label, '")'), dims = "D2")
    wb$add_font(sheet = sheet_name, dims = "D2", bold = TRUE, color = openxlsx2::wb_color("blue"), underline = "single")
    
    # Rows 3-4: Compact, styled step-by-step instruction box
    wb$set_row_heights(sheet = sheet_name, rows = c(3, 4), heights = c(20, 20))
    pu_guide_line1 <- if (lang == "id") {
      "Panduan Penggunaan: 1. Sunting nilai probabilitas langsung pada sel matriks, atau sesuaikan nilai % penyesuaian / luas skenario (ha) pada tabel hijau di kanan. 2. Nilai pada sel lain dalam baris otomatis disesuaikan secara proporsional agar total probabilitas baris tetap 1.000."
    } else {
      "Quick Guide: 1. Edit transition probabilities directly in matrix cells, or adjust % adjustment / scenario area (ha) in the green table on right. 2. Other cells in the row rebalance proportionally so the row sum remains 1.000."
    }
    pu_guide_line2 <- if (lang == "id") {
      "3. Sel yang diubah otomatis ditandai kuning dan berstatus 'Diintervensi'. Transisi baru (nilai baseline 0) dapat langsung diisi nilai > 0. 4. Gunakan 'Lanjut ke Unit Perencanaan Berikutnya' untuk beralih lembar, atau 'Kembali ke Dashboard Utama'."
    } else {
      "3. Modified cells turn yellow with status 'Intervened'. New transitions (baseline 0) can be entered > 0. 4. Use 'Go to Next Planning Unit' to switch sheets, or 'Back to Master Dashboard'."
    }
    wb$add_data(sheet = sheet_name, x = pu_guide_line1, start_col = 2, start_row = 3)
    wb$add_font(sheet = sheet_name, dims = "B3", size = 9.5, bold = TRUE, color = openxlsx2::wb_color(hex = "0369A1"))
    wb$add_data(sheet = sheet_name, x = pu_guide_line2, start_col = 2, start_row = 4)
    wb$add_font(sheet = sheet_name, dims = "B4", size = 9.5, italic = TRUE, color = openxlsx2::wb_color(hex = "075985"))
    
    mat_end_col <- 2 + n_cols
    mat_sum_col <- mat_end_col + 1
    panel_col <- mat_sum_col + 2
    base_sheet_name <- substr(paste0("_Base_", sheet_name), 1, 31)
    
    pu_box_end_col <- panel_col + 7
    pu_box_range <- paste0("B3:", openxlsx2::int2col(pu_box_end_col), "4")
    wb$add_fill(sheet = sheet_name, dims = pu_box_range, color = openxlsx2::wb_color(hex = "F0F9FF"))
    
    # Row 5: Spacer
    wb$set_row_heights(sheet = sheet_name, rows = 5, heights = 10)
    
    # 2. Transition Matrix Headers (Row 6)
    wb$set_row_heights(sheet = sheet_name, rows = 6, heights = 24)
    mat_from_to <- if (lang == "id") "Dari / Ke" else "From \\ To"
    mat_gt      <- if (lang == "id") "Total Baris" else "Grand Total"
    matrix_header <- c(mat_from_to, col_labels, mat_gt)
    
    # Column A Header (cell A6)
    wb$add_data(sheet = sheet_name, x = col_a_header, start_col = 1, start_row = 6)
    wb$add_data(sheet = sheet_name, x = as.data.frame(t(matrix_header)), start_col = 2, start_row = 6, col_names = FALSE)
    
    # Style Matrix Header row (A6 to end of matrix)
    hdr_range <- paste0("A6:", openxlsx2::int2col(mat_sum_col), "6")
    wb$add_fill(sheet = sheet_name, dims = hdr_range, color = openxlsx2::wb_color(hex = "1E3A8A")) # Dark navy
    wb$add_font(sheet = sheet_name, dims = hdr_range, color = openxlsx2::wb_color("white"), bold = TRUE)
    
    # 3. Transition Matrix Data (Rows 7 to 6 + n_rows)
    # Column A Data (cells A7:A<6 + n_rows>) - Initial class area in ha
    wb$add_data(sheet = sheet_name, x = as.data.frame(col_a_vals), start_col = 1, start_row = 7, col_names = FALSE)
    col_a_range <- paste0("A7:A", 6 + n_rows)
    wb$add_fill(sheet = sheet_name, dims = col_a_range, color = openxlsx2::wb_color(hex = "F1F5F9")) # Light slate
    wb$add_font(sheet = sheet_name, dims = col_a_range, bold = TRUE)
    wb$add_numfmt(sheet = sheet_name, dims = col_a_range, numfmt = "#,##0.00")
    
    mat_data_df <- as.data.frame(result_matrix)
    mat_data_df <- cbind(Row_Label = row_labels, mat_data_df)
    
    # Add row sums
    row_sums_vec <- rowSums(result_matrix)
    mat_data_df$Grand_Total <- row_sums_vec
    
    wb$add_data(sheet = sheet_name, x = mat_data_df, start_col = 2, start_row = 7, col_names = FALSE)
    
    # Format matrix row headers (Col B)
    lbl_range <- paste0("B7:B", 6 + n_rows)
    wb$add_fill(sheet = sheet_name, dims = lbl_range, color = openxlsx2::wb_color(hex = "F1F5F9")) # Light slate
    wb$add_font(sheet = sheet_name, dims = lbl_range, bold = TRUE)
    
    # Format matrix data cells (Cols C to 2+n_cols)
    data_range <- paste0("C7:", openxlsx2::int2col(mat_end_col), 6 + n_rows)
    wb$add_numfmt(sheet = sheet_name, dims = data_range, numfmt = "0.000")
    
    # Format sum column (Col mat_sum_col)
    sum_range <- paste0(openxlsx2::int2col(mat_sum_col), "7:", openxlsx2::int2col(mat_sum_col), 6 + n_rows)
    wb$add_fill(sheet = sheet_name, dims = sum_range, color = openxlsx2::wb_color(hex = "E0F2FE")) # Soft sky blue
    wb$add_font(sheet = sheet_name, dims = sum_range, bold = TRUE)
    wb$add_numfmt(sheet = sheet_name, dims = sum_range, numfmt = "0.000")
    
    # Apply diagonal and active cell styling
    for (ri in 1:n_rows) {
      for (ci in 1:n_cols) {
        cell_ref <- paste0(openxlsx2::int2col(2 + ci), 6 + ri)
        if (ri == ci) {
          # Diagonal persistence: light yellow
          wb$add_fill(sheet = sheet_name, dims = cell_ref, color = openxlsx2::wb_color(hex = "FEF9C3"))
        } else if (result_matrix[ri, ci] > 0.000001) {
          # Active off-diagonal: subtle blue
          wb$add_fill(sheet = sheet_name, dims = cell_ref, color = openxlsx2::wb_color(hex = "DBEAFE"))
        } else {
          # Zero cells: muted font
          wb$add_font(sheet = sheet_name, dims = cell_ref, color = openxlsx2::wb_color(hex = "9CA3AF"))
        }
      }
    }
    
    # 3b. Blank separator row and 3 Bottom Summary Rows
    last_mat_row  <- 6 + n_rows
    blank_row     <- last_mat_row + 1
    base_sum_row  <- blank_row + 1
    scen_sum_row  <- base_sum_row + 1
    delta_sum_row <- scen_sum_row + 1
    
    wb$set_row_heights(sheet = sheet_name, rows = blank_row, heights = 12)
    wb$set_row_heights(sheet = sheet_name, rows = c(base_sum_row, scen_sum_row, delta_sum_row), heights = c(22, 22, 22))
    
    first_c_let <- openxlsx2::int2col(3)
    last_c_let  <- openxlsx2::int2col(mat_end_col)
    gt_let      <- openxlsx2::int2col(mat_sum_col)
    
    # Row 1 - Baseline Projected Year Area
    wb$add_data(sheet = sheet_name, x = base_label, start_col = 2, start_row = base_sum_row)
    wb$add_formula(sheet = sheet_name, x = paste0("=SUM(A7:A", last_mat_row, ")"), dims = paste0("A", base_sum_row))
    for (ci in 1:n_cols) {
      c_let <- openxlsx2::int2col(2 + ci)
      wb$add_formula(
        sheet = sheet_name,
        x = paste0("=ROUND(SUMPRODUCT($A$7:$A$", last_mat_row, ", '", base_sheet_name, "'!", c_let, "7:", c_let, last_mat_row, "), 2)"),
        dims = paste0(c_let, base_sum_row)
      )
    }
    wb$add_formula(sheet = sheet_name, x = paste0("=SUM(", first_c_let, base_sum_row, ":", last_c_let, base_sum_row, ")"), dims = paste0(gt_let, base_sum_row))
    
    # Row 2 - Scenario Projected Year Area
    wb$add_data(sheet = sheet_name, x = scen_label, start_col = 2, start_row = scen_sum_row)
    wb$add_formula(sheet = sheet_name, x = paste0("=SUM(A7:A", last_mat_row, ")"), dims = paste0("A", scen_sum_row))
    for (ci in 1:n_cols) {
      c_let <- openxlsx2::int2col(2 + ci)
      wb$add_formula(
        sheet = sheet_name,
        x = paste0("=ROUND(SUMPRODUCT($A$7:$A$", last_mat_row, ", ", c_let, "7:", c_let, last_mat_row, "), 2)"),
        dims = paste0(c_let, scen_sum_row)
      )
    }
    wb$add_formula(sheet = sheet_name, x = paste0("=SUM(", first_c_let, scen_sum_row, ":", last_c_let, scen_sum_row, ")"), dims = paste0(gt_let, scen_sum_row))
    
    # Row 3 - Net Change / Delta
    wb$add_data(sheet = sheet_name, x = delta_label, start_col = 2, start_row = delta_sum_row)
    wb$add_formula(sheet = sheet_name, x = paste0("=ROUND(A", scen_sum_row, "-A", base_sum_row, ", 2)"), dims = paste0("A", delta_sum_row))
    for (ci in 1:n_cols) {
      c_let <- openxlsx2::int2col(2 + ci)
      wb$add_formula(
        sheet = sheet_name,
        x = paste0("=ROUND(", c_let, scen_sum_row, "-", c_let, base_sum_row, ", 2)"),
        dims = paste0(c_let, delta_sum_row)
      )
    }
    wb$add_formula(sheet = sheet_name, x = paste0("=SUM(", first_c_let, delta_sum_row, ":", last_c_let, delta_sum_row, ")"), dims = paste0(gt_let, delta_sum_row))
    
    # Number formats
    wb$add_numfmt(sheet = sheet_name, dims = paste0("A", base_sum_row, ":", gt_let, scen_sum_row), numfmt = "#,##0.00")
    wb$add_numfmt(sheet = sheet_name, dims = paste0("A", delta_sum_row, ":", gt_let, delta_sum_row), numfmt = "+#,##0.00;-#,##0.00;0.00")
    
    # Fills: Baseline (#F1F5F9), Scenario (#E0F2FE), Net Change (#FEF3C7)
    wb$add_fill(sheet = sheet_name, dims = paste0("A", base_sum_row, ":", gt_let, base_sum_row), color = openxlsx2::wb_color(hex = "F1F5F9"))
    wb$add_fill(sheet = sheet_name, dims = paste0("A", scen_sum_row, ":", gt_let, scen_sum_row), color = openxlsx2::wb_color(hex = "E0F2FE"))
    wb$add_fill(sheet = sheet_name, dims = paste0("A", delta_sum_row, ":", gt_let, delta_sum_row), color = openxlsx2::wb_color(hex = "FEF3C7"))
    
    # Bold fonts for labels (Col B), Col A, and Grand Total
    wb$add_font(sheet = sheet_name, dims = paste0("A", base_sum_row, ":B", delta_sum_row), bold = TRUE)
    wb$add_font(sheet = sheet_name, dims = paste0(gt_let, base_sum_row, ":", gt_let, delta_sum_row), bold = TRUE)
    
    # 4. Active Transitions Recapitulation Panel (Row 6 headers, Row 7+ data)
    panel_headers <- if (lang == "id") {
      c("Kelas Asal", "Kelas Tujuan", "Laju Baseline", "Luas Baseline (ha)", "% Penyesuaian", "Laju Skenario", "Luas Skenario (ha)", "Status Intervensi", "_mRow", "_mCol")
    } else {
      c("From Class", "To Class", "Baseline Rate", "Baseline Area (ha)", "% Adjustment", "Scenario Rate", "Scenario Area (ha)", "Intervention Status", "_mRow", "_mCol")
    }
    wb$add_data(sheet = sheet_name, x = as.data.frame(t(panel_headers)), start_col = panel_col, start_row = 6, col_names = FALSE)
    
    # Panel Header style (Col panel_col to panel_col + 7)
    panel_hdr_range <- paste0(openxlsx2::int2col(panel_col), "6:", openxlsx2::int2col(panel_col + 7), "6")
    wb$add_fill(sheet = sheet_name, dims = panel_hdr_range, color = openxlsx2::wb_color(hex = "0F766E")) # Dark teal
    wb$add_font(sheet = sheet_name, dims = panel_hdr_range, color = openxlsx2::wb_color("white"), bold = TRUE)
    
    if (nrow(panel_df) > 0) {
      wb$add_data(sheet = sheet_name, x = panel_df, start_col = panel_col, start_row = 7, col_names = FALSE)
      
      panel_end_row     <- 6 + nrow(panel_df)
      base_rate_col_let <- openxlsx2::int2col(panel_col + 2)
      base_area_col_let <- openxlsx2::int2col(panel_col + 3)
      adj_col_let       <- openxlsx2::int2col(panel_col + 4)
      scen_rate_col_let <- openxlsx2::int2col(panel_col + 5)
      scen_area_col_let <- openxlsx2::int2col(panel_col + 6)
      
      # Add dynamic formula for Scenario Area (ha): =ROUND(Baseline_Area_ha * (1 + Pct_Adjustment), 2)
      for (pr in 7:panel_end_row) {
        form_str <- paste0("=ROUND(", base_area_col_let, pr, "*(1+", adj_col_let, pr, "), 2)")
        wb$add_formula(sheet = sheet_name, x = form_str, dims = paste0(scen_area_col_let, pr))
      }
      
      wb$add_numfmt(sheet = sheet_name, dims = paste0(base_rate_col_let, "7:", base_rate_col_let, panel_end_row), numfmt = "0.0000")
      wb$add_numfmt(sheet = sheet_name, dims = paste0(base_area_col_let, "7:", base_area_col_let, panel_end_row), numfmt = "#,##0.00")
      wb$add_numfmt(sheet = sheet_name, dims = paste0(adj_col_let, "7:", adj_col_let, panel_end_row), numfmt = "0.0%")
      wb$add_numfmt(sheet = sheet_name, dims = paste0(scen_rate_col_let, "7:", scen_rate_col_let, panel_end_row), numfmt = "0.0000")
      wb$add_numfmt(sheet = sheet_name, dims = paste0(scen_area_col_let, "7:", scen_area_col_let, panel_end_row), numfmt = "#,##0.00")
    }
    
    # Hide helper columns _mRow, _mCol
    wb$set_col_widths(sheet = sheet_name, cols = c(panel_col + 8, panel_col + 9), widths = 0.1, hidden = TRUE)
    
    # Set column widths for readability
    wb$set_col_widths(sheet = sheet_name, cols = 1, widths = 18)
    wb$set_col_widths(sheet = sheet_name, cols = 2, widths = 34)
    wb$set_col_widths(sheet = sheet_name, cols = 3:mat_sum_col, widths = 14)
    wb$set_col_widths(sheet = sheet_name, cols = panel_col:(panel_col + 1), widths = 24)
    wb$set_col_widths(sheet = sheet_name, cols = (panel_col + 2):(panel_col + 7), widths = 18)
    
    # 5. Hidden Baseline Backup Sheet (_Base_<sheet_name>)
    wb$add_worksheet(base_sheet_name)
    wb$set_sheet_visibility(sheet = base_sheet_name, value = "hidden")
    wb$add_data(sheet = base_sheet_name, x = pu_title_text, start_col = 2, start_row = 1)
    
    # Column A and Matrix Headers on _Base_
    wb$add_data(sheet = base_sheet_name, x = col_a_header, start_col = 1, start_row = 6)
    wb$add_data(sheet = base_sheet_name, x = as.data.frame(col_a_vals), start_col = 1, start_row = 7, col_names = FALSE)
    wb$add_numfmt(sheet = base_sheet_name, dims = col_a_range, numfmt = "#,##0.00")
    
    wb$add_data(sheet = base_sheet_name, x = as.data.frame(t(matrix_header)), start_col = 2, start_row = 6, col_names = FALSE)
    wb$add_data(sheet = base_sheet_name, x = mat_data_df, start_col = 2, start_row = 7, col_names = FALSE)
    
    # Mirror summary rows on _Base_ sheet
    wb$add_data(sheet = base_sheet_name, x = base_label, start_col = 2, start_row = base_sum_row)
    wb$add_formula(sheet = base_sheet_name, x = paste0("=SUM(A7:A", last_mat_row, ")"), dims = paste0("A", base_sum_row))
    for (ci in 1:n_cols) {
      c_let <- openxlsx2::int2col(2 + ci)
      wb$add_formula(
        sheet = base_sheet_name,
        x = paste0("=ROUND(SUMPRODUCT($A$7:$A$", last_mat_row, ", ", c_let, "7:", c_let, last_mat_row, "), 2)"),
        dims = paste0(c_let, base_sum_row)
      )
    }
    wb$add_formula(sheet = base_sheet_name, x = paste0("=SUM(", first_c_let, base_sum_row, ":", last_c_let, base_sum_row, ")"), dims = paste0(gt_let, base_sum_row))

    wb$add_data(sheet = base_sheet_name, x = scen_label, start_col = 2, start_row = scen_sum_row)
    wb$add_formula(sheet = base_sheet_name, x = paste0("=SUM(A7:A", last_mat_row, ")"), dims = paste0("A", scen_sum_row))
    for (ci in 1:n_cols) {
      c_let <- openxlsx2::int2col(2 + ci)
      wb$add_formula(
        sheet = base_sheet_name,
        x = paste0("=ROUND(SUMPRODUCT($A$7:$A$", last_mat_row, ", ", c_let, "7:", c_let, last_mat_row, "), 2)"),
        dims = paste0(c_let, scen_sum_row)
      )
    }
    wb$add_formula(sheet = base_sheet_name, x = paste0("=SUM(", first_c_let, scen_sum_row, ":", last_c_let, scen_sum_row, ")"), dims = paste0(gt_let, scen_sum_row))

    wb$add_data(sheet = base_sheet_name, x = delta_label, start_col = 2, start_row = delta_sum_row)
    wb$add_formula(sheet = base_sheet_name, x = paste0("=ROUND(A", scen_sum_row, "-A", base_sum_row, ", 2)"), dims = paste0("A", delta_sum_row))
    for (ci in 1:n_cols) {
      c_let <- openxlsx2::int2col(2 + ci)
      wb$add_formula(
        sheet = base_sheet_name,
        x = paste0("=ROUND(", c_let, scen_sum_row, "-", c_let, base_sum_row, ", 2)"),
        dims = paste0(c_let, delta_sum_row)
      )
    }
    wb$add_formula(sheet = base_sheet_name, x = paste0("=SUM(", first_c_let, delta_sum_row, ":", last_c_let, delta_sum_row, ")"), dims = paste0(gt_let, delta_sum_row))

    wb$add_numfmt(sheet = base_sheet_name, dims = paste0("A", base_sum_row, ":", gt_let, scen_sum_row), numfmt = "#,##0.00")
    wb$add_numfmt(sheet = base_sheet_name, dims = paste0("A", delta_sum_row, ":", gt_let, delta_sum_row), numfmt = "+#,##0.00;-#,##0.00;0.00")
    
    if (nrow(panel_df) > 0) {
      wb$add_data(sheet = base_sheet_name, x = as.data.frame(t(panel_headers)), start_col = panel_col, start_row = 6, col_names = FALSE)
      wb$add_data(sheet = base_sheet_name, x = panel_df, start_col = panel_col, start_row = 7, col_names = FALSE)
      for (pr in 7:(6 + nrow(panel_df))) {
        form_str <- paste0("=ROUND(", base_area_col_let, pr, "*(1+", adj_col_let, pr, "), 2)")
        wb$add_formula(sheet = base_sheet_name, x = form_str, dims = paste0(scen_area_col_let, pr))
      }
    }
    
    dashboard_rows[[length(dashboard_rows) + 1]] <- data.frame(
      PU_ID        = pu_id,
      PU_Name      = pu_name,
      Sheet_Name   = sheet_name,
      Active_Count = 0,
      Status       = "VALID",
      Transitions  = nrow(panel_df),
      stringsAsFactors = FALSE
    )
  }
  
  dash_df <- do.call(rbind, dashboard_rows)
  
  # 6. Reference Sheet for Dropdowns (_LULC_List)
  wb$add_worksheet("_LULC_List")
  wb$set_sheet_visibility(sheet = "_LULC_List", value = "hidden")
  
  unique_classes <- sort(unique(as.character(lc_lookup_table$LC)))
  if (length(unique_classes) == 0) unique_classes <- c("Class 1", "Class 2")
  wb$add_data(sheet = "_LULC_List", x = unique_classes, start_col = 1, start_row = 1, col_names = FALSE)
  
  default_target_pu_option <- if (lang == "id") "Semua Unit Perencanaan" else "All Planning Units"
  pu_options <- c(default_target_pu_option, sprintf("PU %s: %s", dash_df$PU_ID, dash_df$PU_Name))
  wb$add_data(sheet = "_LULC_List", x = pu_options, start_col = 2, start_row = 1, col_names = FALSE)
  
  # 7. Populate Master Dashboard Sheet
  # Title Block (Rows 2 & 3)
  dash_title <- if (lang == "id") {
    "SCIENDO Scenario Builder"
  } else {
    "SCIENDO Scenario Builder"
  }
  wb$add_data(sheet = "Dashboard", x = dash_title, start_col = 2, start_row = 2)
  wb$add_font(sheet = "Dashboard", dims = "B2", size = 14, bold = TRUE, color = openxlsx2::wb_color(hex = "1E3A8A"))
  
  dash_subtitle <- if (lang == "id") {
    "Pembangunan Skenario Melalui Matriks Probabilitas Transisi (TPM)"
  } else {
    "Transition Probability Matrix (TPM) Scenario Builder"
  }
  wb$add_data(sheet = "Dashboard", x = dash_subtitle, start_col = 2, start_row = 3)
  wb$add_font(sheet = "Dashboard", dims = "B3", size = 10, italic = TRUE, color = openxlsx2::wb_color(hex = "475569"))
  
  # Row 5: Action Toolbar space (height 32, buttons anchored in drawing1.xml)
  wb$set_row_heights(sheet = "Dashboard", rows = 5, heights = 32)
  
  # Rows 7-10: Dedicated Dashboard-Embedded Policy Card
  wb$set_row_heights(sheet = "Dashboard", rows = c(7, 8, 9, 10), heights = c(22, 25, 25, 20))
  
  # Card Header (Row 7)
  policy_hdr <- if (lang == "id") "Input Intervensi untuk Seluruh Unit Perencanaan" else "Intervention Input Across Planning Units"
  wb$add_data(sheet = "Dashboard", x = policy_hdr, start_col = 2, start_row = 7)
  wb$add_fill(sheet = "Dashboard", dims = "B7:G7", color = openxlsx2::wb_color(hex = "0F766E"))
  wb$add_font(sheet = "Dashboard", dims = "B7:G7", size = 11, bold = TRUE, color = openxlsx2::wb_color("white"))
  
  # Card Body styling (Rows 8-10 across columns B to G)
  wb$add_fill(sheet = "Dashboard", dims = "B8:G10", color = openxlsx2::wb_color(hex = "F8FAFC"))
  
  # Row 8: From Class & % Adjustment (and Apply Policy button in F8:G9)
  lbl_from <- if (lang == "id") "Kelas Asal:" else "From Class:"
  wb$add_data(sheet = "Dashboard", x = lbl_from, start_col = 2, start_row = 8)
  wb$add_font(sheet = "Dashboard", dims = "B8", bold = TRUE, color = openxlsx2::wb_color(hex = "334155"))
  wb$add_data(sheet = "Dashboard", x = unique_classes[1], start_col = 3, start_row = 8)
  
  lbl_adj <- if (lang == "id") "% Penyesuaian:" else "% Adjustment:"
  wb$add_data(sheet = "Dashboard", x = lbl_adj, start_col = 4, start_row = 8)
  wb$add_font(sheet = "Dashboard", dims = "D8", bold = TRUE, color = openxlsx2::wb_color(hex = "334155"))
  wb$add_data(sheet = "Dashboard", x = -0.50, start_col = 5, start_row = 8)
  wb$add_numfmt(sheet = "Dashboard", dims = "E8", numfmt = "0.0%")
  
  # Row 9: To Class & Target PU
  to_init <- if (length(unique_classes) > 1) unique_classes[2] else unique_classes[1]
  lbl_to <- if (lang == "id") "Kelas Tujuan:" else "To Class:"
  wb$add_data(sheet = "Dashboard", x = lbl_to, start_col = 2, start_row = 9)
  wb$add_font(sheet = "Dashboard", dims = "B9", bold = TRUE, color = openxlsx2::wb_color(hex = "334155"))
  wb$add_data(sheet = "Dashboard", x = to_init, start_col = 3, start_row = 9)
  
  lbl_pu <- if (lang == "id") "Target PU:" else "Target PU:"
  wb$add_data(sheet = "Dashboard", x = lbl_pu, start_col = 4, start_row = 9)
  wb$add_font(sheet = "Dashboard", dims = "D9", bold = TRUE, color = openxlsx2::wb_color(hex = "334155"))
  wb$add_data(sheet = "Dashboard", x = default_target_pu_option, start_col = 5, start_row = 9)
  
  # Row 10: Instructions hint
  policy_hint <- if (lang == "id") {
    "Petunjuk: Pilih Kelas Asal/Tujuan, tentukan % Penyesuaian, pilih Target PU, lalu klik Input Intervensi."
  } else {
    "Instructions: Select From/To classes, specify % Adjustment, choose Target PU, and click Input Intervention."
  }
  wb$add_data(sheet = "Dashboard", x = policy_hint, start_col = 2, start_row = 10)
  wb$add_font(sheet = "Dashboard", dims = "B10", size = 9, italic = TRUE, color = openxlsx2::wb_color(hex = "64748B"))
  
  # Attach native Excel Data Validation dropdowns to From Class, To Class, Target PU
  wb$add_data_validation(
    sheet = "Dashboard",
    dims  = "C8",
    type  = "list",
    value = paste0("'_LULC_List'!$A$1:$A$", length(unique_classes))
  )
  wb$add_data_validation(
    sheet = "Dashboard",
    dims  = "C9",
    type  = "list",
    value = paste0("'_LULC_List'!$A$1:$A$", length(unique_classes))
  )
  wb$add_data_validation(
    sheet = "Dashboard",
    dims  = "E9",
    type  = "list",
    value = paste0("'_LULC_List'!$B$1:$B$", length(pu_options))
  )
  
  # Row 11: Spacer row
  wb$set_row_heights(sheet = "Dashboard", rows = 11, heights = 10)
  
  # Rows 12–16: 4-Step Compact Instruction Box on Dashboard
  wb$set_row_heights(sheet = "Dashboard", rows = c(12, 13, 14, 15, 16), heights = c(20, 18, 18, 18, 18))
  dash_box_hdr <- if (lang == "id") "Panduan Singkat Dashboard:" else "Quick Dashboard Guide:"
  step1 <- if (lang == "id") {
    "1. Periksa daftar Unit Perencanaan dan status validasinya pada tabel ringkasan di bawah."
  } else {
    "1. Review the Planning Units list and validation statuses in the summary table below."
  }
  step2 <- if (lang == "id") {
    "2. Gunakan 'Input Intervensi untuk Seluruh Unit Perencanaan' untuk menerapkan intervensi secara serentak ke semua atau unit perencanaan tertentu."
  } else {
    "2. Use 'Intervention Input Across Planning Units' to input intervention across all or specific planning units."
  }
  step3_4 <- if (lang == "id") {
    "3. Klik tautan navigasi untuk membuka sheet Unit Perencanaan masing-masing. 4. Klik 'Validasi Semua Unit Perencanaan' untuk memastikan seluruh matriks valid (total baris = 1.000) sebelum menjalankan modul SCIENDO Simulate."
  } else {
    "3. Click navigation links to open individual Planning Unit sheets. 4. Click 'Validate All Sheets' to verify all matrices sum to 1.000 before running SCIENDO Simulate."
  }
  motw_note <- if (lang == "id") {
    "Catatan: Jika membuka di komputer lain, klik kanan file -> Properties -> centang 'Unblock' / Buka Blokir, atau jalankan berkas activate_macros.bat."
  } else {
    "Note: If opening on another computer, right-click file -> Properties -> check 'Unblock', or run the activate_macros.bat file."
  }
  
  wb$add_data(sheet = "Dashboard", x = dash_box_hdr, start_col = 2, start_row = 12)
  wb$add_fill(sheet = "Dashboard", dims = "B12:G12", color = openxlsx2::wb_color(hex = "E0F2FE"))
  wb$add_font(sheet = "Dashboard", dims = "B12:G12", size = 10, bold = TRUE, color = openxlsx2::wb_color(hex = "0369A1"))
  
  wb$add_data(sheet = "Dashboard", x = step1, start_col = 2, start_row = 13)
  wb$add_data(sheet = "Dashboard", x = step2, start_col = 2, start_row = 14)
  wb$add_data(sheet = "Dashboard", x = step3_4, start_col = 2, start_row = 15)
  wb$add_data(sheet = "Dashboard", x = motw_note, start_col = 2, start_row = 16)
  wb$add_fill(sheet = "Dashboard", dims = "B13:G16", color = openxlsx2::wb_color(hex = "F0F9FF"))
  wb$add_font(sheet = "Dashboard", dims = "B13:B15", size = 9.5, color = openxlsx2::wb_color(hex = "0F172A"))
  wb$add_font(sheet = "Dashboard", dims = "B16", size = 9, italic = TRUE, color = openxlsx2::wb_color(hex = "64748B"))
  
  # Row 17: Planning Unit Summary Table Headers
  wb$set_row_heights(sheet = "Dashboard", rows = 17, heights = 24)
  dash_headers <- if (lang == "id") {
    c("ID PU", "Nama Unit Perencanaan", "Tautan Navigasi", "Intervensi Aktif", "Status Validasi", "Transisi Historis")
  } else {
    c("PU ID", "Planning Unit Name", "Navigation Link", "Active Interventions", "Validation Status", "Historical Transitions")
  }
  wb$add_data(sheet = "Dashboard", x = as.data.frame(t(dash_headers)), start_col = 2, start_row = 17, col_names = FALSE)
  wb$add_fill(sheet = "Dashboard", dims = "B17:G17", color = openxlsx2::wb_color(hex = "1E3A8A"))
  wb$add_font(sheet = "Dashboard", dims = "B17:G17", color = openxlsx2::wb_color("white"), bold = TRUE)
  
  # Rows 18+: Planning Unit Summary Table Data
  for (i in seq_len(nrow(dash_df))) {
    row_num <- 17 + i
    wb$add_data(sheet = "Dashboard", x = dash_df$PU_ID[i], start_col = 2, start_row = row_num)
    wb$add_data(sheet = "Dashboard", x = dash_df$PU_Name[i], start_col = 3, start_row = row_num)
    
    link_label <- if (lang == "id") paste0("Buka Sheet ", dash_df$Sheet_Name[i]) else paste0("Go to ", dash_df$Sheet_Name[i])
    link_formula <- paste0('=HYPERLINK("#\'', dash_df$Sheet_Name[i], '\'!A1", "', link_label, '")')
    wb$add_formula(sheet = "Dashboard", x = link_formula, dims = paste0("D", row_num))
    wb$add_font(sheet = "Dashboard", dims = paste0("D", row_num), color = openxlsx2::wb_color("blue"), underline = "single", bold = TRUE)
    
    wb$add_data(sheet = "Dashboard", x = dash_df$Active_Count[i], start_col = 5, start_row = row_num)
    wb$add_data(sheet = "Dashboard", x = dash_df$Status[i], start_col = 6, start_row = row_num)
    wb$add_fill(sheet = "Dashboard", dims = paste0("F", row_num), color = openxlsx2::wb_color(hex = "DCFCE7"))
    wb$add_font(sheet = "Dashboard", dims = paste0("F", row_num), color = openxlsx2::wb_color(hex = "15803D"), bold = TRUE)
    
    wb$add_data(sheet = "Dashboard", x = dash_df$Transitions[i], start_col = 7, start_row = row_num)
  }
  
  # Column widths for Dashboard
  wb$set_col_widths(sheet = "Dashboard", cols = 2, widths = 14)
  wb$set_col_widths(sheet = "Dashboard", cols = 3, widths = 28)
  wb$set_col_widths(sheet = "Dashboard", cols = 4, widths = 26)
  wb$set_col_widths(sheet = "Dashboard", cols = 5, widths = 22)
  wb$set_col_widths(sheet = "Dashboard", cols = 6, widths = 18)
  wb$set_col_widths(sheet = "Dashboard", cols = 7, widths = 22)
  
  # Save workbook
  wb$save(output_file)
  
  # Localize DrawingML buttons in xl/drawings/drawing1.xml inside output_file
  tryCatch({
    unzip_dir <- tempfile("unzip_draw_")
    dir.create(unzip_dir, recursive = TRUE)
    utils::unzip(output_file, exdir = unzip_dir)
    dpath <- file.path(unzip_dir, "xl", "drawings", "drawing1.xml")
    if (file.exists(dpath)) {
      dlines <- readLines(dpath, warn = FALSE, encoding = "UTF-8")
      if (lang == "id") {
        dlines <- gsub("Validate All Sheets", "Validasi Semua Unit Perencanaan", dlines, fixed = TRUE)
        dlines <- gsub("Validasi Semua Lembar", "Validasi Semua Unit Perencanaan", dlines, fixed = TRUE)
        dlines <- gsub("Reset All to Baseline", "Atur Ulang ke Baseline", dlines, fixed = TRUE)
        dlines <- gsub("Apply Policy", "Aplikasikan Intervensi", dlines, fixed = TRUE)
        dlines <- gsub("Apply Intervention", "Aplikasikan Intervensi", dlines, fixed = TRUE)
        dlines <- gsub("Terapkan Kebijakan", "Aplikasikan Intervensi", dlines, fixed = TRUE)
        dlines <- gsub("Input Intervention", "Aplikasikan Intervensi", dlines, fixed = TRUE)
        dlines <- gsub("Input Intervensi", "Aplikasikan Intervensi", dlines, fixed = TRUE)
      } else {
        dlines <- gsub("Validasi Semua Unit Perencanaan", "Validate All Sheets", dlines, fixed = TRUE)
        dlines <- gsub("Validasi Semua Lembar", "Validate All Sheets", dlines, fixed = TRUE)
        dlines <- gsub("Atur Ulang ke Baseline", "Reset All to Baseline", dlines, fixed = TRUE)
        dlines <- gsub("Apply Policy", "Apply Intervention", dlines, fixed = TRUE)
        dlines <- gsub("Aplikasikan Intervensi", "Apply Intervention", dlines, fixed = TRUE)
        dlines <- gsub("Terapkan Kebijakan", "Apply Intervention", dlines, fixed = TRUE)
        dlines <- gsub("Input Intervensi", "Apply Intervention", dlines, fixed = TRUE)
        dlines <- gsub("Input Intervention", "Apply Intervention", dlines, fixed = TRUE)
      }
      writeLines(dlines, dpath, useBytes = TRUE)
      all_f <- list.files(unzip_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
      old_wd <- getwd()
      tmp_zip <- tempfile(fileext = ".xlsm")
      tryCatch({
        setwd(unzip_dir)
        zip::zip(tmp_zip, files = all_f)
      }, finally = {
        setwd(old_wd)
      })
      if (file.exists(tmp_zip) && file.info(tmp_zip)$size > 0) {
        file.copy(tmp_zip, output_file, overwrite = TRUE)
        unlink(tmp_zip)
      }
    }
    unlink(unzip_dir, recursive = TRUE)
  }, error = function(e) {
    message("Notice: DrawingML localization skipped or failed: ", e$message)
  })
  
  message("Consolidated TPM scenario workbook successfully generated: ", output_file)
  invisible(output_file)
}

#' Render a DataTable with Enhanced Features
#'
#' Creates an interactive DT::datatable with common extensions and styling options
#' pre-configured for ease of use. Includes export buttons, responsive design,
#' professional styling, and automatic numeric formatting.
#'
#' @param data A data frame or matrix containing the data to be displayed.
#' @param caption Character string specifying the table caption (optional).
#' @param digits Integer specifying the number of decimal places for percentages (default = 2).
#' @param area_digits Integer specifying the number of decimal places for area values (default = 0).
#' @param notification_timeout Time in milliseconds for the copy notification to auto-dismiss (default = 3000 = 3 seconds).
#'
#' @return A DT::datatable object with enhanced features and styling.
#'
#' @details
#' This function provides a convenient wrapper for creating DataTables with
#' commonly used features:
#' \itemize{
#'   \item \strong{Extensions}: Buttons (export functionality) and Responsive (mobile-friendly)
#'   \item \strong{Options}: Pagination, search, fixed columns, auto-width, ordering
#'   \item \strong{Styling}: Display class with stripe and hover effects
#'   \item \strong{Export}: Copy, CSV, and Excel export buttons
#'   \item \strong{Formatting}: Automatic numeric formatting with thousands separators
#'   \item \strong{Notification}: Auto-dismissing copy notifications
#' }
#'
#' The DOM layout includes Buttons (B), length menu (l), filter (f), 
#' processing (r), table (t), information (i), and pagination (p).
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' render_dt_table(mtcars, caption = "Motor Trend Car Road Tests")
#'
#' # Without caption
#' render_dt_table(iris)
#'
#' # Custom decimal places
#' render_dt_table(mtcars, digits = 0)
#'
#' # Use in R Markdown
#' ```{r}
#' library(DT)
#' render_dt_table(mtcars, "Sample Data Table")
#' ```
#' }
#'
#' @seealso
#' \code{\link[DT]{datatable}}, \code{\link[DT]{DTOutput}}
#'
#' @export
render_dt_table <- function(data, caption = NULL, digits = 2, area_digits = 0, notification_timeout = 1000) {
  css_fix <- htmltools::tags$style(htmltools::HTML(sprintf("
    div.dt-button-info {
      position: fixed;
      top: 50%%;
      left: 50%%;
      transform: translate(-50%%, -50%%);
      z-index: 10000;
      background: white;
      padding: 20px;
      border: 2px solid #999;
      border-radius: 5px;
      box-shadow: 0 0 10px rgba(0,0,0,0.3);
      animation: fadeOut %dms ease-in %dms forwards;
    }
    
    @keyframes fadeOut {
      from { opacity: 1; }
      to { opacity: 0; visibility: hidden; }
    }
  ", notification_timeout, notification_timeout)))
  
  formatted_data <- data
  numeric_cols <- which(sapply(data, function(x) {
    is.numeric(x) || (inherits(x, "units") && is.numeric(as.numeric(x)))
  }))
  
  # Apply formatting to numeric columns
  if (length(numeric_cols) > 0) {
    for (col in numeric_cols) {
      col_data <- data[[col]]
      col_name <- names(data)[col]
      is_percentage_col <- grepl("^%|Percent|Percentage|% T1|% T2", col_name, ignore.case = TRUE)
      if (inherits(col_data, "units")) {
        numeric_values <- as.numeric(col_data)
        units_attr <- attributes(col_data)
        is_integer_col <- all(numeric_values == floor(numeric_values), na.rm = TRUE)
        if (is_integer_col) {
          formatted_values <- format(numeric_values, big.mark = ",", scientific = FALSE, trim = TRUE)
        } else {
          formatted_values <- format(round(numeric_values, area_digits), big.mark = ",", scientific = FALSE, nsmall = area_digits, trim = TRUE)
        }
        if (!is.null(units_attr$units)) {
          formatted_data[[col]] <- paste(formatted_values, units_attr$units)
        } else {
          formatted_data[[col]] <- formatted_values
        }
      } else {
        is_integer_col <- all(col_data == floor(col_data), na.rm = TRUE)
        if (is_percentage_col) {
          formatted_data[[col]] <- format(round(col_data, digits), nsmall = digits, trim = TRUE)
        } else if (is_integer_col) {
          formatted_data[[col]] <- format(col_data, big.mark = ",", scientific = FALSE, trim = TRUE)
        } else {
          formatted_data[[col]] <- format(round(col_data, area_digits), big.mark = ",", scientific = FALSE, nsmall = area_digits, trim = TRUE)
        }
      }
    }
  }
  
  dt <- DT::datatable(
    formatted_data,
    extensions = c('Buttons', 'Responsive'),
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Blfrtip',
      buttons = list(
        list(extend = "copy", 
             className = "btn btn-light btn-sm",
             text = "Copy",
             title = caption),
        list(extend = "csv",  
             className = "btn btn-light btn-sm",
             title = caption),
        list(extend = "excel", 
             className = "btn btn-light btn-sm",
             title = caption)
      )
    ),
    class = "display stripe hover",
    caption = caption,
    rownames = FALSE
  )
  
  htmltools::tagList(css_fix, dt)
}

#' Extract Calendar Year from Uploaded File Name
#'
#' Auto-detects 4-digit or 2-digit years from a filename string.
#' 4-digit years (1900-2099) take precedence.
#' 2-digit years: 00-70 -> 2000-2070, 71-99 -> 1971-1999.
#'
#' @param filename Character string.
#' @return Integer year or NA_integer_.
#' @export
extract_year_from_filename <- function(filename) {
  if (is.null(filename) || length(filename) == 0 || is.na(filename)) {
    return(NA_integer_)
  }
  name_no_ext <- sub("\\.[^.]+$", "", basename(filename))
  
  # 1. 4-digit year (1900-2099)
  m4 <- gregexpr("(?<![0-9])(?:19|20)[0-9]{2}(?![0-9])", name_no_ext, perl = TRUE)
  matches4 <- regmatches(name_no_ext, m4)[[1]]
  if (length(matches4) > 0) {
    return(as.integer(tail(matches4, 1)))
  }
  
  # Remove resolutions like 250m, 30m and versions like v1, v2
  cleaned <- gsub("(?i)[0-9]+m(?![a-z])", "", name_no_ext, perl = TRUE)
  cleaned <- gsub("(?i)v[0-9]+", "", cleaned, perl = TRUE)
  
  # 2. 2-digit year (00-70 -> 2000-2070, 71-99 -> 1971-1999)
  m2 <- gregexpr("(?<![0-9])[0-9]{2}(?![0-9])", cleaned, perl = TRUE)
  matches2 <- regmatches(cleaned, m2)[[1]]
  if (length(matches2) > 0) {
    val <- as.integer(tail(matches2, 1))
    if (!is.na(val)) {
      if (val >= 0 && val <= 70) {
        return(2000L + val)
      } else if (val >= 71 && val <= 99) {
        return(1900L + val)
      }
    }
  }
  return(NA_integer_)
}

#' Help Text Definitions for SCIENDO Train Inputs
#' @export
train_help_texts <- list(
  map1_file = list(
    title = "Land Cover Map at T1 (Initial)",
    body = "<p>Raster GeoTIFF (.tif) map representing land cover / use at the initial time period (T1).</p><p>Pixel values must correspond to the numeric IDs defined in the Land Use/Cover Lookup Table.</p>"
  ),
  map1_year = list(
    title = "Year of T1",
    body = "<p>Calendar year corresponding to the initial land cover map (T1), e.g. <code>2015</code>.</p><p>Used alongside the Year of T2 to determine the time interval for transition probability rates. This field is automatically prefilled if a year is detected in the uploaded file name.</p>"
  ),
  map2_file = list(
    title = "Land Cover Map at T2 (Final)",
    body = "<p>Raster GeoTIFF (.tif) map representing land cover / use at the final time period (T2).</p><p>Must share the spatial extent, resolution, and coordinate reference system of the T1 land cover map.</p>"
  ),
  map2_year = list(
    title = "Year of T2",
    body = "<p>Calendar year corresponding to the final land cover map (T2), e.g. <code>2020</code>.</p><p>Must be greater than the Year of T1. This field is automatically prefilled if a year is detected in the uploaded file name.</p>"
  ),
  lc_file = list(
    title = "Land Use/Cover Lookup Table",
    body = "<p>CSV file mapping numeric land cover IDs to descriptive class names.</p><p>It must contain at least two columns:</p><ol><li>Numeric ID (integer)</li><li>Land cover class name (text)</li></ol>"
  ),
  zone_type = list(
    title = "Planning Unit Input Type",
    body = "<p>Select whether the Planning Unit dataset is supplied as a categorical <b>Raster (.tif)</b> or a polygon <b>Shapefile (.shp)</b>.</p>"
  ),
  mapz_file = list(
    title = "Planning Unit Map",
    body = "<p>Spatial delineation of planning units or administrative zones (e.g. districts, watersheds, or spatial planning units).</p><p>Can be supplied as:</p><ul><li><b>Raster (.tif):</b> Categorical GeoTIFF where each integer pixel value corresponds to a planning unit ID.</li><li><b>Shapefile (.shp, .shx, .dbf, .prj):</b> Polygon vector features delineating planning units.</li></ul>"
  ),
  z_file = list(
    title = "Planning Unit Lookup Table",
    body = "<p>CSV table mapping numeric planning unit IDs to descriptive zone names.</p><p>Required when using raster planning units. Must contain at least two columns:</p><ol><li>Planning Unit ID (integer)</li><li>Planning Unit Name (text)</li></ol>"
  ),
  factors_path = list(
    title = "Predictor Maps Directory",
    body = "<p>Directory containing spatial predictor raster files in GeoTIFF (.tif) format.</p><p>These predictors represent spatial drivers of land use change (e.g., elevation, slope, distance to roads, distance to rivers, distance to settlements). All predictor maps will be assembled into a raster cube used to calculate Weights of Evidence.</p>"
  ),
  wd = list(
    title = "Output Directory",
    body = "<p>Folder where all SCIENDO Train analysis outputs, including transition matrices, raster cube, weights of evidence reports, and HTML summary reports, will be saved.</p>"
  ),
  memory_allocation = list(
    title = "Memory Allocation Policy",
    body = "<p>Controls how DINAMICA EGO manages RAM and disk space when executing the model.</p><ul><li><b>Balanced (default):</b> Keeps inputs in memory; keeps results in memory when RAM allows, otherwise writes to disk.</li><li><b>Prefer Memory:</b> Keeps both inputs and results in RAM whenever possible.</li><li><b>Prefer Disk:</b> Streams maps to/from disk to minimize RAM usage.</li><li><b>Memory Only:</b> Aborts execution if RAM is insufficient.</li><li><b>Aggressive:</b> Keeps inputs on disk while results are kept in RAM.</li></ul>"
  ),
  dinamica_path = list(
    title = "DINAMICA EGO Path (Optional)",
    body = "<p>Path to the DINAMICA EGO installation directory (e.g., <code>C:/Program Files/Dinamica EGO 7</code>).</p><p>If left blank, LUMENS automatically detects the installation under <code>Program Files</code>.</p>"
  )
)

#' Clickable Help Icon for SCIENDO Train UI
#'
#' @param id Help topic ID.
#' @return A `shiny.tag` containing an actionLink with circle-question icon.
#' @export
train_help_icon <- function(id) {
  tags$span(
    class = "alloc-help-icon",
    actionLink(
      inputId = paste0("train_help_", id),
      label   = NULL,
      icon    = icon("circle-question")
    )
  )
}

#' Label With a Right-Aligned Help Icon
#'
#' @param text Label text.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` combining label text and a help icon.
#' @export
label_with_help <- function(text, help_id) {
  tags$div(
    class = "alloc-label",
    tags$span(text),
    train_help_icon(help_id)
  )
}

#' Directory-Picker Button With an Inline Help Icon
#'
#' @param id Shiny input ID.
#' @param label Button label.
#' @param title Button title tooltip.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` containing directory button and help icon.
#' @export
dir_button_with_help <- function(id, label, title, help_id, label_key = NULL) {
  btn <- shinyDirButton(id, label, title)
  if (!is.null(label_key)) {
    btn[[2]]$children <- list(tags$span(class = "i18n", `data-key` = label_key, label))
  }
  tags$div(
    class = "dir-btn-wrapper",
    btn,
    tags$div(
      class = "dir-help-overlay",
      train_help_icon(help_id)
    )
  )
}
