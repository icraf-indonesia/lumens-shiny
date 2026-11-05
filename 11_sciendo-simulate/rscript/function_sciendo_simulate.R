# ---------------------------------------------------------------- session ---

#' Format Session Info as a Tibble
#'
#' Summarises the current R session: R version, platform/OS, library paths,
#' and locale.
#'
#' @return A tibble with columns `Category` and `Details`.
#' @importFrom tibble tibble
#' @export
format_session_info_table <- function() {
  si <- sessionInfo()
  
  r_version <- si$R.version[c("major", "minor", "year", "month", "day", "nickname")]
  r_version <- paste0(
    "R ", r_version$major, ".", r_version$minor,
    " (", r_version$year, "-", r_version$month, "-", r_version$day, ")",
    " '", r_version$nickname, "'"
  )
  
  platform_os  <- paste(si$platform, "|", si$running)
  locale_info  <- paste(strsplit(si[[3]], ";")[[1]], collapse = "<br>")
  lib_paths    <- .libPaths() |> paste(collapse = "<br>")
  
  tibble::tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details  = c(r_version, platform_os, lib_paths, locale_info)
  )
}

# ------------------------------------------------------------- utilities ---

#' Install and Load Packages
#'
#' Installs any missing packages and loads all of them.
#'
#' @param package1 Character vector (or single name) of package names.
#' @param ... Additional package names.
#' @return Invisibly `NULL`; called for its side effects.
#' @export
install_load <- function(package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (!package %in% rownames(installed.packages())) {
      install.packages(package)
    }
    do.call("library", list(package))
  }
}

# --------------------------------------------------------- spatial helpers ---

#' Spatially Sync Rasters
#'
#' Aligns ("syncs") a raster to a reference raster by matching projection,
#' resolution, and extent.
#'
#' @param unsynced A `Raster` object to be aligned.
#' @param reference A `Raster` object used as the alignment reference.
#' @param method Resampling method, `"ngb"` or `"bilinear"`.
#' @param size_only Logical. If `TRUE`, resize only, using `raster_size`.
#' @param raster_size Numeric vector `c(ncol, nrow)`. Required when
#'   `size_only = TRUE`.
#' @param verbose Logical. If `TRUE`, emit progress messages.
#' @param ... Passed to `writeRaster`.
#'
#' @return A `RasterLayer`, `RasterBrick`, or `RasterStack` synced to
#'   `reference`.
#' @importFrom raster projection res bbox rotate projectExtent setExtent
#'   resample extend extent crop
#' @export
spatial_sync_raster <- function(unsynced, reference, method = "ngb",
                                size_only = FALSE, raster_size,
                                verbose = FALSE, ...) {
  if (!size_only) {
    new_projection <- projection(reference)
    old_projection <- projection(unsynced)
    new_res        <- res(reference)
    old_res        <- res(unsynced)
    new_extent     <- bbox(reference)
    old_extent     <- bbox(unsynced)
    
    if ((new_extent[1, 1] < 0 && old_extent[1, 1] >= 0) ||
        (new_extent[1, 1] >= 0 && old_extent[1, 1] < 0)) {
      if (verbose) message("Rotating...")
      unsynced_rotated <- rotate(unsynced)
    } else {
      unsynced_rotated <- unsynced
    }
    
    if (new_projection != old_projection ||
        new_res[1] != old_res[1] || new_res[2] != old_res[2]) {
      pr_extent <- projectExtent(unsynced_rotated, new_projection)
      pr_extent <- setExtent(pr_extent, extent(reference))
      res(pr_extent) <- res(reference)
      
      if (new_projection != old_projection) {
        if (verbose) message("Projecting and resampling...")
        pr <- projectRaster(unsynced_rotated, pr_extent, method = method)
      } else {
        if (verbose) message("Same projection, resampling only...")
        pr <- raster::resample(unsynced_rotated, pr_extent, method = method)
      }
    } else {
      if (verbose) message("Same projection and pixel size...")
      pr <- unsynced_rotated
    }
    
    if (verbose) message("Expanding...")
    expanded_raster <- extend(pr, reference)
    if (verbose) message("Cropping...")
    synced_raster <- crop(expanded_raster, reference)
    if (verbose) message("Fixing extents...")
    extent(synced_raster) <- extent(reference)
  } else {
    unsynced_ncol <- ncol(unsynced)
    unsynced_nrow <- nrow(unsynced)
    
    unsynced_ulx <- (raster_size[[1]] - unsynced_ncol) / 2
    unsynced_uly <- (raster_size[[2]] - unsynced_nrow) / 2
    
    extent(unsynced) <- extent(
      unsynced_ulx, unsynced_ulx + unsynced_ncol,
      unsynced_uly, unsynced_uly + unsynced_nrow
    )
    full_extent <- extent(0, raster_size[[1]], 0, raster_size[[2]])
    
    synced_raster <- extend(unsynced, full_extent)
    extent(synced_raster) <- full_extent
    res(synced_raster)    <- c(1, 1)
  }
  
  synced_raster
}

# ------------------------------------------------------- cross-tabulation ---

#' Generate a Dummy Cross-tabulation
#'
#' Cross-tabulates two data frames to build a contingency table of land
#' cover / planning unit combinations.
#'
#' @param landcover Data frame. Land cover lookup table.
#' @param zone Data frame. Zone (planning unit) lookup table.
#' @return A data frame with columns `ID_PU`, `ID_LC1`, `ID_LC2`.
#' @importFrom splitstackshape expandRows
#' @export
generate_dummy_crosstab <- function(landcover, zone) {
  if (!is.data.frame(landcover)) stop("Land cover is not a data frame")
  if (!is.data.frame(zone))      stop("Zone is not a data frame")
  
  n_lc <- nrow(landcover)
  n_pu <- nrow(zone)
  
  dummy1 <- data.frame(nPU = zone[, 1], divider = n_lc * n_lc)
  dummy1 <- expandRows(dummy1, "divider")
  
  dummy2 <- data.frame(nT1 = landcover[, 1], divider = n_lc)
  dummy2 <- expandRows(dummy2, "divider")
  dummy2 <- data.frame(nT1 = rep(dummy2$nT1, n_pu))
  
  dummy3 <- data.frame(nT2 = rep(rep(landcover[, 1], n_lc), n_pu))
  
  lucDummy <- cbind(dummy1, dummy2, dummy3)
  colnames(lucDummy) <- c("ID_PU", "ID_LC1", "ID_LC2")
  lucDummy
}

# --------------------------------------------------------- plotting utils ---

#' Plot a Categorical Raster with an Optional Download Button
#'
#' Renders a categorical map with **ggplot2** / **tidyterra**. In HTML output
#' an inline PNG plus a **Download PNG** button is returned; otherwise a
#' plain `ggplot` object is returned.
#'
#' If the raster's category table includes a `color_palette` column of hex
#' codes, those colors are used; otherwise a default 50-color palette is
#' applied.
#'
#' @param raster_object A `SpatRaster` with categorical data.
#' @param filename Default filename for the downloaded PNG (HTML only).
#' @param dpi Resolution for the saved PNG (HTML only).
#' @return `htmltools::tagList` in HTML output, otherwise a `ggplot`.
#' @importFrom tidyterra geom_spatraster
#' @importFrom ggplot2 ggplot theme_bw labs theme scale_fill_manual
#'   element_text unit element_blank guides guide_legend ggsave
#' @importFrom htmltools tagList tags
#' @importFrom knitr is_html_output
#' @importFrom base64enc dataURI
#' @export
plot_categorical_raster <- function(raster_object, filename = "raster_plot.png",
                                    dpi = 300) {
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
  
  if (!knitr::is_html_output()) return(plot_lc)
  
  tf <- tempfile(fileext = ".png")
  plot_width <- if (legend_ncol > 1) 10.5 else 9.0
  plot_height <- max(5.5, min(9.0, 4.0 + (n_cats / legend_ncol) * 0.18))
  ggsave(tf, plot_lc, width = plot_width, height = plot_height, dpi = dpi)
  img_data <- base64enc::dataURI(file = tf, mime = "image/png")
  
  htmltools::tagList(
    tags$div(
      style = "margin-bottom:10px;",
      tags$img(
        src   = img_data,
        style = "max-width:100%; height:auto; display:block; margin-bottom:5px;"
      ),
      tags$button(
        "Download PNG",
        onclick = sprintf(
          "var link = document.createElement('a'); link.download = '%s'; link.href = this.previousElementSibling.src; link.click();",
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

# ------------------------------------------------------- DINAMICA helpers ---

#' Execute a DINAMICA EGO Model
#'
#' Launches the DINAMICA EGO console with the given `.egoml` model file.
#'
#' @param params List with elements `dinamica_path` and `egoml`.
#' @param memory_allocation Memory-allocation policy string passed to the
#'   console.
#' @return Invisibly `NULL`; called for its side effects.
#' @importFrom magrittr %>%
#' @importFrom purrr nth
#' @export
executeDINAMICA <- function(params, memory_allocation) {
  dinamica_path <- params$dinamica_path
  message(paste("Using DINAMICA EGO installation:", dinamica_path))
  
  if (!dir.exists(dinamica_path)) {
    stop("Specified DINAMICA EGO directory does not exist.")
  }
  
  dinamica_exe <- dinamica_path %>%
    list.files(pattern = "^DinamicaConsole", full.names = TRUE) %>%
    nth(2)
  
  if (!file.exists(params$egoml)) {
    stop("Specified egoml does not exist.")
  }
  
  command <- paste0(
    '"', dinamica_exe, '" ',
    "-processors 0 -log-level 4 ",
    "-memory-allocation-policy ", memory_allocation, " ",
    '"', params$egoml, '"'
  )
  
  result <- system(command)
  
  if (result != 0) {
    stop("DINAMICA EGO execution failed. Check DINAMICA EGO installation and parameters.")
  }
  message("DINAMICA EGO execution completed successfully.")
}

#' Generate a SCIENDO Simulation `.egoml`
#'
#' Writes a DINAMICA EGO `.egoml` model file that loads the initial
#' landscape, static variables, and planning units; iterates a per-region
#' allocation using the transition matrix and Weights of Evidence; and saves
#' the resulting landscape (and optionally probability) rasters.
#'
#' @param lc1_path Path to the initial land cover raster.
#' @param lusim_lc Data frame of land cover IDs and names.
#' @param zone_path Path to the planning unit raster.
#' @param ers_path Path to the static variables raster.
#' @param n_rep Number of simulation repetitions.
#' @param tm_path Directory with transition matrix files.
#' @param dcf_path Directory with WoE `.dcf` files.
#' @param output_dir Directory for outputs.
#' @param probability Logical. Save probability maps.
#' @param egoml Base name for the `.egoml` file.
#' @param memory_allocation Memory-allocation policy string.
#' @param allocate_transitions_temp Reserved.
#' @param percent,exp_mean,exp_var,exp_iso,gen_mean,gen_var,gen_iso Uniform
#'   `AllocateTransitions` parameters.
#' @param override_df Optional per-transition override data frame.
#' @return A list describing the generated model and its inputs.
#' @importFrom XML xmlOutputDOM saveXML
#' @export
generate_egoml_simulate <- function(lc1_path, lusim_lc,
                                    zone_path, ers_path, n_rep,
                                    tm_path, dcf_path,
                                    output_dir, probability = FALSE,
                                    egoml, memory_allocation = NULL,
                                    allocate_transitions_temp = NULL,
                                    percent  = NULL, exp_mean = NULL,
                                    exp_var  = NULL, exp_iso  = NULL,
                                    gen_mean = NULL, gen_var  = NULL,
                                    gen_iso  = NULL,
                                    override_df = NULL) {
  prob_path      <- paste0(output_dir, "/probabilities.tif")
  landscape_path <- paste0(output_dir, "/landscape.tif")
  
  allocate_transitions <- build_allocate_transitions(
    lusim_lc    = lusim_lc,
    percent     = percent,
    exp_mean    = exp_mean,
    exp_var     = exp_var,
    exp_iso     = exp_iso,
    gen_mean    = gen_mean,
    gen_var     = gen_var,
    gen_iso     = gen_iso,
    override_df = override_df
  )
  
  skeleton <- expand.grid(nT1 = lusim_lc[, 1], nT2 = lusim_lc[, 1])
  skeleton <- skeleton[skeleton$nT1 != skeleton$nT2, ]
  skeleton <- na.omit(skeleton)
  rownames(skeleton) <- NULL
  skeleton$char <- paste(skeleton$nT1, skeleton$nT2, sep = "->")
  
  con <- xmlOutputDOM(tag = "script")
  con$addTag("property", attrs = c(key = "dff.date",    value = "2016-Nov-09 17:01:03"))
  con$addTag("property", attrs = c(key = "dff.version", value = "3.0.17.20160922"))
  
  # Static variables
  con$addTag("functor", attrs = c(name = "LoadMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "Static Variables"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment", value = "Static variable maps."))
  con$addTag("inputport", attrs = c(name = "filename"),      paste0('"', ers_path, '"'))
  con$addTag("inputport", attrs = c(name = "nullValue"),     ".none")
  con$addTag("inputport", attrs = c(name = "loadAsSparse"),  ".no")
  con$addTag("inputport", attrs = c(name = "suffixDigits"),  0)
  con$addTag("inputport", attrs = c(name = "step"),          "0")
  con$addTag("inputport", attrs = c(name = "workdir"),       ".none")
  con$addTag("outputport", attrs = c(name = "map", id = "v1"))
  con$closeTag("functor")
  
  # Initial landscape
  con$addTag("functor", attrs = c(name = "LoadCategoricalMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "Initial Landscape"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment", value = "Initial landscape maps."))
  con$addTag("inputport", attrs = c(name = "filename"),      paste0('"', lc1_path, '"'))
  con$addTag("inputport", attrs = c(name = "nullValue"),     ".none")
  con$addTag("inputport", attrs = c(name = "loadAsSparse"),  ".no")
  con$addTag("inputport", attrs = c(name = "suffixDigits"),  0)
  con$addTag("inputport", attrs = c(name = "step"),          "0")
  con$addTag("inputport", attrs = c(name = "workdir"),       ".none")
  con$addTag("outputport", attrs = c(name = "map", id = "v2"))
  con$closeTag("functor")
  
  # Planning unit
  con$addTag("functor", attrs = c(name = "LoadCategoricalMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "Planning Unit"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment", value = "Municipalities"))
  con$addTag("inputport", attrs = c(name = "filename"),      paste0('"', zone_path, '"'))
  con$addTag("inputport", attrs = c(name = "nullValue"),     ".none")
  con$addTag("inputport", attrs = c(name = "loadAsSparse"),  ".no")
  con$addTag("inputport", attrs = c(name = "suffixDigits"),  0)
  con$addTag("inputport", attrs = c(name = "step"),          "0")
  con$addTag("inputport", attrs = c(name = "workdir"),       ".none")
  con$addTag("outputport", attrs = c(name = "map", id = "v3"))
  con$closeTag("functor")
  
  # Region loop
  con$addTag("containerfunctor", attrs = c(name = "RegionManager"), close = FALSE)
  con$addTag("property",           attrs = c(key = "dff.container.collapsed", value = "no"))
  con$addTag("property",           attrs = c(key = "dff.functor.alias",       value = "regionManager3260"))
  con$addTag("inputport",          attrs = c(name = "regions",      peerid = "v3"))
  con$addTag("inputport",          attrs = c(name = "borderCells"), 0)
  con$addTag("internaloutputport", attrs = c(name = "regionManager", id = "v4"))
  
  # Repetition loop
  con$addTag("containerfunctor", attrs = c(name = "Repeat"), close = FALSE)
  con$addTag("property",           attrs = c(key = "dff.container.collapsed", value = "no"))
  con$addTag("property",           attrs = c(key = "dff.functor.alias",       value = "repeat279"))
  con$addTag("property",           attrs = c(key = "dff.functor.comment",     value = "Simulation model."))
  con$addTag("inputport",          attrs = c(name = "iterations"), n_rep)
  con$addTag("internaloutputport", attrs = c(name = "step", id = "v5"))
  
  # Landscape mux
  con$addTag("functor", attrs = c(name = "MuxCategoricalMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "Landscape"))
  con$addTag("inputport", attrs = c(name = "initial",  peerid = "v2"))
  con$addTag("inputport", attrs = c(name = "feedback", peerid = "v15"))
  con$addTag("outputport", attrs = c(name = "map", id = "v6"))
  con$closeTag("functor")
  
  # Save landscape
  con$addTag("functor", attrs = c(name = "SaveMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "saveMap282"))
  con$addTag("inputport", attrs = c(name = "map",            peerid = "v15"))
  con$addTag("inputport", attrs = c(name = "filename"),      paste0('"', landscape_path, '"'))
  con$addTag("inputport", attrs = c(name = "suffixDigits"),  2)
  con$addTag("inputport", attrs = c(name = "step",           peerid = "v5"))
  con$addTag("inputport", attrs = c(name = "useCompression"), ".yes")
  con$addTag("inputport", attrs = c(name = "workdir"),        ".none")
  con$closeTag("functor")
  
  # Save probabilities (optional)
  if (probability) {
    con$addTag("functor", attrs = c(name = "SaveMap"), close = FALSE)
    con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "saveMap3414"))
    con$addTag("inputport", attrs = c(name = "map",            peerid = "v16"))
    con$addTag("inputport", attrs = c(name = "filename"),      paste0('"', prob_path, '"'))
    con$addTag("inputport", attrs = c(name = "suffixDigits"),  4)
    con$addTag("inputport", attrs = c(name = "step",           peerid = "v5"))
    con$addTag("inputport", attrs = c(name = "useCompression"), ".yes")
    con$addTag("inputport", attrs = c(name = "workdir"),        ".none")
    con$closeTag("functor")
  }
  
  # Category loop
  con$addTag("containerfunctor", attrs = c(name = "ForEachCategory"), close = FALSE)
  con$addTag("property",           attrs = c(key = "dff.container.collapsed", value = "no"))
  con$addTag("property",           attrs = c(key = "dff.functor.alias",       value = "forEachCategory283"))
  con$addTag("inputport",          attrs = c(name = "categorization", peerid = "v3"))
  con$addTag("internaloutputport", attrs = c(name = "step", id = "v7"))
  
  con$addTag("functor", attrs = c(name = "IntegerValue"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "int290"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment",
                                    value = "This operator is used here to force a dependence between two groups."))
  con$addTag("inputport",  attrs = c(name = "constant"), 0)
  con$addTag("outputport", attrs = c(name = "object", id = "v8"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "LoadTable"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "Transition Matrix"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment", value = "Load transition matrix."))
  con$addTag("inputport", attrs = c(name = "filename"),      paste0('"', tm_path, '/single_step.csv"'))
  con$addTag("inputport", attrs = c(name = "suffixDigits"),  6)
  con$addTag("inputport", attrs = c(name = "step",           peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "workdir"),        ".none")
  con$addTag("outputport", attrs = c(name = "table", id = "v9"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "LoadWeights"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "Weights of Evidence Coefficients"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment", value = "Load Weights of Evidence coefficients."))
  con$addTag("inputport", attrs = c(name = "filename"),      paste0('"', dcf_path, '/woe.dcf"'))
  con$addTag("inputport", attrs = c(name = "suffixDigits"),  6)
  con$addTag("inputport", attrs = c(name = "step",           peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "workdir"),        ".none")
  con$addTag("outputport", attrs = c(name = "weights", id = "v10"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "RegionalCategoricalMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "regionalCategoricalMap289"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment",
                                    value = "Assign a map to the region using the given identifier."))
  con$addTag("inputport", attrs = c(name = "globalMapName"), paste0('"landscape"'))
  con$addTag("inputport", attrs = c(name = "regionalMap",    peerid = "v11"))
  con$addTag("inputport", attrs = c(name = "regionId",       peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "regionManager",  peerid = "v4"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "AllocateTransitions"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "Updated Landscape (Region)"))
  con$addTag("inputport", attrs = c(name = "lanscape",         peerid = "v13"))
  con$addTag("inputport", attrs = c(name = "probabilities",    peerid = "v14"))
  con$addTag("inputport", attrs = c(name = "transitionMatrix", peerid = "v9"))
  con$addTag("inputport", attrs = c(name = "percentOfTransitionsByExpansion"),
             allocate_transitions$percentOfTransitionsByExpansion)
  con$addTag("inputport", attrs = c(name = "patchExpansionParameters"),
             allocate_transitions$patchExpansionParameters)
  con$addTag("inputport", attrs = c(name = "patchGenerationParameters"),
             allocate_transitions$patchGenerationParameters)
  con$addTag("inputport", attrs = c(name = "printTransitionInfo"), ".no")
  con$addTag("outputport", attrs = c(name = "resultingLanscape", id = "v11"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "RegionalizeMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "Static Variables (Region)"))
  con$addTag("inputport", attrs = c(name = "globalMap",          peerid = "v1"))
  con$addTag("inputport", attrs = c(name = "regionId",           peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs = c(name = "regionManager",      peerid = "v4"))
  con$addTag("outputport", attrs = c(name = "regionalMap", id = "v12"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "RegionalizeCategoricalMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "Landscape (Region)"))
  con$addTag("inputport", attrs = c(name = "globalMap",          peerid = "v6"))
  con$addTag("inputport", attrs = c(name = "regionId",           peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs = c(name = "regionManager",      peerid = "v4"))
  con$addTag("outputport", attrs = c(name = "regionalMap", id = "v13"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "RegionalMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "regionalMap3412"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment",
                                    value = "Assign a map to the region using the given identifier."))
  con$addTag("inputport", attrs = c(name = "globalMapName"), paste0('"probabilities"'))
  con$addTag("inputport", attrs = c(name = "regionalMap",    peerid = "v14"))
  con$addTag("inputport", attrs = c(name = "regionId",       peerid = "v7"))
  con$addTag("inputport", attrs = c(name = "regionManager",  peerid = "v4"))
  con$closeTag("functor")
  
  con$addTag("containerfunctor", attrs = c(name = "CalcWOfEProbabilityMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.container.collapsed", value = "no"))
  con$addTag("property",  attrs = c(key = "dff.functor.alias",       value = "Probabilities (Region)"))
  con$addTag("property",  attrs = c(key = "dff.functor.extendedcomment",
                                    value = "Calculate probability map."))
  con$addTag("inputport", attrs = c(name = "landscape",   peerid = "v13"))
  con$addTag("inputport", attrs = c(name = "weights",     peerid = "v10"))
  con$addTag("inputport", attrs = c(name = "transitions"),
             paste0('[ ', paste(skeleton$char, collapse = ", "), ']'))
  con$addTag("inputport", attrs = c(name = "cellType"),   ".uint8")
  con$addTag("inputport", attrs = c(name = "nullValue"),  ".default")
  con$addTag("outputport", attrs = c(name = "probabilities", id = "v14"))
  
  con$addTag("functor", attrs = c(name = "NameMap"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "nameMap298"))
  con$addTag("inputport", attrs = c(name = "map",     peerid = "v12"))
  con$addTag("inputport", attrs = c(name = "mapName"), paste0('"static_var"'))
  con$closeTag("functor")
  
  con$closeTag("containerfunctor")  # CalcWOfEProbabilityMap
  con$closeTag("containerfunctor")  # ForEachCategory
  
  con$addTag("containerfunctor", attrs = c(name = "Group"), close = FALSE)
  con$addTag("property", attrs = c(key = "dff.container.collapsed", value = "no"))
  con$addTag("property", attrs = c(key = "dff.functor.alias",       value = "group300"))
  
  con$addTag("functor", attrs = c(name = "IntegerValue"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "int302"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment",
                                    value = "This operator is used here to force a dependence between two groups."))
  con$addTag("inputport", attrs = c(name = "constant", peerid = "v8"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs = c(name = "MergeRegionalCategoricalMaps"), close = FALSE)
  con$addTag("property",  attrs = c(key = "dff.functor.alias",   value = "Updated Landscape"))
  con$addTag("property",  attrs = c(key = "dff.functor.comment",
                                    value = "Merge all maps assigned to the regions using the given identifier."))
  con$addTag("inputport", attrs = c(name = "globalMapName"),       paste0('"landscape"'))
  con$addTag("inputport", attrs = c(name = "mergeNonRegionCells"), ".no")
  con$addTag("inputport", attrs = c(name = "regionManager",        peerid = "v4"))
  con$addTag("outputport", attrs = c(name = "globalMap", id = "v15"))
  con$closeTag("functor")
  
  if (probability) {
    con$addTag("functor", attrs = c(name = "MergeRegionalMaps"), close = FALSE)
    con$addTag("property",  attrs = c(key = "dff.functor.alias", value = "mergeRegionalMaps3413"))
    con$addTag("property",  attrs = c(key = "dff.functor.comment",
                                      value = "Merge all maps assigned to the regions using the given identifier."))
    con$addTag("inputport", attrs = c(name = "globalMapName"),       paste0('"probabilities"'))
    con$addTag("inputport", attrs = c(name = "mergeNonRegionCells"), ".no")
    con$addTag("inputport", attrs = c(name = "regionManager",        peerid = "v4"))
    con$addTag("outputport", attrs = c(name = "globalMap", id = "v16"))
    con$closeTag("functor")
  }
  
  con$closeTag("containerfunctor")  # Group
  con$closeTag("containerfunctor")  # Repeat
  con$closeTag("containerfunctor")  # RegionManager
  
  egoml_sim_file <- paste0(output_dir, "/", egoml, ".egoml")
  saveXML(con$value(), file = egoml_sim_file)
  
  egoml_text     <- readLines(egoml_sim_file)
  egoml_text_new <- gsub(pattern = "amp;", replace = "", x = egoml_text)
  writeLines(egoml_text_new, con = egoml_sim_file)
  
  list(
    egoml_sim_file  = egoml_sim_file,
    rep             = n_rep,
    lc1             = lc1_path,
    zone            = zone_path,
    transition_mtx  = tm_path,
    dcf             = dcf_path,
    ers             = ers_path
  )
}

#' Run a DINAMICA EGO Simulation
#'
#' Wrapper around [executeDINAMICA()] that also checks that the expected
#' landscape output was produced.
#'
#' @param dinamica_path Path to the DINAMICA EGO installation.
#' @param output_dir Directory where outputs are written.
#' @param egoml Path to the `.egoml` model file.
#' @param memory_allocation Memory-allocation policy string.
#' @return Invisibly `NULL`; called for its side effects.
#' @export
run_dinamica_simulation <- function(dinamica_path = NULL, output_dir, egoml,
                                    memory_allocation) {
  params <- list(
    dinamica_path = dinamica_path,
    output_dir    = output_dir,
    egoml         = egoml
  )
  
  executeDINAMICA(params, memory_allocation)
  
  new_lc_file <- paste0(output_dir, "/landscape01.tif")
  if (!file.exists(new_lc_file)) {
    stop("Land use change simulation failed! Check DINAMICA EGO log.")
  }
}

#' Run the Full SCIENDO Simulation Pipeline
#'
#' Orchestrates a SCIENDO simulation: converts scenario matrices if needed,
#' builds the `.egoml` model, runs DINAMICA EGO, renames the output
#' landscapes, and generates the HTML report.
#'
#' @param lc_t1_path Path to the initial land cover raster.
#' @param initial_year Base year of the simulation.
#' @param period_value Number of years per simulation step.
#' @param lc_lookup_table_path Path to the land cover lookup table.
#' @param lc_lookup_table Data frame with land cover IDs and names.
#' @param zone_lookup_table Planning unit lookup table.
#' @param zone_path Path to the planning unit raster.
#' @param ers_path Path to the static variables raster.
#' @param n_rep Number of simulation repetitions.
#' @param tm_path Folder with transition matrix files.
#' @param dcf_path Folder with WoE `.dcf` files.
#' @param dinamica_path Path to the DINAMICA EGO installation.
#' @param output_dir Output directory.
#' @param memory_allocation Memory-allocation policy string.
#' @param alloc_params Optional list of `AllocateTransitions` parameters.
#' @param alloc_override_df Optional per-transition override data frame.
#' @param progress_callback Optional `function(progress, message)`.
#' @return A list summarising the run (start/end time, inputs, session log).
#' @export
run_sciendo_simulate_process <- function(lc_t1_path, initial_year, period_value,
                                         lc_lookup_table_path, lc_lookup_table,
                                         zone_lookup_table, zone_path, ers_path,
                                         n_rep, tm_path, dcf_path,
                                         dinamica_path = NULL, output_dir,
                                         memory_allocation,
                                         alloc_params = NULL,
                                         alloc_override_df = NULL,
                                         progress_callback = NULL) {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (file.exists(tm_path) && !dir.exists(tm_path)) {
    if (grepl("\\.xlsm$", tm_path, ignore.case = TRUE) && !grepl("^~\\$", basename(tm_path))) {
      xlsm_files <- tm_path
    } else {
      xlsm_files <- character(0)
    }
  } else {
    files      <- list.files(path = tm_path, full.names = TRUE, ignore.case = TRUE)
    xlsm_files <- files[grep("\\.xlsm$", files, ignore.case = TRUE)]
    xlsm_files <- xlsm_files[!grepl("^~\\$", basename(xlsm_files))]
  }
  
  if (length(xlsm_files) > 0) {
    matrix_to_tpm(tm_path, lc_lookup_table, output_dir)
    tm_path <- file.path(output_dir, "scenario_tpm")
  } else {
    message("No .xlsm files found. Using non-macro TPM")
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(0.3, "generate egoml: initialize simulation per region parameters")
  }
  
  out_sim <- generate_egoml_simulate(
    lc1_path          = lc_t1_path,
    lusim_lc          = lc_lookup_table,
    zone_path         = zone_path,
    ers_path          = ers_path,
    n_rep             = n_rep,
    tm_path           = tm_path,
    dcf_path          = dcf_path,
    output_dir        = output_dir,
    probability       = FALSE,
    egoml             = "03_sciendo_simulation",
    memory_allocation = memory_allocation,
    percent           = if (!is.null(alloc_params)) alloc_params$percent  else NULL,
    exp_mean          = if (!is.null(alloc_params)) alloc_params$exp_mean else NULL,
    exp_var           = if (!is.null(alloc_params)) alloc_params$exp_var  else NULL,
    exp_iso           = if (!is.null(alloc_params)) alloc_params$exp_iso  else NULL,
    gen_mean          = if (!is.null(alloc_params)) alloc_params$gen_mean else NULL,
    gen_var           = if (!is.null(alloc_params)) alloc_params$gen_var  else NULL,
    gen_iso           = if (!is.null(alloc_params)) alloc_params$gen_iso  else NULL,
    override_df       = alloc_override_df
  )
  
  if (!is.null(progress_callback)) progress_callback(0.7, "run dinamica simulation per region")
  run_dinamica_simulation(dinamica_path, output_dir, out_sim$egoml_sim_file, memory_allocation)
  
  rename_landscape(output_dir, initial_year, period_value)
  
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  session_log <- format_session_info_table()
  
  out <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time   = as.character(format(end_time,   "%Y-%m-%d %H:%M:%S")),
    inputs     = list(
      lc_t1_path           = lc_t1_path,
      initial_year         = initial_year,
      lc_lookup_table_path = lc_lookup_table_path,
      zone_path            = zone_path,
      zone_lookup_table    = zone_lookup_table,
      ers_path             = ers_path,
      tm_path              = tm_path,
      dcf_path             = dcf_path,
      rep                  = n_rep,
      output_dir           = output_dir
    ),
    session_log = session_log
  )
  
  if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")
  if (!is.null(progress_callback)) progress_callback(1, "generate report")
  
  generate_sciendo_simulate_report(output = out, dir = output_dir)
  out
}

# ------------------------------------------------------- report generation ---

#' Render the SCIENDO Simulation Report
#'
#' Renders the SCIENDO simulation R Markdown template to HTML.
#'
#' @param output List produced by `run_sciendo_simulate_process`.
#' @param dir Output directory for the rendered HTML.
#' @return Invisibly `NULL`; called for its side effects.
#' @importFrom rmarkdown render
#' @export
generate_sciendo_simulate_report <- function(output, dir) {
  report_params <- list(
    start_time  = output$start_time,
    end_time    = output$end_time,
    inputs      = output$inputs,
    session_log = output$session_log
  )
  template_candidates <- c(
    "../report_template/sciendo_simulate_report_template_INA.Rmd",
    "11_sciendo-simulate/report_template/sciendo_simulate_report_template_INA.Rmd",
    file.path(getwd(), "11_sciendo-simulate/report_template/sciendo_simulate_report_template_INA.Rmd"),
    system.file("report_template", "sciendo_simulate_report_template_INA.Rmd", package = "LUMENSR")
  )
  template_path <- NULL
  for (cand in template_candidates) {
    if (nzchar(cand) && file.exists(cand)) {
      template_path <- cand
      break
    }
  }
  if (is.null(template_path)) {
    template_path <- "../report_template/sciendo_simulate_report_template_INA.Rmd"
  }

  output_file <- paste0("sciendo_simulate_report_", Sys.Date(), ".html")

  rmarkdown::render(
    template_path,
    output_file = output_file,
    output_dir  = dir,
    params      = report_params
  )
}

# ------------------------------------------------------------ report utils ---

#' Interactive Stacked Area Chart of Land Use Change
#'
#' Builds a Plotly stacked area chart from wide-format land use data.
#'
#' @param luc_data_wide Data frame in wide format.
#' @param class_col Land cover class column name (defaults to column 2).
#' @param id_col Row identifier column name (defaults to column 1).
#' @param chart_title Chart title.
#' @param x_axis_label X-axis label.
#' @param y_axis_label Y-axis label.
#' @return A `plotly` object.
#' @importFrom dplyr mutate across all_of
#' @importFrom tidyr pivot_longer
#' @importFrom rlang sym :=
#' @importFrom readr parse_number
#' @importFrom ggthemes tableau_color_pal
#' @importFrom plotly plot_ly layout
#' @export
plot_interactive_stacked_area <- function(luc_data_wide,
                                          class_col = if ("Landcover" %in% names(luc_data_wide)) "Landcover" else if ("LC" %in% names(luc_data_wide)) "LC" else "LC",
                                          id_col    = if ("ID" %in% names(luc_data_wide)) "ID" else "ID",
                                          cat_table = NULL,
                                          chart_title  = "Land Use Change",
                                          x_axis_label = "Year / Time Step",
                                          y_axis_label = "Proportion (%)") {
  required_cols <- c(class_col, id_col)
  if (!all(required_cols %in% names(luc_data_wide))) {
    stop(paste("The provided data frame must contain the columns:",
               paste(required_cols, collapse = ", ")))
  }
  
  clean_data_wide <- luc_data_wide %>%
    dplyr::mutate(
      dplyr::across(
        .cols = -dplyr::all_of(required_cols),
        .fns  = ~ if (is.numeric(.x)) .x else readr::parse_number(as.character(.x))
      )
    )
  
  clean_data_wide[[class_col]] <- factor(
    clean_data_wide[[class_col]],
    levels = unique(clean_data_wide[[class_col]])
  )
  
  luc_data_long <- clean_data_wide %>%
    tidyr::pivot_longer(
      cols      = -dplyr::all_of(required_cols),
      names_to  = "Year",
      values_to = "Area"
    ) %>%
    dplyr::mutate(
      Year_Clean = sub("^X(?=[0-9])", "", Year, perl = TRUE),
      Year = factor(Year_Clean, levels = unique(Year_Clean))
    ) %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(
      Total_Area = sum(Area, na.rm = TRUE),
      Percentage = ifelse(Total_Area > 0, (Area / Total_Area) * 100, 0)
    ) %>%
    dplyr::ungroup()
  
  unique_classes <- levels(clean_data_wide[[class_col]])
  n_colors       <- length(unique_classes)
  
  predefined_50_colors <- c(
    "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F", "#8CD17D", "#B6992D", "#F1CE63",
    "#499894", "#86BCB6", "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295", "#FABFD2",
    "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6", "#E6194B", "#3CB44B", "#FFE119", "#4363D8",
    "#F58231", "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE", "#008080", "#E6BEFF",
    "#9A6324", "#FFFAC8", "#800000", "#AAFFC3", "#808000", "#FFD8B1", "#000075", "#A9A9A9",
    "#231F20", "#1170AA", "#A2C8EC", "#FFBC79", "#C85200", "#FF9DA7", "#D55E00", "#59A14F",
    "#EDC948", "#66A61E"
  )
  
  if (!is.null(cat_table) && "color_palette" %in% names(cat_table) && ncol(cat_table) >= 2) {
    category_col_name <- names(cat_table)[2]
    matched_colors <- cat_table$color_palette[match(unique_classes, cat_table[[category_col_name]])]
    missing_idx <- which(is.na(matched_colors) | matched_colors == "")
    if (length(missing_idx) > 0) {
      fallback_cols <- if (length(missing_idx) <= length(predefined_50_colors)) {
        predefined_50_colors[seq_along(missing_idx)]
      } else {
        grDevices::colorRampPalette(predefined_50_colors)(length(missing_idx))
      }
      matched_colors[missing_idx] <- fallback_cols
    }
    selected_palette <- matched_colors
  } else {
    selected_palette <- if (n_colors <= length(predefined_50_colors)) {
      predefined_50_colors[1:n_colors]
    } else {
      grDevices::colorRampPalette(predefined_50_colors)(n_colors)
    }
  }
  
  color_formula <- as.formula(paste0("~`", class_col, "`"))
  text_formula  <- as.formula(
    paste0(
      "~paste0('<b>', `", class_col, "`, '</b><br>', ",
      "'Tahun / Step: ', Year, '<br>', ",
      "'Proporsi: ', sprintf('%.2f', Percentage), '%<br>', ",
      "'Luas: ', scales::comma(round(Area, 2)), ' ha')"
    )
  )
  
  plotly::plot_ly(
    data       = luc_data_long,
    x          = ~Year,
    y          = ~Percentage,
    color      = color_formula,
    colors     = selected_palette,
    type       = "scatter",
    mode       = "lines",
    stackgroup = "one",
    line       = list(width = 0),
    hoverinfo  = "text",
    text       = text_formula
  ) %>%
    plotly::layout(
      title = chart_title,
      xaxis = list(
        title = x_axis_label,
        type = "category",
        categoryorder = "array",
        categoryarray = levels(luc_data_long$Year),
        tickmode = "array",
        tickvals = levels(luc_data_long$Year)
      ),
      yaxis = list(title = y_axis_label, range = c(0, 100)),
      legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center")
    )
}

# ------------------------------------------------------------ raster utils ---

#' Calculate Resolution Conversion Factor to Hectares
#'
#' Computes the hectare conversion factor of a raster cell based on its CRS
#' (metre or degree units).
#'
#' @param raster_input A `SpatRaster`.
#' @return Numeric hectare conversion factor.
#' @importFrom terra crs res
#' @export
calc_res_conv_factor_to_ha <- function(raster_input) {
  crs <- terra::crs(raster_input, proj = TRUE)
  
  if (grepl("+units=m", crs)) {
    message("Raster map has a projection in metre unit.")
    conversion_factor <- terra::res(raster_input)[1] * terra::res(raster_input)[2] / 10000
    message(paste("Raster map has ", conversion_factor,
                  " Ha spatial resolution. Pre-QuES will automatically generate data in Ha unit."))
  } else if (grepl("+proj=longlat", crs)) {
    message("Raster map has a projection in degree unit.")
    conversion_factor <- terra::res(raster_input)[1] * terra::res(raster_input)[2] *
      (111319.9 ^ 2) / 10000
    message(paste("Raster map has ", conversion_factor,
                  " Ha spatial resolution. Pre-QuES will automatically generate data in Ha unit."))
  } else {
    stop("Projection of the raster map is unknown")
  }
  
  conversion_factor
}

#' Calculate Land Cover Frequency for Multiple Rasters
#'
#' Returns a frequency table for each raster layer, sorted by the count of
#' the last layer in descending order.
#'
#' @param raster_list A list of `SpatRaster` objects, or a single one.
#' @return A data frame of frequencies, or a message string if time
#'   attributes are missing.
#' @importFrom terra compareGeom freq levels time
#' @importFrom dplyr left_join select arrange desc rename
#' @export
calc_lc_freq <- function(raster_list) {
  if (class(raster_list)[1] == "SpatRaster") {
    raster_list <- list(raster_list)
  } else if (!is.list(raster_list)) {
    stop("Input must be a list of raster layers or a single raster layer")
  }
  
  if (length(raster_list) > 1) {
    for (i in 2:length(raster_list)) {
      if (!terra::compareGeom(raster_list[[1]], raster_list[[i]])) {
        stop("All rasters must have the same extent and projection system")
      }
    }
  }
  
  freq_tables <- list()
  for (i in 1:length(raster_list)) {
    if (is.null(terra::levels(raster_list[[i]]))) {
      warning(paste0("Raster ", i, " has no attributes"))
    }
    freq <- terra::freq(raster_list[[i]])
    names(freq)[names(freq) == "count"] <- paste0(names(raster_list[[i]]), "_count")
    freq_tables[[i]] <- freq
  }
  
  freq_df <- freq_tables[[1]]
  if (length(freq_tables) > 1) {
    for (i in 2:length(freq_tables)) {
      freq_df <- dplyr::left_join(freq_df, freq_tables[[i]], by = c("layer", "value"))
    }
    freq_df <- dplyr::select(freq_df, -layer)
  }
  
  freq_df <- dplyr::arrange(freq_df, dplyr::desc(freq_df[[ncol(freq_df)]]))
  freq_df <- dplyr::rename(freq_df, `Land-use/cover types` = value)
  
  all_times_present <- all(sapply(raster_list, function(x) !is.null(time(x))))
  if (all_times_present) {
    for (i in seq_along(raster_list)) {
      time_i <- as.character(time(raster_list[[i]]))
      names(freq_df)[i + 1] <- time_i
    }
    return(freq_df)
  } else {
    return("Not all SpatRaster objects in the list have a time attribute")
  }
}

#' Add a Legend to a Categorical Raster
#'
#' Attaches a lookup table as the category legend of a `SpatRaster`.
#'
#' @param raster_file A `SpatRaster`.
#' @param lookup_table Data frame whose first column holds numeric class IDs
#'   and second column the descriptions.
#' @param year Optional 4-digit year to set as the raster's time attribute.
#' @return The modified `SpatRaster`.
#' @importFrom terra levels freq time names
#' @importFrom stats setNames
#' @export
add_legend_to_categorical_raster <- function(raster_file, lookup_table, year = NULL) {
  if (!inherits(raster_file, "SpatRaster")) {
    stop("raster_file should be a SpatRaster object")
  }
  if (!is.data.frame(lookup_table)) {
    stop("lookup_table should be a data frame")
  }
  
  first_column <- lookup_table[[1]]
  if (!is.numeric(first_column) && any(is.na(as.numeric(first_column)))) {
    stop("The first column of lookup_table should be numeric or convertible to numeric")
  }
  
  if (!is.null(year) && (!is.numeric(year) || nchar(as.character(year)) != 4)) {
    stop("year should be a numeric value consisting of 4 digits")
  }
  
  if (isTRUE(terra::is.factor(raster_file)[[1]])) {
    cur_levels <- tryCatch(terra::levels(raster_file)[[1]], error = function(e) NULL)
    if (!is.null(cur_levels) && is.data.frame(cur_levels) && nrow(cur_levels) > 0) {
      if (!is.null(year)) {
        terra::time(raster_file, tstep = "years") <- year
      }
      return(raster_file)
    }
  }
  
  raw_vals <- tryCatch({
    unique(as.numeric(values(raster_file, na.rm = TRUE)))
  }, error = function(e) {
    as.numeric(terra::freq(raster_file)[["value"]])
  })
  
  lookup_table <- lookup_table[lookup_table[[1]] %in% raw_vals, ]
  lookup_table <- data.frame(lookup_table)
  
  if (!is.numeric(first_column)) {
    lookup_table[[1]] <- as.numeric(first_column)
  }
  
  name_rast <- names(raster_file)
  levels(raster_file) <- lookup_table
  raster_file <- setNames(raster_file, name_rast)
  
  if (!is.null(year)) {
    terra::time(raster_file, tstep = "years") <- year
  }
  
  raster_file
}

#' Combine Land Cover Frequencies Across Years and Planning Units
#'
#' Reads all `landscapeYYYY.tif` files in `lc_dir` and returns either a
#' landscape-wide frequency table or a per-planning-unit table.
#'
#' @param lc_dir Directory containing `landscapeYYYY.tif` files.
#' @param df_lc Land cover lookup table.
#' @param PU `"YES"` to compute per-planning-unit frequencies, otherwise
#'   `"NO"`.
#' @param zone `SpatRaster` of planning unit IDs (required when `PU = "YES"`).
#' @param split `"YES"` to return a list split by planning unit.
#' @return A tibble (or list of tibbles when `split = "YES"`).
#' @importFrom terra rast
#' @importFrom dplyr arrange mutate across select everything
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom stringr str_extract
#' @importFrom sf st_crs
#' @export
multiple_lc_freq_combined <- function(lc_dir, df_lc, PU = "NO", zone = NULL,
                                      split = "NO", initial_raster = NULL, initial_year = NULL) {
  list_luc <- lc_dir %>%
    list.files(full.names = TRUE, pattern = "^landscape\\d{4}\\.tif$")
  
  list_luc <- list_luc[order(as.numeric(stringr::str_extract(basename(list_luc), "\\d{4}")))]
  
  if (length(list_luc) == 0) {
    stop("No landscape*.tif files found in ", lc_dir)
  }
  
  ref_raster <- terra::rast(list_luc[1])
  rst_list   <- list()
  years      <- character(0)
  init_yr_str <- NULL
  
  if (!is.null(initial_raster)) {
    r_init <- if (inherits(initial_raster, "SpatRaster")) {
      initial_raster
    } else if (inherits(initial_raster, "RasterLayer")) {
      terra::rast(initial_raster)
    } else if (is.character(initial_raster) && file.exists(initial_raster)) {
      terra::rast(initial_raster)
    } else {
      NULL
    }
    
    if (!is.null(r_init)) {
      geom_matches <- tryCatch({
        terra::compareGeom(r_init, ref_raster, stopOnError = FALSE)
      }, error = function(e) FALSE)
      
      if (!isTRUE(geom_matches)) {
        r_init <- terra::resample(r_init, ref_raster, method = "near")
      }
      
      r_init <- add_legend_to_categorical_raster(r_init, lookup_table = df_lc)
      init_yr_str <- if (!is.null(initial_year) && !is.na(initial_year) && nzchar(as.character(initial_year))) {
        as.character(initial_year)
      } else {
        detected_yr <- tryCatch({
          src <- if (inherits(r_init, "SpatRaster")) terra::sources(r_init) else ""
          if (is.character(initial_raster)) src <- initial_raster
          y <- stringr::str_extract(basename(src), "\\d{4}")
          if (!is.na(y)) y else NULL
        }, error = function(e) NULL)
        if (!is.null(detected_yr)) detected_yr else "Initial"
      }
      names(r_init) <- paste0("landscape_", init_yr_str)
      rst_list[[1]] <- r_init
      years <- c(years, init_yr_str)
    }
  }
  
  for (i in seq_along(list_luc)) {
    r <- list_luc[i] %>%
      rast()
    
    geom_matches <- tryCatch({
      terra::compareGeom(r, ref_raster, stopOnError = FALSE)
    }, error = function(e) FALSE)
    
    if (!isTRUE(geom_matches)) {
      r <- terra::resample(r, ref_raster, method = "near")
    }
    
    r <- add_legend_to_categorical_raster(r, lookup_table = df_lc)
    yr_i <- stringr::str_extract(basename(list_luc[i]), "\\d{4}")
    names(r) <- paste0("landscape_", yr_i)
    rst_list[[length(rst_list) + 1]] <- r
    years <- c(years, yr_i)
  }
  
  # Compute chronological column order: initial year first, followed by projected years
  proj_year_cols <- if (!is.null(init_yr_str)) years[years != init_yr_str] else years
  proj_year_cols <- proj_year_cols[order(as.numeric(proj_year_cols))]
  year_cols_ordered <- if (!is.null(init_yr_str)) c(init_yr_str, proj_year_cols) else proj_year_cols
  
  if (toupper(PU) == "YES") {
    if (is.null(zone)) stop("Zone parameter must be provided when PU = YES")
    
    zone_r <- if (inherits(zone, "SpatRaster")) {
      zone
    } else if (inherits(zone, "RasterLayer")) {
      terra::rast(zone)
    } else if (is.character(zone) && file.exists(zone)) {
      terra::rast(zone)
    } else {
      zone
    }
    
    zone_matches <- tryCatch({
      terra::compareGeom(zone_r, ref_raster, stopOnError = FALSE)
    }, error = function(e) FALSE)
    
    if (!isTRUE(zone_matches)) {
      has_levels <- isTRUE(terra::is.factor(zone_r)[[1]])
      z_levels <- if (has_levels) tryCatch(terra::levels(zone_r)[[1]], error = function(e) NULL) else NULL
      zone_r <- terra::resample(zone_r, ref_raster, method = "near")
      if (!is.null(z_levels) && is.data.frame(z_levels) && nrow(z_levels) > 0) {
        levels(zone_r) <- z_levels
      }
    }
    zone <- zone_r
    
    year_data_list <- list()
    for (i in seq_along(rst_list)) {
      year       <- years[i]
      raster_obj <- rst_list[[i]]
      
      freq_table <- terra::crosstab(c(raster_obj, zone))
      freq_df    <- as.data.frame(freq_table)
      colnames(freq_df) <- c("landcover", "PU", "value")
      freq_df$year <- year
      
      year_data_list[[as.character(year)]] <- freq_df
    }
    
    combined_data <- do.call(rbind, year_data_list)
    
    final_data <- tidyr::pivot_wider(combined_data,
                                     names_from   = year,
                                     values_from  = value,
                                     values_fill  = 0) %>%
      dplyr::arrange(PU, landcover) %>%
      dplyr::select(PU, landcover, dplyr::all_of(year_cols_ordered))
    
    spatRes <- tryCatch({
      calc_res_conv_factor_to_ha(rst_list[[1]])
    }, error = function(e) {
      1
    })
    final_data <- dplyr::mutate(final_data, dplyr::across(c(3:ncol(final_data)), ~ (spatRes * .x)))
    
    if (toupper(split) == "YES") {
      pu_list <- final_data %>%
        dplyr::group_split(PU) %>%
        purrr::map(~ dplyr::select(., -PU))
      pu_names <- unique(final_data$PU)
      names(pu_list) <- paste0("PU_", pu_names)
      return(pu_list)
    }
    return(final_data)
  }
  
  freq_data <- calc_lc_freq(raster_list = rst_list)
  
  if (ncol(freq_data) == length(years) + 1) {
    colnames(freq_data) <- c("Landcover", years)
  }
  
  spatRes <- tryCatch({
    calc_res_conv_factor_to_ha(rst_list[[1]])
  }, error = function(e) {
    1
  })
  
  freq_tbl <- freq_data %>%
    tibble::as_tibble() %>%
    dplyr::select(Landcover, dplyr::all_of(year_cols_ordered)) %>%
    dplyr::arrange(Landcover) %>%
    dplyr::mutate(dplyr::across(-Landcover, ~ .x * spatRes))
  
  freq_tbl
}

# --------------------------------------------------------- string helpers ---

#' Abbreviate Column Values
#'
#' Shortens character column values by replacing spaces with underscores,
#' trimming after the first slash, and (optionally) dropping vowels after
#' the first letter of each word.
#'
#' @param df Data frame.
#' @param col_names Columns to abbreviate; defaults to the first character
#'   column.
#' @param remove_vowels Logical. Drop vowels after the first character.
#' @return The data frame with abbreviated columns.
#' @importFrom textclean replace_non_ascii
#' @export
abbreviate_by_column <- function(df, col_names = NULL, remove_vowels = FALSE) {
  if (!is.data.frame(df)) stop("df must be a data frame")
  if (ncol(df) < 1)       stop("df must have at least one column")
  
  if (is.null(col_names)) {
    col_names <- names(df)[which(sapply(df, is.character))[1]]
  }
  if (!all(col_names %in% names(df))) {
    stop("Some column names provided are not columns in df")
  }
  
  abbreviate_string <- function(input_string, drop_vowels = remove_vowels) {
    string <- textclean::replace_non_ascii(input_string)
    string <- strsplit(string, " / ")[[1]][1]
    
    if (isTRUE(drop_vowels)) {
      string <- gsub(" ", "_", string)
      words  <- strsplit(string, "_")[[1]]
      words  <- sapply(words, function(word) {
        ifelse(grepl("^[aeiouAEIOU]", word),
               paste0(substr(word, 1, 1),
                      gsub("[aeiouAEIOU]", "", substr(word, 2, nchar(word)))),
               gsub("[aeiouAEIOU]", "", word))
      })
      string <- paste(words, collapse = "_")
    }
    string
  }
  
  for (col_name in col_names) {
    df[[col_name]] <- unlist(lapply(df[[col_name]], abbreviate_string))
  }
  df
}

# -------------------------------------------------------- transition matrix ---

#' Convert Macro TPM Files to Long CSV
#'
#' Reads `.xlsm` transition probability matrices, converts land cover names
#' to IDs, and writes each as a long-format CSV under
#' `output_dir/scenario_tpm/`.
#'
#' @param input_folder_path Folder with `.xlsm` files.
#' @param lc_lookup Lookup table with columns `ID` and `LC`.
#' @param output_dir Directory where `scenario_tpm/` is created.
#' @return Invisibly, a character vector of written file paths.
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate rename filter
#' @importFrom tidyr pivot_longer
#' @export
matrix_to_tpm <- function(input_folder_path, lc_lookup, output_dir) {
  tpm_dir <- file.path(output_dir, "scenario_tpm")
  dir.create(tpm_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (file.exists(input_folder_path) && !dir.exists(input_folder_path)) {
    xlsm_files <- input_folder_path
  } else {
    xlsm_files <- list.files(path = input_folder_path, pattern = "\\.xlsm$",
                             full.names = TRUE, ignore.case = TRUE)
  }
  xlsm_files <- xlsm_files[!grepl("^~\\$", basename(xlsm_files))]
  
  if (length(xlsm_files) == 0) {
    message("No .xlsm files found in: ", input_folder_path)
    return(invisible(NULL))
  }
  
  clean_text <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))
  lc_lookup_clean <- lc_lookup %>% dplyr::mutate(LC_clean = clean_text(LC))
  
  convert_to_id <- function(data, lc_lookup_clean) {
    row_raw <- data[[1]]
    col_raw <- colnames(data)[-1]
    
    if (all(!is.na(suppressWarnings(as.numeric(row_raw))))) {
      row_ids <- as.numeric(row_raw)
    } else {
      row_clean <- clean_text(row_raw)
      row_ids   <- lc_lookup_clean$ID[match(row_clean, lc_lookup_clean$LC_clean)]
      na_idx    <- which(is.na(row_ids))
      if (length(na_idx) > 0) {
        fallback_ids <- suppressWarnings(as.numeric(gsub(".*?([0-9]+).*", "\\1", row_clean[na_idx])))
        row_ids[na_idx] <- ifelse(!is.na(fallback_ids), fallback_ids, row_ids[na_idx])
      }
    }
    
    if (all(!is.na(suppressWarnings(as.numeric(col_raw))))) {
      col_ids <- as.numeric(col_raw)
    } else {
      col_clean <- clean_text(col_raw)
      col_ids   <- lc_lookup_clean$ID[match(col_clean, lc_lookup_clean$LC_clean)]
      na_idx    <- which(is.na(col_ids))
      if (length(na_idx) > 0) {
        fallback_ids <- suppressWarnings(as.numeric(gsub(".*?([0-9]+).*", "\\1", col_clean[na_idx])))
        col_ids[na_idx] <- ifelse(!is.na(fallback_ids), fallback_ids, col_ids[na_idx])
      }
    }
    
    list(row_ids = row_ids, col_ids = col_ids)
  }
  
  processed_files <- character(0)
  
  for (xlsm_path in xlsm_files) {
    message("\nProcessing: ", basename(xlsm_path))
    
    all_sheets <- readxl::excel_sheets(xlsm_path)
    is_consolidated <- any(grepl("^dashboard$", all_sheets, ignore.case = TRUE)) ||
                       sum(grepl("^PU_", all_sheets, ignore.case = TRUE)) > 1
    
    if (is_consolidated) {
      pu_sheets <- all_sheets[
        !grepl("^dashboard$", all_sheets, ignore.case = TRUE) &
        !grepl("^_", all_sheets) &
        !grepl("_original$", all_sheets, ignore.case = TRUE) &
        !grepl("_backup$", all_sheets, ignore.case = TRUE) &
        !grepl("_baseline$", all_sheets, ignore.case = TRUE)
      ]
      
      message("Found consolidated workbook with ", length(pu_sheets), " Planning Unit sheets.")
      
      for (sh_idx in seq_along(pu_sheets)) {
        sh_name <- pu_sheets[sh_idx]
        tryCatch({
          pu_id_match <- regmatches(sh_name, regexec("^PU_?([0-9]+)", sh_name, ignore.case = TRUE))[[1]]
          if (length(pu_id_match) >= 2) {
            pu_id <- as.numeric(pu_id_match[2])
          } else {
            num_match <- regmatches(sh_name, regexec("([0-9]+)", sh_name))[[1]]
            if (length(num_match) >= 2) {
              pu_id <- as.numeric(num_match[2])
            } else {
              pu_id <- sh_idx
            }
          }
          
          raw_data <- readxl::read_excel(xlsm_path, sheet = sh_name, col_names = FALSE) %>% as.data.frame()
          if (nrow(raw_data) == 0) next
          
          header_row_idx <- which(apply(raw_data, 1, function(r) any(grepl("From", r, ignore.case = TRUE))))
          if (length(header_row_idx) == 0) header_row_idx <- 1 else header_row_idx <- header_row_idx[1]
          
          header_row_vals <- as.character(raw_data[header_row_idx, ])
          start_col_idx <- which(grepl("From", header_row_vals, ignore.case = TRUE))
          if (length(start_col_idx) == 0) start_col_idx <- 1 else start_col_idx <- start_col_idx[1]
          
          gt_col_idx <- which(grepl("grand total", header_row_vals, ignore.case = TRUE))
          if (length(gt_col_idx) == 0) {
            non_empty <- which(!is.na(header_row_vals) & header_row_vals != "")
            end_col_idx <- max(non_empty)
          } else {
            end_col_idx <- gt_col_idx[1] - 1
          }
          
          col_names_vec <- as.character(raw_data[header_row_idx, (start_col_idx + 1):end_col_idx])
          
          row_scan <- (header_row_idx + 1):nrow(raw_data)
          valid_rows <- integer(0)
          for (ri in row_scan) {
            val <- raw_data[ri, start_col_idx]
            if (!is.na(val) && as.character(val) != "" && !grepl("grand total", as.character(val), ignore.case = TRUE)) {
              valid_rows <- c(valid_rows, ri)
            } else {
              break
            }
          }
          
          if (length(valid_rows) == 0) next
          
          row_names_vec <- as.character(raw_data[valid_rows, start_col_idx])
          matrix_vals <- raw_data[valid_rows, (start_col_idx + 1):end_col_idx, drop = FALSE]
          
          data <- as.data.frame(matrix_vals)
          colnames(data) <- col_names_vec
          data <- cbind(From = row_names_vec, data)
          
          ids <- convert_to_id(data, lc_lookup_clean)
          data[[1]] <- ids$row_ids
          colnames(data)[-1] <- ids$col_ids
          
          parse_num <- function(x) suppressWarnings(as.numeric(trimws(gsub(",", ".", as.character(x)))))
          
          long_data <- data %>%
            tidyr::pivot_longer(
              cols             = -1,
              names_to         = "To*",
              values_to        = "Rate",
              values_transform = list(Rate = parse_num)
            ) %>%
            dplyr::rename("From*" = 1) %>%
            dplyr::mutate(
              `From*` = as.numeric(`From*`),
              `To*`   = as.numeric(`To*`)
            ) %>%
            dplyr::filter(
              `From*` != `To*`,
              !is.na(Rate),
              Rate != 0,
              !is.na(`From*`),
              !is.na(`To*`)
            )
          
          out_file <- file.path(tpm_dir, sprintf("single_step%06d.csv", pu_id))
          write.csv(long_data, out_file, row.names = FALSE, quote = FALSE)
          processed_files <- c(processed_files, out_file)
          message("  Extracted: ", sh_name, " -> ", basename(out_file))
        }, error = function(e) {
          message("Error processing sheet ", sh_name, " in ", basename(xlsm_path), ": ", e$message)
        })
      }
    } else {
      # Legacy individual .xlsm file processing
      tryCatch({
        data <- readxl::read_excel(xlsm_path, sheet = 1, col_names = TRUE) %>% as.data.frame()
        if (nrow(data) == 0) {
          message("Empty dataset in file: ", basename(xlsm_path))
          next
        }
        
        ids <- convert_to_id(data, lc_lookup_clean)
        data[[1]] <- ids$row_ids
        colnames(data)[-1] <- ids$col_ids
        data <- data[, -ncol(data), drop = FALSE]
        
        long_data <- data %>%
          tidyr::pivot_longer(
            cols             = -1,
            names_to         = "To*",
            values_to        = "Rate",
            values_transform = list(Rate = function(x) suppressWarnings(as.numeric(trimws(gsub(",", ".", as.character(x))))))
          ) %>%
          dplyr::rename("From*" = 1) %>%
          dplyr::mutate(
            `From*` = as.numeric(`From*`),
            `To*`   = as.numeric(`To*`)
          ) %>%
          dplyr::filter(
            `From*` != `To*`,
            !is.na(Rate),
            Rate != 0,
            !is.na(`From*`),
            !is.na(`To*`)
          )
        
        clean_name <- gsub("_macros", "", basename(xlsm_path))
        out_file   <- file.path(tpm_dir, sub("\\.xlsm$", ".csv", clean_name))
        write.csv(long_data, out_file, row.names = FALSE, quote = FALSE)
        processed_files <- c(processed_files, out_file)
      }, error = function(e) {
        message("Error processing ", basename(xlsm_path), ": ", e$message)
      })
    }
  }
  
  invisible(processed_files)
}

# ------------------------------------------------------------- landscape io ---

#' Rename Landscape Rasters to Year Labels
#'
#' Renames `landscapeNN.tif` files to `landscapeYYYY.tif` using
#' `initial_year + file_number * period_value`, updates the layer name, and
#' removes stray `.tif` / `.xml` files that don't match the expected pattern.
#'
#' @param folder_path Directory containing the raster files.
#' @param initial_year Base year.
#' @param period_value Number of years per simulation step.
#' @return Invisibly `NULL`; called for its side effects.
#' @importFrom terra rast writeRaster
#' @importFrom stringr str_extract
#' @export
rename_landscape <- function(folder_path, initial_year, period_value) {
  all_tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(all_tif_files) == 0) {
    stop("No .tif files found in the specified directory.")
  }
  
  landscape_files <- all_tif_files[grepl("^landscape\\d{2}\\.tif$", basename(all_tif_files))]
  
  xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)
  if (length(xml_files) > 0) {
    cat("Removing .xml files:\n")
    for (file in xml_files) {
      file.remove(file)
      cat(sprintf("  Removed: %s\n", basename(file)))
    }
    cat(sprintf("Removed %d .xml file(s).\n\n", length(xml_files)))
  } else {
    cat("No .xml files found to remove.\n\n")
  }
  
  non_landscape_files <- all_tif_files[!grepl("^landscape\\d{2}\\.tif$", basename(all_tif_files))]
  if (length(non_landscape_files) > 0) {
    cat("Removing files that don't match landscape pattern:\n")
    for (file in non_landscape_files) {
      file.remove(file)
      cat(sprintf("  Removed: %s\n", basename(file)))
    }
    cat(sprintf("Removed %d non-matching file(s).\n\n", length(non_landscape_files)))
  }
  
  if (length(landscape_files) == 0) {
    stop("No landscape raster files found matching the pattern 'landscapeXX.tif'.")
  }
  
  file_numbers    <- as.numeric(stringr::str_extract(basename(landscape_files), "\\d+"))
  sorted_indices  <- order(file_numbers)
  landscape_files <- landscape_files[sorted_indices]
  file_numbers    <- file_numbers[sorted_indices]
  
  for (i in seq_along(landscape_files)) {
    file_path   <- landscape_files[i]
    file_number <- file_numbers[i]
    
    target_year   <- initial_year + (file_number * period_value)
    new_filename  <- paste0("landscape", target_year, ".tif")
    new_file_path <- file.path(folder_path, new_filename)
    
    raster_obj <- terra::rast(file_path)
    names(raster_obj) <- target_year
    terra::writeRaster(raster_obj, filename = new_file_path, overwrite = TRUE)
    file.remove(file_path)
    
    cat(sprintf("Renamed: %s -> %s (layer name: %s)\n",
                basename(file_path), new_filename, names(raster_obj)))
  }
  
  cat(sprintf("\nSuccessfully renamed %d raster files.\n", length(landscape_files)))
}

# ------------------------------------------------------------ DT rendering ---

#' Render an Enhanced DataTable
#'
#' Wraps `DT::datatable` with sensible defaults: export buttons, responsive
#' layout, formatted numerics, and auto-dismissing copy notifications.
#'
#' @param data Data frame or matrix.
#' @param caption Optional table caption.
#' @param digits Decimal places for percentage columns.
#' @param area_digits Decimal places for non-percentage numeric columns.
#' @param notification_timeout Milliseconds before the copy notification
#'   auto-dismisses.
#' @return An `htmltools::tagList` containing CSS and the DataTable.
#' @importFrom DT datatable
#' @importFrom htmltools tags HTML tagList
#' @export
render_dt_table <- function(data, caption = NULL, digits = 2, area_digits = 0,
                            notification_timeout = 1000) {
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
  
  if (length(numeric_cols) > 0) {
    for (col in numeric_cols) {
      col_data <- data[[col]]
      col_name <- names(data)[col]
      is_percentage_col <- grepl("^%|Percent|Percentage|% T1|% T2", col_name,
                                 ignore.case = TRUE)
      
      if (inherits(col_data, "units")) {
        numeric_values <- as.numeric(col_data)
        units_attr     <- attributes(col_data)
        is_integer_col <- all(numeric_values == floor(numeric_values), na.rm = TRUE)
        
        formatted_values <- if (is_integer_col) {
          format(numeric_values, big.mark = ",", scientific = FALSE, trim = TRUE)
        } else {
          format(round(numeric_values, area_digits), big.mark = ",",
                 scientific = FALSE, nsmall = area_digits, trim = TRUE)
        }
        
        formatted_data[[col]] <- if (!is.null(units_attr$units)) {
          paste(formatted_values, units_attr$units)
        } else {
          formatted_values
        }
      } else {
        is_integer_col <- all(col_data == floor(col_data), na.rm = TRUE)
        if (is_percentage_col) {
          formatted_data[[col]] <- format(round(col_data, digits),
                                          nsmall = digits, trim = TRUE)
        } else if (is_integer_col) {
          formatted_data[[col]] <- format(col_data, big.mark = ",",
                                          scientific = FALSE, trim = TRUE)
        } else {
          formatted_data[[col]] <- format(round(col_data, area_digits),
                                          big.mark = ",", scientific = FALSE,
                                          nsmall = area_digits, trim = TRUE)
        }
      }
    }
  }
  
  dt <- DT::datatable(
    formatted_data,
    extensions = c("Buttons", "Responsive"),
    options = list(
      paging        = TRUE,
      searching     = TRUE,
      fixedColumns  = TRUE,
      autoWidth     = TRUE,
      ordering      = TRUE,
      dom           = "Blfrtip",
      buttons = list(
        list(extend = "copy",  className = "btn btn-light btn-sm",
             text = "Copy",  title = caption),
        list(extend = "csv",   className = "btn btn-light btn-sm",
             title = caption),
        list(extend = "excel", className = "btn btn-light btn-sm",
             title = caption)
      )
    ),
    class    = "display stripe hover",
    caption  = caption,
    rownames = FALSE
  )
  
  htmltools::tagList(css_fix, dt)
}

# ---------------------------------------------------- mapview categorical ---

#' Interactive Categorical Raster Map with mapview
#'
#' Builds an interactive mapview / leaflet map for a categorical raster,
#' using custom hex colors from `cat_table$color_palette` when available,
#' with Esri World Topo Map as the default basemap.
#'
#' @param cat_raster A categorical `SpatRaster`.
#' @param cat_table Data frame whose first column holds IDs and second column
#'   the category names. Optional `color_palette` column with hex codes.
#' @param layer_title Legend / layer title.
#' @return A `mapview` object with an added legend.
#' @importFrom terra values classify as.factor levels<- activeCat coltab ncell
#' @importFrom mapview mapview
#' @importFrom leaflet colorFactor addLegend
#' @importFrom dplyr %>%
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

# ------------------------------------------------- allocate transitions ---

#' Build a Template for Per-Transition Allocation Overrides
#'
#' Produces a data frame (optionally written to CSV) with one row per
#' `from -> to` transition, columns ready to be filled with `percent`,
#' `exp_*`, and `gen_*` values.
#'
#' @param lusim_lc Data frame of land cover IDs and names.
#' @param path Optional CSV path to write the template to.
#' @return The template data frame.
#' @export
make_alloc_trans_template <- function(lusim_lc, path = NULL) {
  classes <- data.frame(
    id   = lusim_lc[[1]],
    name = as.character(lusim_lc[[2]]),
    stringsAsFactors = FALSE
  )
  
  skel <- expand.grid(from_id = classes$id, to_id = classes$id,
                      stringsAsFactors = FALSE)
  skel <- skel[skel$from_id != skel$to_id, , drop = FALSE]
  rownames(skel) <- NULL
  
  tmpl <- skel
  tmpl$from_name <- classes$name[match(tmpl$from_id, classes$id)]
  tmpl$to_name   <- classes$name[match(tmpl$to_id,   classes$id)]
  tmpl$percent  <- NA_real_
  tmpl$exp_mean <- NA_real_; tmpl$exp_var <- NA_real_; tmpl$exp_iso <- NA_real_
  tmpl$gen_mean <- NA_real_; tmpl$gen_var <- NA_real_; tmpl$gen_iso <- NA_real_
  
  tmpl <- tmpl[, c("from_id", "from_name", "to_id", "to_name",
                   "percent",
                   "exp_mean", "exp_var", "exp_iso",
                   "gen_mean", "gen_var", "gen_iso")]
  
  if (!is.null(path)) {
    utils::write.csv(tmpl, path, row.names = FALSE, na = "")
    message("Template ditulis di: ", normalizePath(path))
  }
  tmpl
}

#' Build AllocateTransitions Parameter Strings
#'
#' Prepares the three Dinamica EGO `AllocateTransitions` input-port strings
#' (`percentOfTransitionsByExpansion`, `patchExpansionParameters`,
#' `patchGenerationParameters`).
#'
#' Supports two modes:
#' \enumerate{
#'   \item Uniform mode: pass the same value for every transition via
#'     individual arguments.
#'   \item Override mode: supply `override_df` to specify values for selected
#'     transitions (non-`NA` cells override uniform values).
#' }
#'
#' Value priority (highest to lowest): `override_df` cells > individual
#' arguments > `defaults`.
#'
#' @param lusim_lc Data frame of land cover IDs (column 1) and names (column 2).
#' @param percent,exp_mean,exp_var,exp_iso,gen_mean,gen_var,gen_iso Uniform
#'   parameter values; `NULL`/`NA` falls back to `defaults`.
#' @param override_df Optional per-transition override data frame with columns
#'   `from_id`, `to_id`, `percent`, `exp_mean`, `exp_var`, `exp_iso`,
#'   `gen_mean`, `gen_var`, `gen_iso`.
#' @param defaults Named list of fallback values.
#' @param validate Logical. Validate the resolved parameter table.
#' @return A named list of three character strings (one per input port).
#' @seealso [make_alloc_trans_template()]
#' @export
build_allocate_transitions <- function(
    lusim_lc,
    percent  = NULL, exp_mean = NULL, exp_var = NULL, exp_iso = NULL,
    gen_mean = NULL, gen_var = NULL, gen_iso = NULL,
    override_df = NULL,
    defaults = list(
      percent  = 0.5,
      exp_mean = 2, exp_var = 1, exp_iso = 1,
      gen_mean = 1, gen_var = 1, gen_iso = 1
    ),
    validate = TRUE
) {
  .fmt_num <- function(x)
    ifelse(x == as.integer(x), as.character(as.integer(x)), as.character(x))
  
  .build_port_string <- function(from_id, to_id, values_list) {
    n     <- length(from_id)
    lines <- character(n)
    for (i in seq_len(n)) {
      vals  <- paste(values_list[[i]], collapse = " ")
      comma <- if (i < n) "," else ""
      lines[i] <- sprintf("%d->%d %s%s&#x0A;", from_id[i], to_id[i], vals, comma)
    }
    paste0("[&#x0A;    ", paste(lines, collapse = "    "), "]")
  }
  
  .validate_params <- function(df) {
    errs <- character(0)
    chk <- function(cond, msg) if (any(cond, na.rm = TRUE)) errs <<- c(errs, msg)
    chk(df$percent  < 0 | df$percent  > 1, "percent must be in [0,1]")
    chk(df$exp_mean <= 0,                  "exp_mean must be > 0")
    chk(df$gen_mean <= 0,                  "gen_mean must be > 0")
    chk(df$exp_var  < 0,                   "exp_var must be >= 0")
    chk(df$gen_var  < 0,                   "gen_var must be >= 0")
    chk(df$exp_iso  < 0 | df$exp_iso > 2,  "exp_iso must be in [0,2]")
    chk(df$gen_iso  < 0 | df$gen_iso > 2,  "gen_iso must be in [0,2]")
    if (length(errs) > 0) stop(paste(errs, collapse = "\n"), call. = FALSE)
    invisible(TRUE)
  }
  
  .pick <- function(x, d) if (is.null(x) || (length(x) == 1 && is.na(x))) d else x
  
  uniform <- list(
    percent  = .pick(percent,  defaults$percent),
    exp_mean = .pick(exp_mean, defaults$exp_mean),
    exp_var  = .pick(exp_var,  defaults$exp_var),
    exp_iso  = .pick(exp_iso,  defaults$exp_iso),
    gen_mean = .pick(gen_mean, defaults$gen_mean),
    gen_var  = .pick(gen_var,  defaults$gen_var),
    gen_iso  = .pick(gen_iso,  defaults$gen_iso)
  )
  
  classes <- data.frame(
    id   = lusim_lc[[1]],
    name = as.character(lusim_lc[[2]]),
    stringsAsFactors = FALSE
  )
  
  skel <- expand.grid(from_id = classes$id, to_id = classes$id,
                      stringsAsFactors = FALSE)
  skel <- skel[skel$from_id != skel$to_id, , drop = FALSE]
  rownames(skel) <- NULL
  
  df <- skel
  for (nm in names(uniform)) df[[nm]] <- uniform[[nm]]
  
  if (!is.null(override_df)) {
    if (!is.data.frame(override_df))
      stop("`override_df` must be a data.frame.", call. = FALSE)
    
    required <- c("from_id", "to_id",
                  "percent", "exp_mean", "exp_var", "exp_iso",
                  "gen_mean", "gen_var", "gen_iso")
    miss <- setdiff(required, names(override_df))
    if (length(miss) > 0)
      stop("`override_df` is missing columns: ",
           paste(miss, collapse = ", "), call. = FALSE)
    
    key_df <- sprintf("%d->%d", df$from_id,          df$to_id)
    key_ov <- sprintf("%d->%d", override_df$from_id, override_df$to_id)
    
    if (anyDuplicated(key_ov))
      stop("`override_df` contains duplicate transitions", call. = FALSE)
    
    unknown <- setdiff(key_ov, key_df)
    if (length(unknown) > 0)
      warning("`override_df` contains unknown transitions: ",
              paste(unknown, collapse = ", "), call. = FALSE)
    
    cols <- c("percent", "exp_mean", "exp_var", "exp_iso",
              "gen_mean", "gen_var", "gen_iso")
    idx  <- match(key_df, key_ov)
    for (col in cols) {
      v <- override_df[[col]][idx]
      if (is.null(v)) next
      ok <- !is.na(v)
      df[[col]][ok] <- v[ok]
    }
  }
  
  if (isTRUE(validate)) .validate_params(df)
  
  list(
    percentOfTransitionsByExpansion = .build_port_string(
      df$from_id, df$to_id, lapply(df$percent, .fmt_num)
    ),
    patchExpansionParameters = .build_port_string(
      df$from_id, df$to_id,
      lapply(seq_len(nrow(df)), function(i)
        c(.fmt_num(df$exp_mean[i]), .fmt_num(df$exp_var[i]), .fmt_num(df$exp_iso[i])))
    ),
    patchGenerationParameters = .build_port_string(
      df$from_id, df$to_id,
      lapply(seq_len(nrow(df)), function(i)
        c(.fmt_num(df$gen_mean[i]), .fmt_num(df$gen_var[i]), .fmt_num(df$gen_iso[i])))
    )
  )
}

# ---------------------------------------------------------- shiny helpers ---

#' Help Texts for the Allocation UI
#'
#' Named list of tooltips (title + HTML body) used by the allocation
#' parameter UI. Each entry is keyed by the corresponding input ID.
#'
#' @format A named list of lists.
alloc_help_texts <- list(
  map1_file = list(
    title = "Initial Land Cover/Use Map",
    body = "<p>GeoTIFF (.tif) file containing the land cover/land use
            classification for the initial year (T1).</p> <p>Pixel values must correspond to the IDs defined in the
            Land Use/Cover Lookup Table.</p>"
  ),
  init_year = list(
    title = "Initial Year",
    body = "<p>Base year (T1) corresponding to the
            Initial Land Cover/Use Map.</p>"
  ),
  mapz_file = list(
    title = "Planning Unit Map",
    body = "<p>GeoTIFF (.tif) file delineating the planning units
            (e.g., provinces, watersheds, or spatial planning units) used to
            partition the simulation into independent regions.</p> <p>Each pixel value corresponds to a planning unit ID.
            The simulation runs the allocation step region by region,
            so planning units should cover the entire study extent.</p>"
  ),
  lc_file = list(
    title = "Land Use/Cover Lookup Table",
    body = "<p>CSV/XLSX table file with two columns:</p> <ol> <li>Numeric land cover ID (integer)</li> <li>Class name (text)</li> </ol>"
  ),
  rc_file = list(
    title = "Raster Cube Map",
    body = "<p>A pair of files produced by the SCIENDO Train module:</p> <ul> <li><code>sciendo_factors.tif</code> - raster stack of static variables
          (e.g., slope, elevation, distance to roads, and population density)
          used as predictors in the Weights of Evidence calculation.</li> <li><code>sciendo_factors.tif.aux.xml</code> - companion header describing
          the variable stack, the planning unit classes, and the
          simulation period value.</li> </ul> <p>Both files must be uploaded together. The app reads the XML
          file to extract the planning unit classes and the period value.</p>"
  ),
  repetition = list(
    title = "Simulation Periods",
    body = "<p>Number of simulation iterations to run.</p> <p>Each iteration produces its own projected landscape
            (<code>landscape2010.tif</code>, <code>landscape2015.tif</code>, ...).</p>"
  ),
  tm_path = list(
    title = "Transition Probability Matrix Folder",
    body = "<p>Folder from SCIENDO Train (Business As Usual) or SCIENDO Scenario Builder
            (Scenario) output containing the transition probability matrices:</p> <ul> <li>CSV table files containing the single-step transition matrix
            (<code>single_step000000.csv</code>), or</li> <li>XLSM table files containing macro-enabled Excel scenario
            matrices.</li> </ul>"
  ),
  dcf_path = list(
    title = "Weights of Evidence Folder",
    body = "<p>Folder containing the DCF files from the SCIENDO Train output
            (<code>woe000000.dcf</code> and its companion files) that store
            the Weights of Evidence coefficients.</p>"
  ),
  wd = list(
    title = "Output Directory",
    body = "<p>Folder where all simulation outputs will be written</p>"
  ),
  dinamica_path = list(
    title = "DINAMICA EGO Path (Optional)",
    body = "<p>Installation folder of DINAMICA EGO
            (e.g., <code>C:/Program Files/Dinamica EGO 6</code>).</p> <p>If left empty, the app will automatically detect the installation
            under <code>Program Files</code> and use the latest version found.
            Provide it manually only if automatic detection fails or if you
            need to use a specific version.</p>"
  ),
  memory_allocation = list(
    title = "Memory Allocation Policy",
    body = "<p>Controls how DINAMICA EGO manages RAM and disk space when
            running the simulation.</p> <ul> <li>Balanced (default): input maps are kept in memory, while results
            are written to disk if RAM is limited.</li> <li>Prefer Memory: keep both inputs and results in RAM when possible.</li> <li>Prefer Disk: stream everything through disk.</li> <li>Memory Only: abort if there is insufficient RAM.</li> <li>Aggressive: inputs are kept on disk, while results are kept in RAM
            when possible.</li> </ul>"
  ),
  custom = list(
    title = "Parameterize Allocate Transitions",
    body = "<p>Enable this option to customize how DINAMICA EGO allocates transitions.</p> <p>Controls the balance between patch expansion
            (growing existing patches) and patch generation
            (seeding brand-new patches), including their size and shape.</p>"
  ),
  percent = list(
    title = "Percent of Transitions by Expansion",
    body = "<p>Share of each transition handled by the Expander
            (growing existing patches) and the Patcher
            (creating new patches).</p> <ul> <li><code>0</code> = all transitions create new patches</li> <li><code>1</code> = all transitions expand existing patches</li> </ul> <p>Value range: <code>0</code> - <code>1</code>.</p>"
  ),
  exp_mean = list(
    title = "Expansion Mean Patch Size",
    body = "<p>Average size (hectares) of an expanded patch.</p> <p>Must be > 0.</p>"
  ),
  exp_var = list(
    title = "Expansion Patch Size Variance",
    body = "<p>Variance (hectares) of expanded patch sizes.</p> <p>Must be ≥ 0.</p>"
  ),
  exp_iso = list(
    title = "Expansion Patch Isometry",
    body = "<p>Shape of expanded patches.</p> <ul> <li><code>0</code> = linear / elongated</li> <li><code>1</code> = neutral</li> <li><code>2</code> = circular / compact</li> </ul> <p>Value range: <code>0</code> - <code>2</code>.</p>"
  ),
  gen_mean = list(
    title = "Generation Mean Patch Size",
    body = "<p>Average size (hectares) of a newly generated patch.</p> <p>Must be > 0.</p>"
  ),
  gen_var = list(
    title = "Generation Patch Size Variance",
    body = "<p>Variance (hectares) of newly generated patch sizes.</p> <p>Must be ≥ 0.</p>"
  ),
  gen_iso = list(
    title = "Generation Patch Isometry",
    body = "<p>Shape of newly generated patches.</p> <ul> <li><code>0</code> = linear</li> <li><code>1</code> = neutral</li> <li><code>2</code> = circular / compact</li> </ul> <p>Value range: <code>0</code> - <code>2</code>.</p>"
  ),
  override = list(
    title = "Per-Transition Allocation (Optional)",
    body = "<p>Upload an Excel/CSV template to customize allocation
            transition parameters for individual transitions.</p>"
  )
)

#' Clickable Help Icon for the Allocation UI
#'
#' @param id Help topic ID (matches a name in [alloc_help_texts]).
#' @return A `shiny.tag` containing an `actionLink` with a help icon.
alloc_help_icon <- function(id) {
  tags$span(
    class = "alloc-help-icon",
    actionLink(
      inputId = paste0("alloc_help_", id),
      label   = NULL,
      icon    = icon("circle-question")
    )
  )
}

#' Label With a Right-Aligned Help Icon
#'
#' @param text Label text.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` combining the label and a help icon.
label_with_help <- function(text, help_id) {
  tags$div(
    class = "alloc-label",
    tags$span(text),
    alloc_help_icon(help_id)
  )
}

#' Directory-Picker Button With an Inline Help Icon
#'
#' @param id Shiny input ID.
#' @param label Button label.
#' @param title Button title tooltip.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` containing a directory-picker button and help icon.
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
      alloc_help_icon(help_id)
    )
  )
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


#' Plot Multi-temporal Categorical Rasters with Interactive Time Stepper / Slider
#'
#' Renders a single Leaflet canvas with hardware-accelerated raster overlays
#' toggled via an interactive time stepper/slider pill (at bottom-left),
#' supporting instant transitions, responsive zoom/pan, legend scrolling, and hover popup.
#'
#' @param raster_list A named list of SpatRaster or RasterLayer objects. Names should be year labels (e.g. list("2019" = r1, "2024" = r2)).
#' @param cat_table A data.frame with at least 2 columns: ID and Category (and optional color_palette).
#' @param height Character. Height of the map canvas (default "600px").
#' @param container_width Character. Width of the map container (default "80%").
#' @param legend_title Character. Title for the legend (default "Kategori Tutupan Lahan").
#' @param basemap_url Character. URL template for Leaflet basemap.
#' @return An htmltools tagList / browsable widget that renders in R Markdown or Shiny.
#' @export
plot_multitemporal_raster_leaflet <- function(
    raster_list,
    cat_table,
    height = "700px",
    container_width = "100%",
    legend_title = "Kategori Tutupan Lahan",
    basemap_url = BASEMAP_PROVIDERS$eox$url,
    basemap_attr = BASEMAP_PROVIDERS$eox$attribution,
    basemap_name = BASEMAP_PROVIDERS$eox$name
) {
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("leaflet", quietly = TRUE)
  requireNamespace("htmlwidgets", quietly = TRUE)
  requireNamespace("htmltools", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  requireNamespace("png", quietly = TRUE)
  requireNamespace("xfun", quietly = TRUE)

  names(cat_table)[1] <- "ID"
  names(cat_table)[2] <- "Category"
  cat_table$ID <- as.numeric(cat_table$ID)

  years <- names(raster_list)
  if (is.null(years) || length(years) == 0) {
    years <- as.character(seq_along(raster_list))
    names(raster_list) <- years
  }

  # Ensure all inputs are SpatRaster and reprojected to EPSG:4326 if needed
  r_list_4326 <- list()
  for (yr in years) {
    r <- raster_list[[yr]]
    if (inherits(r, "RasterLayer")) {
      r <- terra::rast(r)
    }
    crs_str <- terra::crs(r, proj = TRUE)
    if (!grepl("longlat|4326", crs_str, ignore.case = TRUE)) {
      if (terra::ncol(r) > 1500 || terra::nrow(r) > 1500) {
        fact <- max(2, ceiling(max(terra::ncol(r), terra::nrow(r)) / 1000))
        r <- terra::aggregate(r, fact = fact, fun = "modal")
      }
      r_list_4326[[yr]] <- terra::project(r, "EPSG:4326", method = "near")
    } else {
      r_list_4326[[yr]] <- r
    }
  }

  # Unique categories across all years
  all_unique <- unique(unlist(lapply(r_list_4326, function(r) unique(terra::values(r, na.rm = TRUE)))))
  cat_tbl_filtered <- cat_table[cat_table$ID %in% all_unique, ]
  cat_tbl_filtered <- cat_tbl_filtered[order(cat_tbl_filtered$ID), ]

  reclass_from <- cat_tbl_filtered$ID
  reclass_to <- seq_along(cat_tbl_filtered$ID)
  reclass_matrix <- cbind(reclass_from, reclass_to)

  levels_df <- data.frame(
    ID = reclass_to,
    Category = factor(cat_tbl_filtered$Category, levels = cat_tbl_filtered$Category)
  )

  predefined_colors <- c(
    "#006400", "#228B22", "#008000", "#66CDAA", "#2E8B57", "#3CB371", "#8B4513", "#D2691E",
    "#6B8E23", "#B8860B", "#7CFC00", "#32CD32", "#808000", "#4682B4", "#FF4500", "#FFA500",
    "#1E90FF", "#A0522D", "#FFFFFF", "#73C8D2", "#0046FF", "#E6194B", "#3CB44B", "#FFE119",
    "#4363D8", "#F58231", "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE", "#008080"
  )

  map_colors <- if ("color_palette" %in% names(cat_tbl_filtered)) {
    cat_tbl_filtered$color_palette
  } else {
    if (nrow(cat_tbl_filtered) <= length(predefined_colors)) {
      predefined_colors[seq_len(nrow(cat_tbl_filtered))]
    } else {
      grDevices::colorRampPalette(predefined_colors)(nrow(cat_tbl_filtered))
    }
  }

  pal_raster <- leaflet::colorFactor(palette = map_colors, domain = reclass_to, na.color = "#00000000", alpha = TRUE)
  pal_legend <- leaflet::colorFactor(palette = map_colors, domain = levels_df$Category)

  # Convert each raster to:
  # 1. Visual PNG Data URI (palette colors for Leaflet display)
  # 2. ID PNG Data URI (exact integer class index in R/G channels for 100% accurate hover lookup)
  raster_uris <- list()
  id_uris     <- list()
  for (yr in years) {
    r_rec <- terra::classify(r_list_4326[[yr]], reclass_matrix, others = NA)
    r_layer <- raster::raster(r_rec)
    vals <- raster::values(r_layer)
    nc <- ncol(r_layer)
    nr <- nrow(r_layer)

    # 1. Visual tile
    cols <- pal_raster(vals)
    tileData <- as.raw(grDevices::col2rgb(cols, alpha = TRUE))
    dim(tileData) <- c(4, nc, nr)
    pngData <- png::writePNG(tileData)
    raster_uris[[yr]] <- xfun::base64_uri(pngData, "image/png")

    # 2. ID data tile (R = index % 256, G = index / 256, B = 0, A = 255 or 0)
    is_valid <- !is.na(vals) & (vals > 0)
    id_mat <- matrix(as.raw(0), nrow = 4, ncol = length(vals))
    if (any(is_valid)) {
      v_sub <- vals[is_valid]
      id_mat[1, is_valid] <- as.raw(v_sub %% 256)
      id_mat[2, is_valid] <- as.raw((v_sub %/% 256) %% 256)
      id_mat[4, is_valid] <- as.raw(255)
    }
    dim(id_mat) <- c(4, nc, nr)
    idPngData <- png::writePNG(id_mat)
    id_uris[[yr]] <- xfun::base64_uri(idPngData, "image/png")
  }
  json_uris    <- jsonlite::toJSON(raster_uris, auto_unbox = TRUE)
  json_id_uris <- jsonlite::toJSON(id_uris, auto_unbox = TRUE)

  # Bounding box in EPSG:4326
  bb <- terra::ext(r_list_4326[[1]])
  xmin_val <- unname(as.numeric(bb$xmin))
  ymin_val <- unname(as.numeric(bb$ymin))
  xmax_val <- unname(as.numeric(bb$xmax))
  ymax_val <- unname(as.numeric(bb$ymax))
  r_width  <- ncol(r_list_4326[[1]])
  r_height <- nrow(r_list_4326[[1]])

  lookup_list <- lapply(seq_len(nrow(cat_tbl_filtered)), function(i) {
    hex <- map_colors[i]
    list(
      reclass_id = i,
      id = cat_tbl_filtered$ID[i],
      name = cat_tbl_filtered$Category[i],
      color = hex
    )
  })
  json_lookup <- jsonlite::toJSON(lookup_list, auto_unbox = TRUE)
  json_years  <- jsonlite::toJSON(years, auto_unbox = TRUE)

  uid <- paste0("sciendo_map_", paste0(sample(c(letters, 0:9), 8), collapse = ""))

  m <- leaflet::leaflet(
    elementId = uid,
    width = "100%",
    height = height,
    options = leaflet::leafletOptions(zoomSnap = 0.25)
  ) %>%
    leaflet::addTiles(
      urlTemplate = basemap_url,
      attribution = basemap_attr,
      group = basemap_name
    ) %>%
    leaflet::fitBounds(
      lng1 = xmin_val, lat1 = ymin_val,
      lng2 = xmax_val, lat2 = ymax_val
    ) %>%
    leaflet::addLegend(
      position = "topright",
      pal = pal_legend,
      values = levels_df$Category,
      title = legend_title,
      opacity = 1
    ) %>%
    leaflet::addEasyButton(
      leaflet::easyButton(
        icon = "fa-globe",
        title = "Kembalikan ke Batas Awal (Reset Zoom)",
        onClick = leaflet::JS(sprintf(
          "function(btn, map){ map.fitBounds([[%f, %f], [%f, %f]]); }",
          ymin_val, xmin_val, ymax_val, xmax_val
        ))
      )
    )

  js_controller <- paste0("
  function(el, x) {
    var map = this;
    var uid = '", uid, "';
    var years = ", json_years, ";
    var lookupData = ", json_lookup, ";
    var rasterUris = ", json_uris, ";
    var idUris = ", json_id_uris, ";
    var bounds = { xmin: ", xmin_val, ", ymin: ", ymin_val, ", xmax: ", xmax_val, ", ymax: ", ymax_val, " };
    var dims = { width: ", r_width, ", height: ", r_height, " };

    window['sciendoMap_' + uid] = map;
    window['sciendoYears_' + uid] = years;
    window['currentYearIndex_' + uid] = 0;
    window['isPlaying_' + uid] = false;
    window['playTimer_' + uid] = null;
    window['sciendoOverlays_' + uid] = {};
    window['lastHoverEvent_' + uid] = null;

    // Fast lookup dictionary by reclassified class index (1, 2, ..., N)
    var lookupById = {};
    lookupData.forEach(function(item) {
      lookupById[item.reclass_id] = item;
    });

    // In-memory pixel data of the exact integer ID masks
    var yearIdPixelData = {};
    years.forEach(function(yr) {
      if (idUris[yr]) {
        var img = new Image();
        img.src = idUris[yr];
        img.onload = function() {
          var cvs = document.createElement('canvas');
          cvs.width = img.width;
          cvs.height = img.height;
          var ctx = cvs.getContext('2d', { willReadFrequently: true });
          ctx.imageSmoothingEnabled = false;
          ctx.drawImage(img, 0, 0);
          yearIdPixelData[yr] = ctx.getImageData(0, 0, img.width, img.height).data;
        };
      }
    });

    var boundsLatLng = [[bounds.ymin, bounds.xmin], [bounds.ymax, bounds.xmax]];
    years.forEach(function(yr, i) {
      var uri = rasterUris[yr];
      if (uri) {
        var overlay = L.imageOverlay(uri, boundsLatLng, {
          opacity: (i === 0 ? 0.90 : 0),
          interactive: false,
          crossOrigin: true,
          className: 'sciendo-raster-overlay'
        }).addTo(map);

        window['sciendoOverlays_' + uid][yr] = overlay;

        var imgEl = overlay.getElement();
        if (imgEl && i !== 0) {
          imgEl.style.opacity = '0';
          imgEl.style.visibility = 'hidden';
        }
      }
    });

    function setYearIndex(idx) {
      if (idx < 0) idx = 0;
      if (idx >= years.length) idx = years.length - 1;
      window['currentYearIndex_' + uid] = idx;
      var targetYear = years[idx];

      years.forEach(function(yr, i) {
        var ov = window['sciendoOverlays_' + uid][yr];
        if (!ov) return;
        var el = ov.getElement();
        if (i === idx) {
          ov.setOpacity(0.90);
          if (el) {
            el.style.opacity = '0.90';
            el.style.visibility = 'visible';
          }
        } else {
          ov.setOpacity(0);
          if (el) {
            el.style.opacity = '0';
            el.style.visibility = 'hidden';
          }
        }
      });

      var vaYear = document.getElementById('va-year-' + uid);
      var vaSlider = document.getElementById('va-slider-' + uid);
      if (vaYear) vaYear.innerText = targetYear;
      if (vaSlider) vaSlider.value = idx;

      var tooltip = document.getElementById('lulc-hover-tooltip-' + uid);
      if (window['lastHoverEvent_' + uid] && tooltip && tooltip.style.display !== 'none') {
        updateHoverTooltip(window['lastHoverEvent_' + uid]);
      }
    }
    window['setYearIndex_' + uid] = setYearIndex;

    function togglePlay() {
      var isPlaying = !window['isPlaying_' + uid];
      window['isPlaying_' + uid] = isPlaying;
      var playBtn = document.getElementById('va-play-' + uid);
      if (playBtn) {
        playBtn.innerHTML = isPlaying ? '<i class=\"fa fa-pause\"></i>' : '<i class=\"fa fa-play\"></i>';
        if (isPlaying) {
          playBtn.classList.remove('btn-play');
          playBtn.classList.add('btn-pause-active');
          playBtn.title = 'Jeda Animasi (Spasi)';
        } else {
          playBtn.classList.remove('btn-pause-active');
          playBtn.classList.add('btn-play');
          playBtn.title = 'Putar Animasi (Spasi)';
        }
      }
      if (isPlaying) {
        if (window['playTimer_' + uid]) clearInterval(window['playTimer_' + uid]);
        window['playTimer_' + uid] = setInterval(function() {
          var curr = window['currentYearIndex_' + uid] || 0;
          var nextIdx = (curr + 1) % years.length;
          setYearIndex(nextIdx);
        }, 1500);
      } else {
        if (window['playTimer_' + uid]) {
          clearInterval(window['playTimer_' + uid]);
          window['playTimer_' + uid] = null;
        }
      }
    }
    window['togglePlay_' + uid] = togglePlay;

    var playBtn = document.getElementById('va-play-' + uid);
    if (playBtn) {
      playBtn.onclick = function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        togglePlay();
      };
    }

    var prevBtn = document.getElementById('va-prev-' + uid);
    if (prevBtn) {
      prevBtn.onclick = function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        var curr = window['currentYearIndex_' + uid] || 0;
        setYearIndex(curr - 1);
      };
    }

    var nextBtn = document.getElementById('va-next-' + uid);
    if (nextBtn) {
      nextBtn.onclick = function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        var curr = window['currentYearIndex_' + uid] || 0;
        setYearIndex(curr + 1);
      };
    }

    var sliderEl = document.getElementById('va-slider-' + uid);
    if (sliderEl) {
      sliderEl.oninput = function(e) {
        if (e) e.stopPropagation();
        setYearIndex(parseInt(this.value, 10));
      };
    }

    document.addEventListener('keydown', function(e) {
      if (e.target.tagName === 'INPUT' && e.target.type !== 'range') return;
      if (e.target.tagName === 'SELECT' || e.target.tagName === 'TEXTAREA') return;
      var curr = window['currentYearIndex_' + uid] || 0;
      if (e.key === 'ArrowLeft') {
        setYearIndex(curr - 1);
      } else if (e.key === 'ArrowRight') {
        setYearIndex(curr + 1);
      } else if (e.key === ' ') {
        e.preventDefault();
        togglePlay();
      }
    });

    var tooltip = document.getElementById('lulc-hover-tooltip-' + uid);
    var swatch = document.getElementById('tooltip-swatch-' + uid);
    var idBadge = document.getElementById('tooltip-id-' + uid);
    var nameBadge = document.getElementById('tooltip-name-' + uid);
    var hoverRaf = null;

    function updateHoverTooltip(e) {
      var activeYear = years[window['currentYearIndex_' + uid] || 0];
      var data = yearIdPixelData[activeYear];
      if (!data || !tooltip) return;

      var lng = e.latlng.lng;
      var lat = e.latlng.lat;

      if (lng < bounds.xmin || lng > bounds.xmax || lat < bounds.ymin || lat > bounds.ymax) {
        tooltip.style.display = 'none';
        return;
      }

      var col = Math.floor((lng - bounds.xmin) / (bounds.xmax - bounds.xmin) * dims.width);
      var row = Math.floor((bounds.ymax - lat) / (bounds.ymax - bounds.ymin) * dims.height);

      if (col < 0 || col >= dims.width || row < 0 || row >= dims.height) {
        tooltip.style.display = 'none';
        return;
      }

      var idx = (row * dims.width + col) * 4;
      var alpha = data[idx + 3];

      // Alpha === 0 indicates NoData or outside the study area
      if (alpha === 0) {
        tooltip.style.display = 'none';
        return;
      }

      var r = data[idx];
      var g = data[idx + 1];
      var reclassIdx = r + (g * 256);

      var match = lookupById[reclassIdx];

      if (match) {
        swatch.style.backgroundColor = match.color;
        idBadge.innerText = 'ID ' + match.id;
        nameBadge.innerText = match.name;

        var pt = e.containerPoint;
        tooltip.style.left = (pt.x + 16) + 'px';
        tooltip.style.top = (pt.y - 12) + 'px';
        tooltip.style.display = 'flex';
      } else {
        tooltip.style.display = 'none';
      }
    }

    map.on('mousemove', function(e) {
      window['lastHoverEvent_' + uid] = e;
      if (e.originalEvent && (e.originalEvent.buttons > 0 || (map.dragging && map.dragging.moving()))) {
        if (tooltip) tooltip.style.display = 'none';
        return;
      }
      if (hoverRaf) cancelAnimationFrame(hoverRaf);
      hoverRaf = requestAnimationFrame(function() {
        updateHoverTooltip(e);
      });
    });

    map.on('movestart zoomstart dragstart', function() {
      if (tooltip) tooltip.style.display = 'none';
    });

    map.on('mouseout', function() {
      window['lastHoverEvent_' + uid] = null;
      if (tooltip) tooltip.style.display = 'none';
    });

    function updateBreakoutRight() {
      var box = document.getElementById('sciendo-box-' + uid);
      if (!box) return;
      if (window.innerWidth >= 992) {
        var rect = box.parentElement.getBoundingClientRect();
        var availableWidth = window.innerWidth - rect.left - 40;
        box.style.width = Math.max(availableWidth, rect.width) + 'px';
        box.style.maxWidth = 'none';
      } else {
        box.style.width = '100%';
        box.style.maxWidth = '100%';
      }
      map.invalidateSize();
    }
    updateBreakoutRight();
    window.addEventListener('resize', updateBreakoutRight);

    setTimeout(function() {
      updateBreakoutRight();
      map.invalidateSize();
      setYearIndex(0);
    }, 120);
  }
  ")

  m <- htmlwidgets::onRender(m, js_controller)

  widget_ui <- htmltools::tagList(
    htmltools::tags$style(htmltools::HTML("
      body {
        overflow-x: hidden;
      }
      .sciendo-breakout-right-box {
        position: relative;
        width: 100%;
        max-width: none;
        margin: 20px 0;
      }
      .sciendo-map-wrapper {
        position: relative;
        width: 100%;
        border-radius: 10px;
        overflow: hidden;
        border: 1px solid #cbd5e1;
        margin: 0;
      }
      .sciendo-raster-overlay {
        image-rendering: -webkit-optimize-contrast !important;
        image-rendering: crisp-edges !important;
        image-rendering: pixelated !important;
        transition: none !important;
        animation: none !important;
      }
      .lulc-hover-tooltip {
        position: absolute;
        z-index: 2000;
        pointer-events: none;
        background: rgba(15, 23, 42, 0.94);
        backdrop-filter: blur(6px);
        color: #f8fafc;
        padding: 6px 12px;
        border-radius: 8px;
        font-size: 12px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
        display: none;
        align-items: center;
        gap: 8px;
        border: 1px solid rgba(255, 255, 255, 0.2);
        white-space: nowrap;
      }
      .lulc-hover-tooltip .color-swatch {
        width: 13px;
        height: 13px;
        border-radius: 3px;
        border: 1px solid rgba(255,255,255,0.5);
        display: inline-block;
      }
      .lulc-hover-tooltip .id-badge {
        background: rgba(255, 255, 255, 0.22);
        padding: 1px 6px;
        border-radius: 4px;
        font-size: 11px;
        font-weight: 700;
        color: #e2e8f0;
      }
      .lulc-hover-tooltip .class-name {
        font-weight: 500;
        color: #ffffff;
      }
      .variant-a-ctrl {
        position: absolute;
        bottom: 25px;
        left: 20px;
        z-index: 1000;
        background: rgba(255, 255, 255, 0.96);
        backdrop-filter: blur(8px);
        color: #0f172a;
        padding: 7px 14px;
        border-radius: 35px;
        box-shadow: 0 8px 30px rgba(0, 0, 0, 0.2);
        display: flex;
        align-items: center;
        gap: 12px;
        border: 1px solid rgba(0, 0, 0, 0.12);
      }
      .variant-a-ctrl button {
        background: #f8fafc;
        border: 1px solid #cbd5e1;
        color: #334155;
        width: 32px;
        height: 32px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.2s;
        cursor: pointer;
        font-size: 0.8rem;
      }
      .variant-a-ctrl button:hover {
        background: #2563eb;
        color: white;
        border-color: #2563eb;
        transform: scale(1.08);
      }
      .variant-a-ctrl button.btn-play {
        background: #2563eb;
        color: white;
        border-color: #2563eb;
      }
      .variant-a-ctrl button.btn-play:hover {
        background: #1d4ed8;
      }
      .variant-a-ctrl button.btn-pause-active {
        background: #ef4444 !important;
        color: white !important;
        border-color: #ef4444 !important;
      }
      .variant-a-ctrl button.btn-pause-active:hover {
        background: #dc2626 !important;
      }
      .variant-a-ctrl .year-badge {
        font-weight: 500;
        font-size: 1.15rem;
        color: #1e293b;
        letter-spacing: 0.5px;
        min-width: 55px;
        text-align: center;
      }
      .variant-a-ctrl input[type='range'] {
        width: 140px;
        accent-color: #2563eb;
        cursor: pointer;
      }
      .leaflet-control.legend, .info.legend {
        max-height: 630px !important;
        overflow-y: auto !important;
        overflow-x: hidden !important;
        background: rgba(255, 255, 255, 0.95) !important;
        backdrop-filter: blur(8px);
        padding: 12px 16px !important;
        border-radius: 10px !important;
        box-shadow: 0 6px 20px rgba(0,0,0,0.18) !important;
        border: 1px solid rgba(0,0,0,0.12) !important;
        font-size: 11.5px !important;
        line-height: 19px !important;
      }
      .info.legend::-webkit-scrollbar { width: 6px; }
      .info.legend::-webkit-scrollbar-track { background: #f1f5f9; border-radius: 4px; }
      .info.legend::-webkit-scrollbar-thumb { background: #94a3b8; border-radius: 4px; }
      .info.legend::-webkit-scrollbar-thumb:hover { background: #64748b; }
    ")),

    htmltools::tags$div(
      id = paste0("sciendo-box-", uid),
      class = "sciendo-breakout-right-box",
      htmltools::tags$div(
        class = "sciendo-map-wrapper",
        style = paste0("width: 100%; height: ", height, ";"),
        htmltools::tags$div(
          id = paste0("lulc-hover-tooltip-", uid),
          class = "lulc-hover-tooltip",
          htmltools::tags$span(id = paste0("tooltip-swatch-", uid), class = "color-swatch"),
          htmltools::tags$span(id = paste0("tooltip-id-", uid), class = "id-badge", "ID 1"),
          htmltools::tags$span(id = paste0("tooltip-name-", uid), class = "class-name", "Nama Kelas")
        ),
        htmltools::tags$div(
          id = paste0("container-variant-a-", uid),
          class = "variant-a-ctrl",
          htmltools::tags$button(
            id = paste0("va-prev-", uid),
            title = "Tahun Sebelumnya (Left Arrow)",
            onclick = paste0("if(window['setYearIndex_", uid, "']) window['setYearIndex_", uid, "'](window['currentYearIndex_", uid, "'] - 1);"),
            htmltools::HTML("<i class='fa fa-chevron-left'></i>")
          ),
          htmltools::tags$span(id = paste0("va-year-", uid), class = "year-badge", years[1]),
          htmltools::tags$input(
            id = paste0("va-slider-", uid),
            type = "range",
            min = "0",
            max = as.character(length(years) - 1),
            value = "0",
            step = "1",
            oninput = paste0("if(window['setYearIndex_", uid, "']) window['setYearIndex_", uid, "'](parseInt(this.value, 10));")
          ),
          htmltools::tags$button(
            id = paste0("va-next-", uid),
            title = "Tahun Berikutnya (Right Arrow)",
            onclick = paste0("if(window['setYearIndex_", uid, "']) window['setYearIndex_", uid, "'](window['currentYearIndex_", uid, "'] + 1);"),
            htmltools::HTML("<i class='fa fa-chevron-right'></i>")
          ),
          htmltools::tags$button(
            id = paste0("va-play-", uid),
            class = "btn-play",
            title = "Putar Animasi (Spasi)",
            onclick = paste0("if(window['togglePlay_", uid, "']) window['togglePlay_", uid, "']();"),
            htmltools::HTML("<i class='fa fa-play'></i>")
          )
        ),
        m
      )
    )
  )

  return(widget_ui)
}
