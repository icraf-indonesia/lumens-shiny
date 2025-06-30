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
    fill_scale <- scale_fill_manual(values = c(
      "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
      "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
      "#86BCB6", "#FFB84D", "#A5C1DC", "#D37295", "#C4AD66",
      "#7B8D8E", "#B17B62", "#8CD17D", "#DE9D9C", "#5A5A5A",
      "#A0A0A0", "#D7B5A6", "#6D9EEB", "#E69F00", "#56B4E9",
      "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
      "#999999", "#E51E10", "#FF7F00", "#FFFF33", "#A65628",
      "#F781BF", "#999933", "#8DD3C7", "#FFFFB3", "#BEBADA",
      "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
      "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#E41A1C"), 
      na.value = "white")
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
generate_sciendo_simulate_report <- function(output, dir) {
  report_params <- list(
    start_time = output$start_time,
    end_time = output$end_time,
    inputs = output$inputs,
    session_log = output$session_log
  )
  output_file <- paste0("sciendo_simulate_report_", Sys.Date(), ".html")
  rmarkdown::render(
    "../report_template/sciendo_simulate_report_template.Rmd",
    output_file = output_file,
    output_dir = dir,
    params = report_params
  )
}


executeDINAMICA <- function(params, memory_allocation) {
  # Find DINAMICA directory if not provided
  if (is.null(params$dinamica_path) | identical(params$dinamica_path, character(0))) {
    program_files <- c("C:/Program Files/", "C:/Program Files (x86)/")
    dinamica_dirs <- list.files(program_files, pattern = "^Dinamica EGO", full.names = TRUE)
    
    if (length(dinamica_dirs) == 0) {
      stop("No DINAMICA EGO installation found.")
    }
    
    # Sort directories to use the latest version if multiple are found
    dinamica_path <- sort(dinamica_dirs, decreasing = TRUE)[1]
  }
  
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
  # command<-paste('"', dinamica_exe, '" -processors 0 -log-level 4 "', params$egoml, '"', sep="")
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

generate_egoml_simulate <- function(lc1_path, lusim_lc, 
                                    zone_path, ers_path, n_rep,
                                    tm_path, dcf_path,
                                    output_dir, probability = FALSE,
                                    egoml,
                                    memory_allocation) {
  prob_path <- paste0(output_dir, "/probabilities.tif")
  landscape_path <- paste0(output_dir, "/landscape.tif")
  
  skeleton <- expand.grid(nT1 = lusim_lc[, 1], nT2 = lusim_lc[, 1])
  skeleton <- skeleton[skeleton$nT1 != skeleton$nT2, ]
  skeleton <- na.omit(skeleton)
  
  # rebuild the chunk
  skeleton$char <- paste(skeleton$nT1, skeleton$nT2, sep = "-&gt;")
  skeleton$char_fx <- paste0(skeleton$char, " 0.3,&#x0A;")
  skeleton[nrow(skeleton), "char_fx"] <- gsub("3,&", "3&", skeleton[nrow(skeleton), "char_fx"])
  
  txt_skl <- paste(skeleton$char_fx, collapse = "    ")
  txt_skl2 <- gsub("0.3", "2 1 1", txt_skl)
  txt_skl3 <- gsub("2 1 1", "1 1 1", txt_skl2)
  
  # begin writing tag
  con <- xmlOutputDOM(tag="script")
  # add property
  con$addTag("property", attrs=c(key="dff.date", value="2016-Nov-09 17:01:03"))
  con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))
  
  # begin.
  # add functor = LoadMap
  con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Static variable maps."))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', ers_path, '"', sep=''))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), "0")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id="v1"))
  con$closeTag("functor") 
  # end.
  
  # begin.
  # add functor = LoadCategoricalMap
  con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Initial landscape maps."))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', lc1_path, '"', sep=''))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), "0")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id="v2"))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add functor = LoadCategoricalMap
  con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Planning Unit"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Municipalities"))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', zone_path, '"', sep=''))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), "0")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id="v3"))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add containerfunctor = ForEachRegion
  con$addTag("containerfunctor", attrs=c(name="RegionManager"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
  con$addTag("property", attrs=c(key="dff.functor.alias", value="regionManager3260"))
  con$addTag("inputport", attrs=c(name="regions", peerid="v3"))
  con$addTag("inputport", attrs=c(name="borderCells"), 0)
  con$addTag("internaloutputport", attrs=c(name="regionManager", id="v4"))
  
  # add containerfunctor = Repeat
  con$addTag("containerfunctor", attrs=c(name="Repeat"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
  con$addTag("property", attrs=c(key="dff.functor.alias", value="repeat279"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Simulation model."))
  con$addTag("inputport", attrs=c(name="iterations"), n_rep)
  con$addTag("internaloutputport", attrs=c(name="step", id="v5"))
  
  # add functor = LoadCategoricalMap
  con$addTag("functor", attrs=c(name="MuxCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Landscape"))
  con$addTag("inputport", attrs=c(name="initial", peerid="v2"))
  con$addTag("inputport", attrs=c(name="feedback", peerid="v15"))
  con$addTag("outputport", attrs=c(name="map", id="v6"))
  con$closeTag("functor")
  
  # add functor = SaveMap
  con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap282"))
  con$addTag("inputport", attrs=c(name="map", peerid="v15"))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', landscape_path, '"', sep=''))
  con$addTag("inputport", attrs=c(name="suffixDigits"), 2)
  con$addTag("inputport", attrs=c(name="step", peerid="v5"))
  con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$closeTag("functor")
  
  # add functor = SaveMap
  if(probability) {
    con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
    con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap3414"))
    con$addTag("inputport", attrs=c(name="map", peerid="v16"))
    con$addTag("inputport", attrs=c(name="filename"), paste('"', prob_path, '"', sep=''))
    con$addTag("inputport", attrs=c(name="suffixDigits"), 4)
    con$addTag("inputport", attrs=c(name="step", peerid="v5"))
    con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
    con$addTag("inputport", attrs=c(name="workdir"), ".none")
    con$closeTag("functor")
  }
  
  # add containerfunctor = ForEachCategory
  con$addTag("containerfunctor", attrs=c(name="ForEachCategory"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
  con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachCategory283"))
  con$addTag("inputport", attrs=c(name="categorization", peerid="v3"))
  con$addTag("internaloutputport", attrs=c(name="step", id="v7"))
  
  con$addTag("functor", attrs=c(name="IntegerValue"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="int290"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="This operator is used here to force a dependence between two groups."))
  con$addTag("inputport", attrs=c(name="constant"), 0)
  con$addTag("outputport", attrs=c(name="object", id="v8"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="LoadTable"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Transition Matrix"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Load transition matrix."))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', tm_path, '/single_step.csv"', sep=''))
  con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
  con$addTag("inputport", attrs=c(name="step", peerid="v7"))
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="table", id="v9"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="LoadWeights"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Weights of Evidence Coefficients"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Load Weights of Evidence coefficients."))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', dcf_path, '/woe.dcf"', sep=''))
  con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
  con$addTag("inputport", attrs=c(name="step", peerid="v7"))
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="weights", id="v10"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="RegionalCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="regionalCategoricalMap289"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Assign a map to the region using the given identifier."))
  con$addTag("inputport", attrs=c(name="globalMapName"), paste('"landscape"', sep=''))
  con$addTag("inputport", attrs=c(name="regionalMap", peerid="v11"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="AllocateTransitions"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Updated Landscape (Region)"))
  con$addTag("inputport", attrs=c(name="lanscape", peerid="v13"))
  con$addTag("inputport", attrs=c(name="probabilities", peerid="v14"))
  con$addTag("inputport", attrs=c(name="transitionMatrix", peerid="v9"))
  con$addTag("inputport", attrs=c(name="percentOfTransitionsByExpansion"), paste('[&#x0A;    ', txt_skl, ']', sep=''))
  con$addTag("inputport", attrs=c(name="patchExpansionParameters"), paste('[&#x0A;    ', txt_skl2, ']', sep=''))
  con$addTag("inputport", attrs=c(name="patchGenerationParameters"), paste('[&#x0A;    ', txt_skl3, ']', sep=''))
  con$addTag("inputport", attrs=c(name="printTransitionInfo"), ".no")
  con$addTag("outputport", attrs=c(name="resultingLanscape", id="v11"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="RegionalizeMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables (Region)"))
  con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
  con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
  con$addTag("outputport", attrs=c(name="regionalMap", id="v12"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Landscape (Region)"))
  con$addTag("inputport", attrs=c(name="globalMap", peerid="v6"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
  con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
  con$addTag("outputport", attrs=c(name="regionalMap", id="v13"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="RegionalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="regionalMap3412"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Assign a map to the region using the given identifier."))
  con$addTag("inputport", attrs=c(name="globalMapName"), paste('"probabilities"', sep=''))
  con$addTag("inputport", attrs=c(name="regionalMap", peerid="v14"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v7"))
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
  con$closeTag("functor")
  
  con$addTag("containerfunctor", attrs=c(name="CalcWOfEProbabilityMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Probabilities (Region)"))
  con$addTag("property", attrs=c(key="dff.functor.extendedcomment", value="Calculate probability map."))
  con$addTag("inputport", attrs=c(name="landscape", peerid="v13"))
  con$addTag("inputport", attrs=c(name="weights", peerid="v10"))
  con$addTag("inputport", attrs=c(name="transitions"), paste('[ ', paste(skeleton$char, collapse = ", "), ']', sep=''))
  con$addTag("inputport", attrs=c(name="cellType"), ".uint8")
  con$addTag("inputport", attrs=c(name="nullValue"), ".default")
  con$addTag("outputport", attrs=c(name="probabilities", id="v14"))
  
  con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMap298"))
  con$addTag("inputport", attrs=c(name="map", peerid="v12"))
  con$addTag("inputport", attrs=c(name="mapName"), paste('"static_var"', sep=''))
  con$closeTag("functor")
  
  con$closeTag("containerfunctor") #    CalcWOfEProbabilityMap
  
  con$closeTag("containerfunctor") # ForEachCategory
  
  # add containerfunctor = Group
  con$addTag("containerfunctor", attrs=c(name="Group"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.container.collapsed", value="no"))
  con$addTag("property", attrs=c(key="dff.functor.alias", value="group300"))
  
  con$addTag("functor", attrs=c(name="IntegerValue"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="int302"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="This operator is used here to force a dependence between two groups."))
  con$addTag("inputport", attrs=c(name="constant", peerid="v8"))
  con$closeTag("functor")
  
  con$addTag("functor", attrs=c(name="MergeRegionalCategoricalMaps"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Updated Landscape"))
  con$addTag("property", attrs=c(key="dff.functor.comment", value="Merge all maps assigned to the regions using the given identifier."))
  con$addTag("inputport", attrs=c(name="globalMapName"), paste('"landscape"', sep=''))
  con$addTag("inputport", attrs=c(name="mergeNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
  con$addTag("outputport", attrs=c(name="globalMap", id="v15"))
  con$closeTag("functor")
  
  if(probability) {
    con$addTag("functor", attrs=c(name="MergeRegionalMaps"), close=FALSE)
    con$addTag("property", attrs=c(key="dff.functor.alias", value="mergeRegionalMaps3413"))
    con$addTag("property", attrs=c(key="dff.functor.comment", value="Merge all maps assigned to the regions using the given identifier."))
    con$addTag("inputport", attrs=c(name="globalMapName"), paste('"probabilities"', sep=''))
    con$addTag("inputport", attrs=c(name="mergeNonRegionCells"), ".no")
    con$addTag("inputport", attrs=c(name="regionManager", peerid="v4"))
    con$addTag("outputport", attrs=c(name="globalMap", id="v16"))
    con$closeTag("functor")
  }
  
  con$closeTag("containerfunctor") # Group
  
  con$closeTag("containerfunctor")  # Repeat
  con$closeTag("containerfunctor") # RegionManager
  # end.
  
  egoml_sim_file <- paste0(output_dir, "/", egoml, ".egoml")
  saveXML(con$value(), file = egoml_sim_file)
  
  # replace ampersand code character
  egoml_text  <- readLines(egoml_sim_file)
  egoml_text_new  <- gsub(pattern="amp;", replace="", x=egoml_text)
  writeLines(egoml_text_new, con=egoml_sim_file)
  
  out <- list(
    egoml_sim_file = egoml_sim_file,
    rep = n_rep,
    lc1 = lc1_path,
    zone = zone_path,
    transition_mtx = tm_path,
    dcf = dcf_path,
    ers = ers_path
  )
  
  return(out)
}

run_dinamica_simulation <- function(dinamica_path = NULL, output_dir, egoml, memory_allocation) {
  params <- list()
  params$dinamica_path <- dinamica_path
  params$output_dir <- output_dir
  params$egoml <- egoml
  
  executeDINAMICA(params, memory_allocation)
  
  # check result
  new_lc_file <- paste0(output_dir, "/landscape01.tif")
  if (!file.exists(new_lc_file)) {
    stop("Land use change simulation failed! Check DINAMICA EGO log.")
  }
}

run_sciendo_simulate_process <- function(lc_t1_path, lc_lookup_table_path, lc_lookup_table, zone_lookup_table, zone_path, ers_path, 
                                         n_rep, tm_path, dcf_path, dinamica_path = NULL, output_dir, memory_allocation, progress_callback = NULL) {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (!is.null(progress_callback)) progress_callback(0.3, "generate egoml: initialize simulation per region parameters")
  out_sim <- generate_egoml_simulate(lc_t1_path, lc_lookup_table, 
                                     zone_path, ers_path, n_rep,
                                     tm_path, dcf_path, output_dir, 
                                     probability = FALSE, egoml = "03_sciendo_simulation")
  
  if (!is.null(progress_callback)) progress_callback(0.7, "run dinamica simulation per region")
  run_dinamica_simulation(dinamica_path, output_dir, out_sim$egoml_sim_file, memory_allocation)
  
  
  
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  session_log <- format_session_info_table()
  
  out <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    inputs = list(
      lc_t1_path = lc_t1_path,
      lc_lookup_table_path = lc_lookup_table_path,
      zone_path = zone_path,
      zone_lookup_table = zone_lookup_table,
      ers_path = ers_path,
      tm_path = tm_path,
      dcf_path = dcf_path,
      rep = n_rep,
      output_dir = output_dir
    ),
    session_log = session_log
  )
  
  if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")

  if (!is.null(progress_callback)) progress_callback(1, "generate report")
  generate_sciendo_simulate_report(output = out, dir = output_dir)
  
  return(out)
}

# Functions for report ----------------------------------------------------

#' @title Generate a Robust and Interactive Stacked Area Chart using Plotly
#' @description This function takes a tibble of land use data and creates a
#' stacked area chart directly with plotly. It is robust to variations in
#' column names and data types.
#' @param luc_data_wide A tibble or data.frame in wide format.
#' @param class_col A string specifying the name of the column containing land use
#'   class labels. Defaults to "LC".
#' @param id_col A string specifying the name of the column containing row
#'   identifiers. This column is excluded from the plot. Defaults to "ID".
#' @param chart_title The main title for the chart.
#' @param x_axis_label The label for the x-axis.
#' @param y_axis_label The label for the y-axis.
#' @return A plotly object representing the interactive stacked area chart.
#'
plot_interactive_stacked_area <- function(luc_data_wide,
                                          class_col = names(luc_data_wide)[2],
                                          id_col = names(luc_data_wide)[1],
                                          x_axis_label = "Time Step",
                                          y_axis_label = "Area (Hectares)") 
{
  
  # Step 2: Input Validation and Cleaning
  # Check if the specified columns exist in the data frame
  required_cols <- c(class_col, id_col)
  if (!all(required_cols %in% names(luc_data_wide))) {
    stop(paste("The provided data frame must contain the columns:", paste(required_cols, collapse = ", ")))
  }
  
  # Ensure all data columns (non-ID, non-class) are numeric.
  # This version is more robust: it only attempts to parse columns that are not already numeric.
  clean_data_wide <- luc_data_wide %>%
    dplyr::mutate(across(
      .cols = -all_of(required_cols), 
      .fns = ~ if(!is.numeric(.)){ readr::parse_number(as.character(.))} else .
    )
    )
  
  # Step 3: Prepare Data for Plotting (Reshape and Process)
  luc_data_long <- clean_data_wide %>%
    # Use !!sym() to programmatically refer to the class_col
    dplyr::mutate(!!sym(class_col) := factor(!!sym(class_col), levels = unique(!!sym(class_col)))) %>%
    tidyr::pivot_longer(
      cols = -all_of(required_cols),
      names_to = "Year",
      values_to = "Area"
    ) %>%
    dplyr::mutate(
      Year = readr::parse_number(Year)
    )
  
  # Step 4: Define and Shuffle Tableau 20 Color Palette using ggthemes
  n_colors <- length(unique(luc_data_long[[class_col]]))
  tableau_palette <- ggthemes::tableau_color_pal(palette = "Tableau 20", direction=1)(n_colors)
  
  
  
  # Step 5: Create the Interactive Chart directly with Plotly
  # Build formulas for aesthetics programmatically
  color_formula <- as.formula(paste0("~`", class_col, "`"))
  text_formula <- as.formula(
    paste0("~paste('<b>', `", class_col, "`, '</b><br>', 'Time Step (T+n):', Year, '<br>', 'Area:', scales::comma(Area), ' ha')")
  )
  
  interactive_plot <- plot_ly(
    data = luc_data_long,
    x = ~Year,
    y = ~Area,
    color = color_formula,
    colors = tableau_palette,
    type = 'scatter',
    mode = 'lines',
    stackgroup = 'one',
    line = list(width = 0),
    hoverinfo = 'text',
    text = text_formula
  ) %>%
    layout(
      xaxis = list(title = x_axis_label, dtick = 1),
      yaxis = list(title = y_axis_label),
      legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = 'center')
    )
  
  # Step 6: Return the Interactive Plotly Object
  return(interactive_plot)
}

#' Calculate Land Cover Frequency for Entire Landscape or Planning Units
#'
#' This function calculates the frequency (area or pixel count) of land cover classes either for 
#' the entire landscape or within individual planning units (PUs). It can process multiple 
#' time points and automatically converts pixel counts to hectares when appropriate.
#'
#' @param lc_dir Character. Path to directory containing land cover raster files (TIFF format).
#' @param PU Character. Whether to calculate by planning units ("YES") or for entire landscape ("NO"). 
#'           Default is "NO" (case-insensitive).
#' @param zone SpatRaster or NULL. Planning unit raster (required when PU = "YES"). 
#'             Should have the same extent and resolution as land cover rasters.
#'
#' @return A tibble containing land cover frequencies:
#' \itemize{
#'   \item When PU = "NO": Returns tibble with columns Landcover, T+1, T+2, etc. showing frequencies
#'   \item When PU = "YES": Returns tibble with columns PU, Landcover, T+1, T+2, etc. showing frequencies per PU
#' }
#' Values represent either pixel counts or area in hectares (when CRS units are meters).
#'
#' @details 
#' The function:
#' \itemize{
#'   \item Automatically reads all TIFF files in \code{lc_dir}
#'   \item Processes each time point sequentially (T+1, T+2, etc. based on filenames)
#'   \item For PU calculations, requires zone raster with PU IDs
#'   \item Converts pixel counts to hectares when CRS uses meter units
#'   \item Returns results in tidy tibble format
#' }
#'
#' @note 
#' \itemize{
#'   \item Land cover rasters should be categorical with proper legends
#'   \item Files should follow naming convention that includes landscape numbers (e.g., "landscape1.tif")
#'   \item When PU="YES", zone raster must have PU IDs in its attribute table
#' }
#'
#' @examples
#' \dontrun{
#' # For entire landscape
#' lc_freq <- multiple_lc_freq_combined("path/to/landcover/files")
#' 
#' # For planning units
#' pu_freq <- multiple_lc_freq_combined("path/to/landcover/files", 
#'                                    PU = "YES", 
#'                                    zone = pu_raster)
#' }
#' 
#' @importFrom terra rast
#' @importFrom dplyr arrange mutate across select everything rename relocate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble as_tibble
#' @export
#' 
#' @param split Character. Whether to split results by planning unit when PU="YES" ("YES" or "NO"). 
#'              Default is "NO" (case-insensitive). Only applicable when PU="YES".
#'              
multiple_lc_freq_combined <- function(lc_dir, df_lc, PU = "NO", zone = NULL, split = "NO") {
  
  # Read raster data
  list_luc <- lc_dir %>% list.files(full.names=TRUE, pattern="*.tif$")
  rst_list <- list()
  counter <- 0
  
  for(i in list_luc){
    r <- i %>% rast() %>% add_legend_to_categorical_raster(., lookup_table = df_lc)
    rst_list[[i]] <- r
    counter <- counter + 1
    r %>% plot_categorical_raster()
  }
  
  # Conditional scripts
  if (toupper(PU) == "YES") {
    if (is.null(zone)) {
      stop("Zone parameter must be provided when PU = YES")
    }
    
    # Count freq for each planning unit
    all_freq_pu <- lapply(seq_along(rst_list), function(i) {
      result <- terra::crosstab(c(rst_list[[i]], zone))
      names(result) <- names(rst_list)[i]
      return(result)
    })
    
    df <- as.data.frame(all_freq_pu)
    pu_names <- names(zone)
    
    # Extract landscape numbers from filenames
    landscape_numbers <- gsub(".*landscape(\\d+)\\.tif", "\\1", names(rst_list))
    year_labels <- landscape_numbers
    base_pattern <- "layer_0" # refer to naming format of simulated landcover raster files
    year_data_list <- list()
    
    # Extract data for each landscape
    for (i in seq_along(landscape_numbers)) {
      landscape_num <- landscape_numbers[i]
      year_label <- year_labels[i]
      
      # Determine the corresponding columns in the data frame
      if (i == 1) {
        # For first landscape
        landcover_col <- base_pattern 
        pu_col <- pu_names           
        freq_col <- "Freq"        
      } else {
        # For subsequent landscapes
        landcover_col <- paste0(base_pattern, ".", i-1)  
        pu_col <- paste0(pu_names, ".", i-1)           
        freq_col <- paste0("Freq.", i-1)  
      }
      
      # Check if columns exist in the data frame
      required_cols <- c(landcover_col, pu_col, freq_col)
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols) > 0) {
        stop("Columns not found in data frame: ", paste(missing_cols, collapse = ", "),
             "\nAvailable columns: ", paste(names(df), collapse = ", "))
      }
      
      # Extract the data for this landscape
      temp_data <- df[, c(landcover_col, pu_col, freq_col)]
      names(temp_data) <- c("landcover", "PU", "value")
      temp_data$year <- year_label
      
      year_data_list[[year_label]] <- temp_data
    }
    
    combined_data <- do.call(rbind, year_data_list)
    final_data <- tidyr::pivot_wider(combined_data,
                                     names_from = year, 
                                     values_from = value) %>%
      dplyr::arrange(PU, landcover) %>%
      dplyr::select(PU, landcover, dplyr::everything())
    
    lc_ref <- rst_list[[1]]
    
    if (grepl("\\+units=m", st_crs(lc_ref)$proj4string)) {
      spatRes <- calc_res_conv_factor_to_ha(lc_ref)
      final_data <- mutate(final_data, across(c(3:ncol(final_data)), ~(spatRes*.x)))
    } else {
      cat("Frequency is shown in number of pixels instead of hectares")
    }
    
    colnames(final_data) <- c("PU", "Landcover", paste0("T+", 1:(ncol(final_data)-1)))
    
    # Split the data by PU and convert to list of tibbles
    if (toupper(split) == "YES") {
      pu_list <- final_data %>%
        dplyr::group_split(PU) %>%
        purrr::map(~ dplyr::select(., -PU)) 
      
      pu_names <- unique(final_data$PU)
      names(pu_list) <- paste0("PU_", pu_names)
      
      return(pu_list)
    } else {
      return(final_data)
    }
    
  } else {
    
    # Count freq
    freq_data <- calc_lc_freq(raster_list = rst_list) #%>%
      # rename("Land Cover/Types" = 2) %>% 
      # abbreviate_by_column("Land Cover/Types", remove_vowels = FALSE)
    
    # Convert to long format
    landscape_numbers <- gsub(".*landscape(\\d+)\\.tif", "\\1", names(rst_list))
    time_points <- paste0("T+", seq_along(landscape_numbers))
    colnames(freq_data) <- c("Landcover", time_points)
    
    # Convert to tibble
    freq_tbl <- freq_data %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(
        cols = -Landcover,
        names_to = "Time",
        values_to = "value"
      ) %>%
      dplyr::arrange(Landcover, Time) %>%
      tidyr::pivot_wider(
        names_from = Time,
        values_from = value
      )
    
    if (grepl("\\+units=m", st_crs(rst_list[[1]])$proj4string)) {
      spatRes <- calc_res_conv_factor_to_ha(rst_list[[1]])
      freq_tbl <- freq_tbl %>%
        dplyr::mutate(dplyr::across(-Landcover, ~ .x * spatRes))
    } else {
      cat("Frequency is shown in number of pixels instead of hectares")
    }
    
    return(freq_tbl)
  }
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
    fill_scale <- scale_fill_manual(values = c(
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
    ), na.value = "white")
  }
  if(!is.na(time(raster_object))) {
    plot_title <- time(raster_object)
  } else {
    plot_title <- names(raster_object)
  }
  # Generate the plot
  plot_lc <- ggplot() +
    tidyterra::geom_spatraster(data = raster_object) +
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
  freq_df <- dplyr::rename(freq_df, `Land-use/cover types` = value)
  
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
