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
    "../report_template/sciendo_simulate_report_template_INA.Rmd",
    output_file = output_file,
    output_dir = dir,
    params = report_params
  )
}


executeDINAMICA <- function(params, memory_allocation) {
  # Find DINAMICA directory if not provided
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

run_sciendo_simulate_process <- function(lc_t1_path, initial_year, period_value, lc_lookup_table_path, lc_lookup_table, zone_lookup_table, zone_path, ers_path, 
                                         n_rep, tm_path, dcf_path, dinamica_path = NULL, output_dir, memory_allocation, progress_callback = NULL) {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Conditional state to select tpm_path
  files <- list.files(
    path = tm_path,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  xlsm_files <- files[grep("\\.xlsm$", files, ignore.case = TRUE)]
  
  # Conditional report
  if (length(xlsm_files) > 0) {
    # Convert .xlsm files into long data format
    matrix_to_tpm(tm_path, lc_lookup_table, output_dir)
    tm_path <- file.path(output_dir, "scenario_tpm")
  } else {
    message("No .xlsm files found. Using non-macro TPM")
  }
  
  if (!is.null(progress_callback)) progress_callback(0.3, "generate egoml: initialize simulation per region parameters")
  out_sim <- generate_egoml_simulate(lc_t1_path, lc_lookup_table, 
                                     zone_path, ers_path, n_rep,
                                     tm_path, dcf_path, output_dir, 
                                     probability = FALSE, egoml = "03_sciendo_simulation")
  
  if (!is.null(progress_callback)) progress_callback(0.7, "run dinamica simulation per region")
  run_dinamica_simulation(dinamica_path, output_dir, out_sim$egoml_sim_file, memory_allocation)
  
  # rename projected landscape files
  rename_landscape(output_dir, initial_year, period_value)
  
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
#' @param df_lc Data frame. Lookup table for land cover classes with legends.
#' @param PU Character. Whether to calculate by planning units ("YES") or for entire landscape ("NO"). 
#'           Default is "NO" (case-insensitive).
#' @param zone SpatRaster or NULL. Planning unit raster (required when PU = "YES"). 
#'             Should have the same extent and resolution as land cover rasters.
#' @param split Character. Whether to split results by planning unit when PU="YES" ("YES" or "NO"). 
#'              Default is "NO" (case-insensitive). Only applicable when PU="YES".
#'
#' @return A tibble containing land cover frequencies:
#' \itemize{
#'   \item When PU = "NO": Returns tibble with columns Landcover, Year1, Year2, etc. showing frequencies
#'   \item When PU = "YES": Returns tibble with columns PU, Landcover, Year1, Year2, etc. showing frequencies per PU
#' }
#' Values represent either pixel counts or area in hectares (when CRS units are meters).
#'
#' @details 
#' The function:
#' \itemize{
#'   \item Automatically reads all TIFF files in \code{lc_dir} matching pattern "landscape\\d{4}\\.tif$"
#'   \item Processes each time point by year (extracted from filenames)
#'   \item For PU calculations, requires zone raster with PU IDs
#'   \item Converts pixel counts to hectares when CRS uses meter units
#'   \item Returns results in tidy tibble format
#' }
#'
#' @note 
#' \itemize{
#'   \item Land cover rasters should be categorical with proper legends
#'   \item Files should follow naming convention "landscapeYYYY.tif" (e.g., "landscape2025.tif")
#'   \item When PU="YES", zone raster must have PU IDs in its attribute table
#' }
#'
#' @examples
#' \dontrun{
#' # For entire landscape
#' lc_freq <- multiple_lc_freq_combined("path/to/landcover/files", df_lc)
#' 
#' # For planning units
#' pu_freq <- multiple_lc_freq_combined("path/to/landcover/files", df_lc,
#'                                    PU = "YES", 
#'                                    zone = pu_raster)
#' }
#' 
#' @importFrom terra rast
#' @importFrom dplyr arrange mutate across select everything rename relocate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom stringr str_extract
#' @export
multiple_lc_freq_combined <- function(lc_dir, df_lc, PU = "NO", zone = NULL, split = "NO") {
  
  # Read raster data 
  list_luc <- lc_dir %>% 
    list.files(full.names = TRUE, pattern = "^landscape\\d{4}\\.tif$")
  
  # Sort files by year (extracted from filename)
  list_luc <- list_luc[order(as.numeric(stringr::str_extract(basename(list_luc), "\\d{4}")))]
  
  rst_list <- list()
  years <- stringr::str_extract(basename(list_luc), "\\d{4}")
  
  for(i in seq_along(list_luc)){
    r <- list_luc[i] %>% rast() %>% add_legend_to_categorical_raster(., lookup_table = df_lc)
    names(r) <- paste0("landscape_", years[i])
    rst_list[[i]] <- r
    r %>% plot_categorical_raster()
  }
  
  # Conditional scripts
  if (toupper(PU) == "YES") {
    if (is.null(zone)) {
      stop("Zone parameter must be provided when PU = YES")
    }
    
    # Process each year separately and combine
    year_data_list <- list()
    
    for (i in seq_along(rst_list)) {
      year <- years[i]
      raster_obj <- rst_list[[i]]
      
      freq_table <- terra::crosstab(c(raster_obj, zone))
      
      freq_df <- as.data.frame(freq_table)
      colnames(freq_df) <- c("landcover", "PU", "value")
      freq_df$year <- year
      
      year_data_list[[year]] <- freq_df
    }
    
    combined_data <- do.call(rbind, year_data_list)
    
    # Pivot to wide format
    final_data <- tidyr::pivot_wider(combined_data,
                                     names_from = year, 
                                     values_from = value,
                                     values_fill = 0) %>%
      dplyr::arrange(PU, landcover) %>%
      dplyr::select(PU, landcover, dplyr::everything())
    
    lc_ref <- rst_list[[1]]
    
    if (grepl("\\+units=m", st_crs(lc_ref)$proj4string)) {
      spatRes <- calc_res_conv_factor_to_ha(lc_ref)
      final_data <- mutate(final_data, across(c(3:ncol(final_data)), ~(spatRes*.x)))
    } else {
      cat("Frequency is shown in number of pixels instead of hectares")
    }
    
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
    
    # Count freq for entire landscape
    freq_data <- calc_lc_freq(raster_list = rst_list)
    
    # Use actual years for column names
    if (ncol(freq_data) == length(years) + 1) {
      colnames(freq_data) <- c("Landcover", years)
    }
    
    # Convert to tibble
    freq_tbl <- freq_data %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(
        cols = -Landcover,
        names_to = "Year",
        values_to = "value"
      ) %>%
      dplyr::arrange(Landcover, Year) %>%
      tidyr::pivot_wider(
        names_from = Year,
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
plot_categorical_raster <- function(raster_object, filename = "raster_plot.png", dpi = 300) {
  # Color palette
  if ("color_palette" %in% names(cats(raster_object)[[1]]) &&
      all(grepl("^#[0-9A-Fa-f]{6}$", cats(raster_object)$color_pallete))) {
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
  
  if (!is.na(time(raster_object))) {
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
    guides(fill = guide_legend(title.position = "top", ncol = 2)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.key.height = unit(0.25, "cm"),
      legend.key.width = unit(0.25, "cm"),
      legend.position = "bottom",
      legend.justification = c(0, 0.8)
    )
  
  if (!knitr::is_html_output()) {
    return(plot_lc)
  }
  
  # Save PNG with custom dpi
  tf <- tempfile(fileext = ".png")
  ggsave(tf, plot_lc, width = 7, height = 5, dpi = dpi)
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

#' Convert Macro's Format Transition Probability Matrices (TPM) to Long Data CSV Format
#'
#' Processes Excel (.xlsm) files containing transition probability matrices, 
#' cleans the data, converts land cover names to IDs, and saves them as 
#' standardized CSV files in long format.
#'
#' @param input_folder_path Character. Path to the folder containing .xlsm files.
#' @param lc_lookup Data frame. Lookup table with columns `ID` (numeric) and `LC` (land cover names).
#' @param output_dir Character. Directory where output CSV files will be saved (subfolder `/scenario_tpm` will be created).
#'
#' @return Invisibly returns a list of processed file paths. Side effect: Saves cleaned CSV files to `output_dir/scenario_tpm/`.
#'
#' @details
#' - Skips empty datasets and files with errors (logs messages).
#' - Removes totals (last row/column) and zero-rate transitions.
#' - Converts land cover names to IDs using fuzzy matching (case-insensitive, whitespace-trimmed).
#' - Output CSV columns: `From*`, `To*`, `Rate`.
#'
#' @examples
#' \dontrun{
#' lc_lookup <- data.frame(
#'   ID = 1:3,
#'   LC = c("Forest", "Urban", "Cropland")
#' )
#' matrix_to_tpm(
#'   input_folder_path = "path/to/xlsm_files",
#'   lc_lookup = lc_lookup,
#'   output_dir = "output"
#' )
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate rename filter
#' @importFrom tidyr pivot_longer
#' @importFrom tools file_path_sans_ext
#' @export
matrix_to_tpm <- function(input_folder_path, lc_lookup, output_dir) {
  # Create output directory if it doesn't exist
  tpm_dir <- file.path(output_dir, "scenario_tpm")
  dir.create(tpm_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Get list of files
  xlsm_files <- list.files(
    path = input_folder_path,
    pattern = "\\.xlsm$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Early return if no files found
  if (length(xlsm_files) == 0) {
    message("No .xlsm files found in: ", input_folder_path)
    return(invisible(NULL))
  }
  
  # Function to clean and match land cover names to IDs
  convert_to_id <- function(data, lc_lookup) {
    clean_text <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))
    
    lc_lookup_clean <- lc_lookup %>%
      mutate(LC_clean = clean_text(LC))
    
    # Get row names (first column)
    row_names <- clean_text(data[[1]])
    row_ids <- lc_lookup_clean$ID[match(row_names, lc_lookup_clean$LC_clean)]
    
    # Get column names (excluding first column)
    col_names <- clean_text(colnames(data)[-1])
    col_ids <- lc_lookup_clean$ID[match(col_names, lc_lookup_clean$LC_clean)]
    
    list(row_ids = row_ids, col_ids = col_ids)
  }
  
  processed_files <- character(0)
  
  # Process each file
  for (xlsm_path in xlsm_files) {
    message("\nProcessing: ", basename(xlsm_path))
    
    tryCatch({
      # Read the data
      data <- read_excel(xlsm_path, sheet = 1, col_names = TRUE) %>% 
        as.data.frame()
      
      # Skip if empty
      if (nrow(data) == 0) {
        message("Empty dataset in file: ", basename(xlsm_path))
        next
      }
      
      # Convert to IDs
      ids <- convert_to_id(data, lc_lookup)
      
      # Replace with IDs
      data[[1]] <- ids$row_ids
      colnames(data)[-1] <- ids$col_ids
      
      # Remove only the last column (typically totals column), keep all rows
      data <- data[, -ncol(data), drop = FALSE]
      
      # Convert to long format
      long_data <- data %>%
        pivot_longer(
          cols = -1,
          names_to = "To*",
          values_to = "Rate",
          values_transform = list(Rate = as.numeric)
        ) %>%
        rename("From*" = 1) %>%
        filter(
          `From*` != `To*`,
          !is.na(Rate),
          Rate != 0,
          !is.na(`From*`),
          !is.na(`To*`)
        )
      
      clean_name <- gsub("_macros", "", basename(xlsm_path))
      out_file <- file.path(tpm_dir, sub("\\.xlsm$", ".csv", clean_name))
      write.csv(long_data, out_file, row.names = FALSE, quote = FALSE)
      processed_files <- c(processed_files, out_file)
      
    }, error = function(e) {
      message("Error processing ", basename(xlsm_path), ": ", e$message)
    })
  }
  
  invisible(processed_files)
}

#' Rename landscape raster files with year-based naming convention
#'
#' This function automates the renaming of landscape raster files from a numeric
#' sequence pattern (landscape01.tif, landscape02.tif, etc.) to a year-based
#' naming convention using an initial year and period value. It also removes
#' any .tif files that don't match the expected landscape pattern.
#'
#' @param folder_path Character string. The path to the directory containing
#'   the raster files to be renamed.
#' @param initial_year Integer. The base year from which to calculate target years.
#'   For example, if initial_year = 2020, landscape01.tif will correspond to 2025
#'   when period_value = 5.
#' @param period_value Integer. The number of years between each landscape raster.
#'   This value is multiplied by the file number to calculate the target year.
#'
#' @return Invisible NULL. The function primarily produces side effects by renaming
#'   files and updating raster properties. Progress messages are printed to the console.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Finds all .tif files matching the pattern "landscape\\d+.tif"
#'   \item Identifies and removes any .tif files that don't match the expected pattern
#'   \item Sorts files numerically by their embedded number
#'   \item Calculates target years using: target_year = initial_year + (file_number * period_value)
#'   \item Renames both the physical filename and the layer name property of the SpatRaster
#'   \item Saves the modified rasters and removes the original files
#' }
#'
#' @note
#' The function will overwrite existing files if the target filename already exists.
#' Make sure to backup your data before running this function.
#'
#' @examples
#' \dontrun{
#' # Rename landscape rasters starting from 2020 with 5-year intervals
#' rename_landscape("path/to/raster/folder", 2020, 5)
#'
#' # Example output:
#' # landscape01.tif -> landscape2025.tif (layer name: landscape_2025)
#' # landscape02.tif -> landscape2030.tif (layer name: landscape_2030)
#' # landscape03.tif -> landscape2035.tif (layer name: landscape_2035)
#' # Removed: other_file.tif (does not match landscape pattern)
#' }
#'
#' @export
#' @importFrom terra rast writeRaster
#' @importFrom stringr str_extract
rename_landscape <- function(folder_path, initial_year, period_value) {
  # List all .tif files in the directory
  all_tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(all_tif_files) == 0) {
    stop("No .tif files found in the specified directory.")
  }
  
  # Identify files that match the landscape pattern 
  landscape_files <- all_tif_files[grepl("^landscape\\d{2}\\.tif$", basename(all_tif_files))]
  
  # List all .xml files in the directory (if any exist)
  xml_files <- list.files(folder_path, pattern = "\\.xml$", full.names = TRUE)
  
  # Remove all .xml files (if any exist)
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
  
  # Identify files that don't match the pattern
  non_landscape_files <- all_tif_files[!grepl("^landscape\\d{2}\\.tif$", basename(all_tif_files))]
  
  # Remove non-matching files
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
  
  # Extract the numeric part from filenames and sort them
  file_numbers <- as.numeric(stringr::str_extract(basename(landscape_files), "\\d+"))
  sorted_indices <- order(file_numbers)
  landscape_files <- landscape_files[sorted_indices]
  file_numbers <- file_numbers[sorted_indices]
  
  # Process each raster file
  for (i in seq_along(landscape_files)) {
    file_path <- landscape_files[i]
    file_number <- file_numbers[i]
    
    # Calculate the corresponding year
    target_year <- initial_year + (file_number * period_value)
    
    # Create new filename
    new_filename <- paste0("landscape", target_year, ".tif")
    new_file_path <- file.path(folder_path, new_filename)
    
    # Read the raster
    raster_obj <- terra::rast(file_path)
    
    # Rename the layer name (names property)
    names(raster_obj) <- target_year
    
    # Write the raster with new filename and layer name
    terra::writeRaster(raster_obj, filename = new_file_path, overwrite = TRUE)
    
    # Remove the original file
    file.remove(file_path)
    
    cat(sprintf("Renamed: %s -> %s (layer name: %s)\n", 
                basename(file_path), new_filename, names(raster_obj)))
  }
  
  cat(sprintf("\nSuccessfully renamed %d raster files.\n", length(landscape_files)))
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

#' Plot Categorical Raster Map with Interactive Visualization
#'
#' Creates a generic interactive map for any classified raster data using
#' mapview, with proper classification, coloring, and legend.
#'
#' @param cat_raster A SpatRaster object (from terra package) containing
#'   classification values. The raster should contain integer values
#'   corresponding to different categories.
#' @param cat_table A data frame containing the classification scheme.
#'   The **first column** must be the numeric codes (ID) and the
#'   **second column** must be the category names. It may optionally
#'   include a column named 'color_palette' with hex color codes for custom colors.
#' @param yr Character or numeric value representing the year or time period
#'   for the map. Used in the layer name and legend title.
#' @param layer_title A character string for the layer name prefix in the legend
#'   and layer control. Defaults to "Layer".
#'
#' @return A mapview object containing an interactive leaflet map with the
#'   categorical data displayed using the specified colors and including a legend.
#'
#' @details This function performs the following steps:
#' \enumerate{
#'   \item Filters the category table to include only classes present in the raster.
#'   \item Reclassifies raster values to a sequential index for consistent coloring.
#'   \item Converts the raster to a categorical factor with proper labels.
#'   \item Applies a color palette. If a 'color_palette' column exists in `cat_table`,
#'         it will be used. Otherwise, a predefined color set is applied.
#'   \item Creates an interactive map with mapview.
#'   \item Adds a custom legend with category names and colors.
#' }
#'
#' @note The function requires the following packages: terra, mapview, leaflet,
#'   and dplyr (for the pipe operator).
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(terra)
#' library(mapview)
#'
#' # Create example data
#' class_raster <- rast(nrows = 100, ncols = 100, vals = sample(1:3, 10000, replace = TRUE))
#'
#' # Create a table with custom colors
#' class_table <- data.frame(
#'   CODE = 1:3,
#'   CLASS_NAME = c("Class A", "Class B", "Class C"),
#'   color_palette = c("#228B22", "#FF0000", "#FFFF00")
#' )
#'
#' # Create the interactive map
#' cat_map <- plot_categorical_raster_mapview(class_raster, class_table, yr = 2025, layer_title = "Classification")
#' cat_map # Display the map
#' }
#'
#' @importFrom terra values classify as.factor levels<- activeCat coltab ncell
#' @importFrom mapview mapview
#' @importFrom leaflet colorFactor addLegend
#' @importFrom dplyr %>%
#' @export
plot_categorical_raster_mapview <- function(cat_raster, cat_table, layer_title = "Layer") {
  names(cat_table)[1] <- "ID"
  names(cat_table)[2] <- "Category"
  
  cat_table$ID <- as.numeric(cat_table$ID)
  unique_values <- unique(values(cat_raster, na.rm = TRUE))
  cat_tbl_filtered <- cat_table[cat_table$ID %in% unique_values, ]
  
  reclass_from <- cat_tbl_filtered$ID
  reclass_to <- seq_along(cat_tbl_filtered$ID)
  reclass_matrix <- cbind(reclass_from, reclass_to)
  cat_reclass <- classify(cat_raster, reclass_matrix, others = NA)
  cat_factor <- as.factor(cat_reclass)
  
  levels_df <- data.frame(
    ID = reclass_to,
    Category = factor(cat_tbl_filtered$Category, levels = cat_tbl_filtered$Category)
  )
  
  levels(cat_factor) <- levels_df
  activeCat(cat_factor) <- "Category"
  
  predefined_colors <- c(
    "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#e6194B",
    "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9A6324", "#fffac8", "#800000", "#aaffc3",
    "#808000", "#ffd8b1", "#000075", "#808080", "#1F77B4", "#FF7F0E", "#40E0D0", "#6B8E23",
    "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#CD5C5C", "#7B68EE",
    "#17BECF", "#BCBD22", "#FF9896", "#C5B0D5", "#C49C94", "#9C9EDE", "#AEC7E8", "#FFBB78",
    "#98DF8A", "#FF7F50", "#FFD700", "#8B0000", "#20B2AA", "#DA70D6", "#B22222", "#5F9EA0",
    "#ffffff", "#000000"
  )
  
  # Conditionally select color palette
  if ("color_palette" %in% names(cat_tbl_filtered)) {
    map_colors <- cat_tbl_filtered$color_palette
  } else {
    if (nrow(cat_tbl_filtered) > length(predefined_colors)) {
      warning("Not enough predefined colors for all categories. Colors will be recycled.")
    }
    map_colors <- predefined_colors[1:nrow(cat_tbl_filtered)]
  }
  
  color_table <- data.frame(
    value = reclass_to,
    color = map_colors
  )
  
  coltab(cat_factor) <- color_table
  
  map_result <- mapview(
    cat_factor,
    zcol = "Category",
    maxpixels = ncell(cat_factor),
    layer.name = paste(layer_title),
    na.color = "transparent",
    legend = FALSE
  )
  
  pal <- colorFactor(
    palette = color_table$color,
    domain = levels_df$Category
  )
  
  map_result@map <- map_result@map %>%
    addLegend(
      position = "topright",
      pal = pal,
      values = levels_df$Category,
      title = paste(layer_title)
    )
  
  return(map_result)
}