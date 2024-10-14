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


executeDINAMICA <- function(params) {
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
  command<-paste('"', dinamica_exe, '" -processors 0 -log-level 4 "', params$egoml, '"', sep="")
  
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
                                    egoml) {
  prob_path <- paste0(output_dir, "/probabilities.tif")
  landscape_path <- paste0(output_dir, "/landscape.tif")
  
  start <- as.numeric(lusim_lc[1, 1])
  length <- as.numeric(nrow(lusim_lc))
  end <- as.numeric(lusim_lc[length, 1])
  
  skeleton1 <- data.frame(nT1 = c(start:end), divider = length)
  skeleton1 <- expandRows(skeleton1, 'divider')
  skeleton2 <- data.frame(nT2 = rep(rep(c(start:end), length)))
  
  skeleton <- cbind(skeleton1, skeleton2)
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

run_dinamica_simulation <- function(dinamica_path = NULL, output_dir, egoml) {
  params <- list()
  params$dinamica_path <- dinamica_path
  params$output_dir <- output_dir
  params$egoml <- egoml
  
  executeDINAMICA(params)
  
  # check result
  new_lc_file <- paste0(output_dir, "/landscape01.tif")
  if (!file.exists(new_lc_file)) {
    stop("Land use change simulation failed! Check DINAMICA EGO log.")
  }
}

run_sciendo_simulate_process <- function(lc_t1_path, lc_lookup_table_path, lc_lookup_table, zone_path, ers_path, 
                                         n_rep, tm_path, dcf_path, dinamica_path = NULL, output_dir, progress_callback = NULL) {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (!is.null(progress_callback)) progress_callback(0.3, "generate egoml: initialize simulation per region parameters")
  out_sim <- generate_egoml_simulate(lc_t1_path, lc_lookup_table, 
                                     zone_path, ers_path, n_rep,
                                     tm_path, dcf_path, output_dir, 
                                     probability = FALSE, egoml = "03_sciendo_simulation")
  
  if (!is.null(progress_callback)) progress_callback(0.7, "run dinamica simulation per region")
  run_dinamica_simulation(dinamica_path, output_dir, out_sim$egoml_sim_file)
  
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
