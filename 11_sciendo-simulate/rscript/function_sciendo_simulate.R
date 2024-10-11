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
  ers_file <- paste0('"', output_dir, '/sciendo_factor.ers"') 
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

run_dinamica_raster_cube <- function(dinamica_path = NULL, output_dir, egoml) {
  params <- list()
  params$dinamica_path <- dinamica_path
  params$output_dir <- output_dir
  params$egoml <- egoml
  
  
  executeDINAMICA(params)
  
  # check .ers file 
  ers_file <- paste0(output_dir, "/sciendo_factor.ers")
  if (!file.exists(ers_file)) {
    stop("Raster cube creation failed! Check DINAMICA EGO log.")
  }
}

generate_egoml_woe_model <- function(aliasFactor, lusim_lc, 
                                     lc1_path, lc2_path, 
                                     zone_path, ers_path,
                                     output_dir, egoml) {
  dcf_path <- paste0(output_dir, "/woe.dcf")
  weight_report_path <- paste0(output_dir, "/weight_report.csv")
  
  static_var <- aliasFactor %>% 
    data.frame(aliasFactor = .) %>% 
    mutate(
      identifier = paste0('&quot;static_var/', aliasFactor, '&quot; 10 500000 1 5,&#x0A;')
    )
  identifier <- do.call(paste, c(as.list(static_var$identifier), sep="        "))
  
  start <- as.numeric(lusim_lc[1, 1])
  length <- as.numeric(nrow(lusim_lc))
  end <- as.numeric(lusim_lc[length, 1])
  
  skeleton1 <- data.frame(nT1=c(start:end), divider=length)
  skeleton1 <- expandRows(skeleton1, 'divider')
  skeleton2 <- data.frame(nT2=rep(rep(c(start:end), length)))
  
  skeleton <- cbind(skeleton1, skeleton2)
  skeleton$key <- do.call(paste, c(skeleton[c("nT1", "nT2")], sep = "-&gt;"))
  
  skeleton$transition <- paste("&#x0A;    ", skeleton$key, " [&#x0A;        ", identifier, "    ]", sep='')
  
  skeletonFinal <- do.call(paste, c(as.list(skeleton$transition), sep=","))
  skeletonFinal <- paste('[', skeletonFinal, "&#x0A;]", sep='')
  
  # begin writing tag
  con <- xmlOutputDOM(tag="script")
  # add property
  con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-18 12:59:40"))
  con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))
  
  # begin.
  # add functor = LoadCategoricalMap
  con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape"))
  con$addTag("inputport", attrs=c(name="filename"), paste0('"', lc2_path, '"'))
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
  con$addTag("inputport", attrs=c(name="filename"), paste0('"', lc1_path, '"'))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), "0")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id="v2"))
  con$closeTag("functor")
  # end.
  
  # begin.
  # add functor = LoadMap
  con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables"))
  con$addTag("inputport", attrs=c(name="filename"), ers_path)
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), "0")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id="v3"))
  con$closeTag("functor") 
  # end.
  
  # begin.
  # add functor = LoadCategoricalMap
  con$addTag("functor", attrs=c(name="LoadCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Regions"))
  con$addTag("inputport", attrs=c(name="filename"), paste0('"', zone_path, '"'))
  con$addTag("inputport", attrs=c(name="nullValue"), ".none")
  con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
  con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
  con$addTag("inputport", attrs=c(name="step"), "0")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$addTag("outputport", attrs=c(name="map", id="v4"))
  con$closeTag("functor")
  # end.
  # begin.
  
  # begin.
  # add containerfunctor = ForEachRegion
  con$addTag("containerfunctor", attrs=c(name="ForEachRegion"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="forEachRegion"))
  con$addTag("inputport", attrs=c(name="regions", peerid="v4"))
  con$addTag("inputport", attrs=c(name="borderCells"), 0)
  con$addTag("internaloutputport", attrs=c(name="regionManager", id="v5"))
  con$addTag("internaloutputport", attrs=c(name="step", id="v6"))
  
  # add subtag functor for SaveWeights
  con$addTag("functor", attrs=c(name="SaveWeights"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="saveWeights"))
  con$addTag("inputport", attrs=c(name="weights", peerid="v10"))
  con$addTag("inputport", attrs=c(name="filename"), paste0('"', dcf_path, '"'))
  con$addTag("inputport", attrs=c(name="suffixDigits"), 6)
  con$addTag("inputport", attrs=c(name="step", peerid="v6"))
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$closeTag("functor")
  
  # add subtag functor for RegionalizeCategoricalMap
  con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Final Landscape (Region)"))
  con$addTag("inputport", attrs=c(name="globalMap", peerid="v1"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v6"))
  con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v5"))
  con$addTag("outputport", attrs=c(name="regionalMap", id="v7"))
  con$closeTag("functor")
  
  # add subtag functor for RegionalizeCategoricalMap
  con$addTag("functor", attrs=c(name="RegionalizeCategoricalMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Initial Landscape (Region)"))
  con$addTag("inputport", attrs=c(name="globalMap", peerid="v2"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v6"))
  con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v5"))
  con$addTag("outputport", attrs=c(name="regionalMap", id="v8"))
  con$closeTag("functor")
  
  # add subtag functor for RegionalizeMap
  con$addTag("functor", attrs=c(name="RegionalizeMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Static Variables (Region)"))
  con$addTag("inputport", attrs=c(name="globalMap", peerid="v3"))
  con$addTag("inputport", attrs=c(name="regionId", peerid="v6"))
  con$addTag("inputport", attrs=c(name="keepNonRegionCells"), ".no")
  con$addTag("inputport", attrs=c(name="regionManager", peerid="v5"))
  con$addTag("outputport", attrs=c(name="regionalMap", id="v9"))
  con$closeTag("functor")
  
  # add subtag functor for SaveTable
  con$addTag("functor", attrs=c(name="SaveTable"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="saveTable"))
  con$addTag("inputport", attrs=c(name="table", peerid="v11"))
  con$addTag("inputport", attrs=c(name="filename"), paste0('"', weight_report_path, '"'))
  con$addTag("inputport", attrs=c(name="suffixDigits"), 2)
  con$addTag("inputport", attrs=c(name="step", peerid="v6"))
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$closeTag("functor")
  
  # add subtag functor for DetermineWeightsOfEvidenceCoefficients
  con$addTag("containerfunctor", attrs=c(name="DetermineWeightsOfEvidenceCoefficients"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Weight of Evidence Coefficients"))
  con$addTag("inputport", attrs=c(name="initialLandscape", peerid="v8"))
  con$addTag("inputport", attrs=c(name="finalLandscape", peerid="v7"))
  con$addTag("inputport", attrs=c(name="ranges", peerid="v12"))
  con$addTag("inputport", attrs=c(name="fixAbnormalWeights"), ".no")
  con$addTag("outputport", attrs=c(name="weights", id="v10"))
  con$addTag("outputport", attrs=c(name="report", id="v11"))
  
  con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMapCoeff"))
  con$addTag("inputport", attrs=c(name="map", peerid="v9"))
  con$addTag("inputport", attrs=c(name="mapName"), '"static_var"')
  con$closeTag("functor")
  
  con$closeTag("containerfunctor")  
  
  # add subtag functor for DetermineWeightsOfEvidenceRanges
  con$addTag("containerfunctor", attrs=c(name="DetermineWeightsOfEvidenceRanges"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="Weight of Evidence Ranges"))
  con$addTag("inputport", attrs=c(name="initialLandscape", peerid="v8"))
  con$addTag("inputport", attrs=c(name="finalLandscape", peerid="v7"))
  con$addTag("inputport", attrs=c(name="skeleton"), skeletonFinal)
  con$addTag("inputport", attrs=c(name="fixAbnormalWeights"), ".no")
  con$addTag("outputport", attrs=c(name="ranges", id="v12"))
  
  con$addTag("functor", attrs=c(name="NameMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="nameMapRanges"))
  con$addTag("inputport", attrs=c(name="map", peerid="v9"))
  con$addTag("inputport", attrs=c(name="mapName"), '"static_var"')
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
    dcf = dcf_path,
    weight = weight_report_path,
    lc1 = lc1_path,
    lc2 = lc2_path,
    zone = zone_path,
    ers = ers_path
  )
  
  return(out)
}

run_dinamica_woe_model <- function(dinamica_path = NULL, output_dir, egoml){
  params <- list()
  params$dinamica_path <- dinamica_path
  params$output_dir <- output_dir
  params$egoml <- egoml
  
  executeDINAMICA(params)
  
  # check .ers file 
  ers_file <- paste0(output_dir, "/sciendo_factor.ers")
  n_woe_report <- output_dir %>% 
    list.files(full.names=TRUE, pattern="weight_report*") %>%
  length()
  if (n_woe_report == 0) {
    stop("There are no single one of WoE Report! Check DINAMICA EGO log.")
  }
}

run_sciendo_simulate_process <- function(lc_t1_path, lc_t2_path, zone_path, lc_lookup_table_path,
                                         lc_lookup_table, factor_path, time_points,
                                         dinamica_path = NULL, output_dir, progress_callback = NULL) {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (!is.null(progress_callback)) progress_callback(0.7, "generate egoml: initialize weight of evidence parameters")
  out_woe <- generate_egoml_woe_model(out_rc$alias, lc_lookup_table, 
                                      lc_t1_path, lc_t2_path, zone_path, 
                                      out_rc$ers, output_dir, 
                                      egoml = "02_sciendo_train_woe")
  
  if (!is.null(progress_callback)) progress_callback(0.9, "run dinamica determine weight of evidence")
  run_dinamica_woe_model(dinamica_path, output_dir, out_woe$egoml_woe_file)
  
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  session_log <- format_session_info_table()
  
  out <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    inputs = list(
      lc_t1_path = lc_t1_path,
      lc_t2_path = lc_t2_path,
      zone_path = zone_path,
      lc_lookup_table_path = lc_lookup_table_path,
      factor_path = factor_path,
      year1 = time_points$t1,
      year2 = time_points$t2,
      output_dir = output_dir
    ),
    rc_path = out_rc$ers,
    rc_egoml_path = out_rc$egoml_rc_path,
    woe_egoml_path = out_woe$egoml_woe_path,
    session_log = session_log
  )
  
  if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")
  
  if (!is.null(progress_callback)) progress_callback(1, "generate report")
  generate_sciendo_simulate_report(output = out, dir = output_dir)
  
  return(out)
}