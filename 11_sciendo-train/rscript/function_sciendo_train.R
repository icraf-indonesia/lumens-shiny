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
generate_train_report <- function(output, dir) {
  report_params <- list(
    start_time = output$start_time,
    end_time = output$end_time,
    p1 = output$p1,
    p2 = output$p2,
    inputs = output$inputs,
    session_log = output$session_log
  )
  output_file <- paste0("sciendo_train_report_", Sys.Date(), ".html")
  
  rmarkdown::render(
    "../report_template/sciendo_train_report_template.Rmd",
    output_file = output_file,
    output_dir = dir,
    params = report_params
  )
}

run_train_analysis <- function(lc_t1_path, lc_t2_path, admin_z_path,
                               lc_t1_input, lc_t2_input, admin_z_input,
                               zone_lookup_input,
                               time_points, output_dir, progress_callback = NULL) {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  if (!is.null(progress_callback)) progress_callback(0.2, "load maps")
  
  
  if (!is.null(progress_callback)) progress_callback(0.5, "create QUES-C database")
  
  
  if (!is.null(progress_callback)) progress_callback(0.7, "generate carbon, emission, and sequestration maps")
  
  
  
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  session_log <- format_session_info_table()
  
  out <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    p1 = time_points$t1,
    p2 = time_points$t2,
    inputs = list(
      lc_t1_path = lc_t1_path,
      lc_t2_path = lc_t2_path,
      admin_z_path = admin_z_path,
      output_dir = output_dir
    ),
    session_log = session_log
  )
  
  if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")
  
  if (!is.null(progress_callback)) progress_callback(1, "generate report")
  generate_train_report(output_quesc = out, dir = output_dir)
  
  return(out)
}

executeDINAMICA <- function(params) {
  # Find DINAMICA directory if not provided
  if (is.null(params$dinamica_path)) {
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
  
  egoml <- paste0(params$output_dir, '/', params$egoml)
  # Check if egoml exists
  if (!file.exists(egoml)) {
    stop("Specified egoml does not exist.")
  }
  
  # Prepare DINAMICA command
  command<-paste('"', dinamica_exe, '" -processors 0 -log-level 4 "', egoml, '"', sep="")
  
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
  listFactors <- factors_path %>% list.files(full.names=TRUE, pattern=".tif$") %>%
    data.frame(file=., select=1)
  
  factors <- as.character(listFactors$file)
  nFactors <- length(factors)
  
  aliasFactor<-NULL
  for (a in 1:nFactors) {
    temp <- substr(basename(factors[a]), 1, nchar(basename(factors[a])) - 4)
    aliasFactor <- c(aliasFactor, temp)
  }
  
  # create raster cube egoml
  # begin writing tag
  con <- xmlOutputDOM(tag="script")
  # add property
  con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-17 12:02:15"))
  con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))
  
  # begin.
  # add functor = SaveMap
  con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap1680"))
  con$addTag("inputport", attrs=c(name="map", peerid=paste("v", nFactors+1,sep="")))
  con$addTag("inputport", attrs=c(name="filename"), paste('"', output_dir, '/sciendo_factor.ers"', sep=''))
  con$addTag("inputport", attrs=c(name="suffixDigits"), 2)
  con$addTag("inputport", attrs=c(name="step"), ".none")
  con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
  con$addTag("inputport", attrs=c(name="workdir"), ".none")
  con$closeTag("functor")
  # end.
  
  # begin.
  # add functor = LoadMap
  for (b in 1:nFactors){
    con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
    con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[b]))
    con$addTag("inputport", attrs=c(name="filename"), paste('"', factors[b], '"', sep=""))
    con$addTag("inputport", attrs=c(name="nullValue"), ".none")
    con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
    con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
    con$addTag("inputport", attrs=c(name="step"), ".none")
    con$addTag("inputport", attrs=c(name="workdir"), ".none")
    con$addTag("outputport", attrs=c(name="map", id=paste("v",b,sep="")))
    con$closeTag("functor") 
  }
  # end.
  
  # begin.
  # add containerfunctor = CreateCubeMap
  con$addTag("containerfunctor", attrs=c(name="CreateCubeMap"), close=FALSE)
  con$addTag("property", attrs=c(key="dff.functor.alias", value="createCubeMap1678"))
  con$addTag("inputport", attrs=c(name="cellType"), ".float32")
  con$addTag("inputport", attrs=c(name="nullValue"), "-9999")
  con$addTag("outputport", attrs=c(name="map", id=paste("v", nFactors+1, sep="")))
  # add subtag functor for CreateCubeMap
  for (c in 1:nFactors) {
    con$addTag("functor", attrs=c(name="NumberAndNameMap"), close=FALSE)
    con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[c]))
    con$addTag("inputport", attrs=c(name="map", peerid=paste("v", c, sep="")))
    con$addTag("inputport", attrs=c(name="mapName"), paste('"', aliasFactor[c], '"', sep=""))
    con$addTag("inputport", attrs=c(name="mapNumber"), 0)
    con$closeTag("functor")
  }
  con$closeTag("containerfunctor")
  # end.
  
  saveXML(con$value(), file=paste(output_dir, "/", egoml, sep=''))
}

generate_egoml_woe <- function() {
  
}

generate_egoml_sim <- function() {
  
}

run_raster_cube_generation <- function(dinamica_path, factor_path, output_dir, progress_callback = NULL) {
  if (!is.null(progress_callback)) progress_callback(0.2, "load maps")
  
  generate_egoml_raster_cube(factor_path, output_dir, egoml)
  
  params <- list(
    dinamica_path = dinamica_path,
    output_dir = output_dir,
    egoml = egoml
  )
  executeDINAMICA(params)
}

run_woe_model <- function(){
  generate_egoml_woe()
}