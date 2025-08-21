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

create_list_of_weight_report <- function(woe_report_path, list_woe_report, df_zone, lc_lookup_table) {
  listWoeReport <- list_woe_report
  woe <- list()
  len <- nrow(df_zone)
  
  vec <- c()
  for(j in listWoeReport) {
    number <- gsub(paste0(woe_report_path, "/weight_report"), "", j) %>% substr(1, 2) %>% as.numeric()
    vec <- c(vec, number)
  }
  
  for(counter in 1:length(vec)){
    i <- vec[counter]
    woe[[paste0("pu", sprintf("%03d", i))]][['name']] <- df_zone[i, 2]
    woe[[paste0("pu", sprintf("%03d", i))]][['report']] <- listWoeReport[counter] %>%
      read.csv() %>%
      dplyr::select(-X) %>% 
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
                                      lc_lookup_table, z_lookup_table_path, factor_path, time_points,
                                      dinamica_path = NULL, output_dir, memory_allocation, progress_callback = NULL) {
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
      template_path = "../macro_template/macro_template.xlsm"
    )
  }, error = function(e) {
    stop("TPM conversion failed: ", e$message)
  })
  
  if (!is.null(progress_callback)) progress_callback(0.2, "generate egoml: raster cube generation")
  out_rc <- generate_egoml_raster_cube(factor_path, output_dir, egoml = "01_sciendo_train_raster_cube")
  
  if (!is.null(progress_callback)) progress_callback(0.5, "run dinamica raster cube")
  run_dinamica_raster_cube(dinamica_path, output_dir, out_rc$egoml_rc_file, memory_allocation)
  
  # generate modified xml file
  pu_df <- read.csv(z_lookup_table_path)
  
  add_pu_classes_to_pam(
    output_dir = output_dir,  
    pu_classes = pu_df
  )

  if (!is.null(progress_callback)) progress_callback(0.7, "generate egoml: initialize weight of evidence parameters")
  out_woe <- generate_egoml_woe_model(out_rc$alias, lc_lookup_table, 
                                      lc_t1_path, lc_t2_path, zone_path, 
                                      out_rc$ers, output_dir, 
                                      egoml = "02_sciendo_train_woe")
  
  if (!is.null(progress_callback)) progress_callback(0.9, "run dinamica determine weight of evidence")
  run_dinamica_woe_model(dinamica_path, output_dir, out_woe$egoml_woe_file, memory_allocation)
  
  listWoeReport <- out_woe$weight  %>% list.files(full.names=TRUE, pattern="weight_report*")
  df_pu <- read.csv(z_lookup_table_path) %>% dplyr::rename(ID_PU = 1, PU = 2)
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
      z_lookup_table_path = z_lookup_table_path,
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
  rasters <- rast(tif_files)
  names(rasters) <- gsub(".tif", "", basename(tif_files))
  
  # Sample raster values
  set.seed(seed)
  sample_points <- spatSample(rasters, size = sample_size, na.rm = TRUE)
  sample_df <- as.data.frame(sample_points)
  
  # Calculate VIF
  vif_result <- vifstep(sample_df, th = vif_threshold)
  high_vif_vars <- vif_result@excluded
  vif_table <- vif_result@results 
  
  # Verbose printing
  if (verbose) {
    message("\n VIF Analysis Results (threshold = ", vif_threshold, "):")
    print(vif_table)
    
    if (length(high_vif_vars) > 0) {
      message("\n️ Variables recommended for removal (VIF > ", vif_threshold, "):\n", paste(high_vif_vars, collapse = "\n"))
    } else {
      message("\n No variables exceeded VIF threshold.")
    }
  }
  
  # Correlation matrix and plot
  cor_matrix <- cor(sample_df, use = "complete.obs")
  corrplot(cor_matrix,
           method = "color",
           type = "full",
           order = "hclust",
           tl.cex = 0.7,
           tl.col = "black",
           mar = c(0, 0, 2, 0),
           addCoef.col = "black",
           number.cex = 0.6,
           diag = TRUE)
  
  corr_plot <- recordPlot()
  
  return(list(
    vif_result = vif_result,
    vif_table = vif_table,
    excluded_vars = high_vif_vars,
    correlation_matrix = cor_matrix,
    correlation_plot = corr_plot
  ))
}

#' Add Planning Unit Classes to Raster Metadata File
#' 
#' This function enhances a GDAL PAM (.aux.xml) metadata file by adding Planning Unit (PU)
#' class information while preserving existing raster metadata. The PU classes are added
#' as a separate XML node at the same level as the original PAMDataset content.
#'
#' @param output_dir Character string specifying the directory path containing the 
#'                  'sciendo_factors.tif' file and where the PAM metadata will be saved.
#' @param pu_classes A data frame or matrix containing PU class definitions where:
#'                  \itemize{
#'                    \item First column contains PU IDs (numeric or character)
#'                    \item Second column contains PU class names (character)
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
#' # Add to PAM file (using tempdir() for example)
#' output_path <- tempdir()
#' file.create(paste0(output_path, "/sciendo_factors.tif.aux.xml"))
#' 
#' try(
#'   add_pu_classes_to_pam(
#'     output_dir = output_path,
#'     pu_classes = pu_data
#'   )
#' )
#' }
#'
#' @seealso
#' \code{\link[XML]{xmlParse}} for XML parsing functionality
#'
#' @export
#' @importFrom XML newXMLNode xmlParse xmlRoot xmlChildren saveXML
add_pu_classes_to_pam <- function(output_dir, pu_classes, overwrite = TRUE) {
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
    message("Successfully updated PAM metadata file:\n  ", pam_file)
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
#' }
#'
#' @importFrom openxlsx2 wb_load wb_save wb_add_data wb_add_fill wb_add_font
#' @importFrom openxlsx2 wb_add_border wb_add_numfmt
#' @importFrom tidyr pivot_wider expand_grid
#' @importFrom dplyr select mutate left_join arrange
#' @importFrom tools file_path_sans_ext
#' @export
tpm_to_matrix_conversion <- function(input_dir, lc_lookup_path, output_dir, template_path) {
  # Load required libraries
  if (!require(openxlsx2, quietly = TRUE)) {
    stop("openxlsx2 package is required. Please install it with: install.packages('openxlsx2')")
  }
  if (!require(dplyr, quietly = TRUE)) {
    stop("dplyr package is required. Please install it with: install.packages('dplyr')")
  }
  if (!require(tidyr, quietly = TRUE)) {
    stop("tidyr package is required. Please install it with: install.packages('tidyr')")
  }
  
  # Normalize paths to avoid issues
  output_dir <- normalizePath(output_dir, winslash = "/")
  
  # Ensure output_dir does not already contain "baseline_tpm_macro"
  if (basename(output_dir) == "baseline_tpm_macro") {
    output_dir <- dirname(output_dir)
  }
  
  # Check if template file exists
  if (!file.exists(template_path)) {
    stop("Template file not found: ", template_path)
  }
  
  lc_lookup_table <- read.csv(lc_lookup_path)
  
  macro_dir <- file.path(output_dir, "baseline_tpm_macro")
  dir.create(macro_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("Macros will be saved to: ", macro_dir)
  
  tpm_dinamica <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Process each CSV file
  for (tpm in tpm_dinamica) {
    base_name <- tools::file_path_sans_ext(basename(tpm))
    matrix1 <- read.csv(tpm)
    
    # Prepare the matrix data and get all unique IDs from both From and To columns
    matrix1 <- matrix1 %>%
      dplyr::select(-X) %>%  # drop the unused column
      dplyr::mutate(
        From. = as.numeric(From.),
        To. = as.numeric(To.),
        Rate = as.numeric(Rate)
      )
    
    # Get all unique IDs present in matrix1 (from both From and To columns)
    all_ids <- sort(unique(c(matrix1$From., matrix1$To.)))
    
    # Create a complete grid of all possible combinations
    complete_grid <- expand.grid(
      From. = all_ids,
      To. = all_ids
    ) %>%
      dplyr::left_join(matrix1, by = c("From.", "To.")) %>%
      dplyr::mutate(Rate = ifelse(is.na(Rate), 0, Rate))
    
    # Pivot to wide format
    wide_df <- complete_grid %>%
      tidyr::pivot_wider(
        names_from = To., 
        values_from = Rate,
        values_fill = 0
      ) %>%
      dplyr::arrange(From.)  # sort rows by From.
    
    # Reorder columns numerically
    ordered_cols <- sort(as.numeric(names(wide_df)[-1]))
    wide_df <- wide_df[, c("From.", as.character(ordered_cols))]
    
    # Convert to matrix
    result_matrix <- as.matrix(wide_df[, -1])
    rownames(result_matrix) <- wide_df$From.
    
    # Calculate diagonal values and normalize rows
    for(i in 1:nrow(result_matrix)) {
      # First, set diagonal to 0 temporarily
      result_matrix[i, i] <- 0
      
      # Calculate sum of off-diagonal elements
      row_sum_off_diag <- sum(result_matrix[i, ])
      
      # If off-diagonal sum is >= 1, proportionally scale them down
      if (row_sum_off_diag >= 1) {
        # Scale off-diagonal elements to sum to 0.999999
        if (row_sum_off_diag > 0) {
          result_matrix[i, ] <- result_matrix[i, ] * (0.999999 / row_sum_off_diag)
        }
        result_matrix[i, i] <- 0.000001  # Small persistence probability
      } else {
        # Normal case: set diagonal to make row sum = 1
        result_matrix[i, i] <- 1 - row_sum_off_diag
      }
      
      # Force exact normalization to handle floating point precision
      current_row_sum <- sum(result_matrix[i, ])
      if (current_row_sum > 0) {
        result_matrix[i, ] <- result_matrix[i, ] / current_row_sum
      }
      
      # Final precision check - adjust the largest element if needed
      final_sum <- sum(result_matrix[i, ])
      if (abs(final_sum - 1) > .Machine$double.eps) {
        # Find the largest element and adjust it
        max_idx <- which.max(result_matrix[i, ])
        result_matrix[i, max_idx] <- result_matrix[i, max_idx] + (1 - final_sum)
      }
    }
    
    # Verify that all rows sum to exactly 1 (for debugging)
    row_sums <- rowSums(result_matrix)
    max_deviation <- max(abs(row_sums - 1))
    message("Maximum deviation from 1: ", format(max_deviation, scientific = TRUE))
    
    if (any(abs(row_sums - 1) > 1e-10)) {
      warning("Some rows do not sum to exactly 1 in file: ", basename(tpm))
      problematic_rows <- which(abs(row_sums - 1) > 1e-10)
      for (row_idx in problematic_rows) {
        message("Row ", row_idx, " (", rownames(result_matrix)[row_idx], ") sum: ", 
                format(row_sums[row_idx], digits = 15))
      }
    }
    
    # Apply land cover labels
    row_ids <- as.numeric(rownames(result_matrix))
    row_labels <- lc_lookup_table$LC[match(row_ids, lc_lookup_table$ID)]
    rownames(result_matrix) <- row_labels
    col_ids <- as.numeric(colnames(result_matrix))
    col_labels <- lc_lookup_table$LC[match(col_ids, lc_lookup_table$ID)]
    colnames(result_matrix) <- col_labels
    
    # Convert matrix to data frame for writing
    output_df <- as.data.frame(result_matrix)
    output_df <- cbind(rownames(output_df), output_df)
    colnames(output_df)[1] <- ""
    
    # Define output file path
    output_file <- file.path(macro_dir, paste0(base_name, "_macros.xlsm"))
    
    # Copy template to output location first
    file.copy(template_path, output_file, overwrite = TRUE)
    message("Copied template to: ", output_file)
    
    # Load the workbook using
    tryCatch({
      wb <- wb_load(output_file)
      message("Successfully loaded workbook")
      
      # Get the first sheet name
      sheet_name <- wb$get_sheet_names()[1]
      
      n_rows <- nrow(output_df)
      n_cols <- ncol(output_df)
      row_names <- output_df[,1]
      col_names <- colnames(output_df)[-1] 
      
      # Write the main data starting from A1
      wb <- wb$add_data(
        sheet = sheet_name,
        x = output_df,
        start_col = 1,
        start_row = 1,
        col_names = TRUE,
        row_names = FALSE
      )
      
      # Add "Grand Total" header in the last column of row 1
      wb <- wb$add_data(
        sheet = sheet_name,
        x = "Grand Total",
        start_col = n_cols + 1,
        start_row = 1,
        col_names = FALSE,
        row_names = FALSE
      )
      
      # Calculate and add row sums
      for (i in 1:n_rows) {
        # Calculate actual row sum from the matrix
        actual_row_sum <- sum(as.numeric(output_df[i, -1]))
        wb <- wb$add_data(
          sheet = sheet_name,
          x = actual_row_sum,
          start_col = n_cols + 1,
          start_row = i + 1,
          col_names = FALSE,
          row_names = FALSE
        )
      }
      
      # Add column sums in the last row
      # col_sums <- c("Grand Total", colSums(output_df[, -1]), sum(colSums(output_df[, -1])))
      # wb <- wb$add_data(
      #   sheet = sheet_name,
      #   x = t(col_sums),
      #   start_col = 1,
      #   start_row = n_rows + 2,
      #   col_names = FALSE,
      #   row_names = FALSE
      # )
      
      # Apply formatting
      # Style for sum cells (light blue background)
      sum_fill <- wb$add_fill(fgColor = "lightblue")
      
      # Style for diagonal cells (yellow background)
      diagonal_fill <- wb$add_fill(fgColor = "yellow")
      
      # Apply light blue background to row sum cells (column n_cols+1, rows 2 to n_rows+1)
      wb <- wb$add_fill(
        sheet = sheet_name,
        dims = paste0(openxlsx2::int2col(n_cols + 1), "2:", openxlsx2::int2col(n_cols + 1), n_rows + 1),
        color = wb_color("lightblue")
      )
      
      # Apply light blue background to column sum cells (row n_rows+2, all columns)
      wb <- wb$add_fill(
        sheet = sheet_name,
        dims = paste0("A", n_rows + 2, ":", openxlsx2::int2col(n_cols + 1), n_rows + 2),
        color = wb_color("lightblue")
      )
      
      # Apply yellow highlighting to diagonal cells
      for (i in 1:n_rows) {
        row_name <- row_names[i]
        for (j in 2:n_cols) {
          col_name <- col_names[j-1]
          if (row_name == col_name) {
            # Convert to Excel cell reference
            cell_ref <- paste0(openxlsx2::int2col(j), i + 1)
            wb <- wb$add_fill(
              sheet = sheet_name,
              dims = cell_ref,
              color = wb_color("yellow")
            )
          }
        }
      }
      
      # Format numeric cells to show 3 decimal places
      # Format the data range (excluding headers and labels)
      data_range <- paste0("B2:", openxlsx2::int2col(n_cols), n_rows + 1)
      wb <- wb$add_numfmt(
        sheet = sheet_name,
        dims = data_range,
        numfmt = "0.000"
      )
      
      # Format sum cells
      # sum_col_range <- paste0(openxlsx2::int2col(n_cols + 1), "2:", openxlsx2::int2col(n_cols + 1), n_rows + 2)
      # wb <- wb$add_numfmt(
      #   sheet = sheet_name,
      #   dims = sum_col_range,
      #   numfmt = "0.000"
      # )
      
      sum_row_range <- paste0("B", n_rows + 2, ":", openxlsx2::int2col(n_cols), n_rows + 2)
      wb <- wb$add_numfmt(
        sheet = sheet_name,
        dims = sum_row_range,
        numfmt = "0.000"
      )
      
      # Save the workbook
      wb$save(output_file)
      message("Processed: ", tpm, " -> Saved to: ", output_file)
      
    }, error = function(e) {
      message("Error processing file with openxlsx2: ", e$message)
      message("Skipping file: ", tpm)
    })
  }
  
  message("\nAll files processed successfully!")
}