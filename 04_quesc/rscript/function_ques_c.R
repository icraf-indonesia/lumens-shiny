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


#' Generate QUES-C Report
#' 
#' Generates a report for the Pre-QUES analysis using R Markdown.
#'
#' @param output_quesc List. Output from QUES-C analysis.
#' @param dir Character string. Directory to save the report.
#' 
#' @importFrom rmarkdown render
#'
#' @export
generate_quesc_report <- function(output_quesc, dir) {
  report_params <- list(
    map_c1 = output_quesc$map_c1,
    map_c2 = output_quesc$map_c2,
    map_em = output_quesc$map_em,
    map_sq = output_quesc$map_sq,
    ques_db = output_quesc$ques_db,
    p1 = output_quesc$p1,
    p2 = output_quesc$p2
  )
  output_file <- paste0("quesc_report_", Sys.Date(), ".html")
  
  rmarkdown::render(
    "../report_template/report_template.Rmd",
    output_file = output_file,
    output_dir = dir,
    params = report_params
  )
}

#' Run QUES-C Analysis
#' 
#' Perform LUMENS module QUES-C analysis
#'
#' @param lc_t1_input RasterLayer. Land cover data for time point 1.
#' @param lc_t2_input RasterLayer. Land cover data for time point 2.
#' @param admin_z_input RasterLayer Administrative zones data.
#' @param c_lookup_input Data frame. Land cover lookup table input.
#' @param zone_lookup_input Data frame. Zone lookup table input.
#' @param time_points List. Time points for analysis (t1 and t2).
#' @param output_dir Character string. Directory to save output files.
#' @param progress_callback Function. Callback function to report progress (optional).
#'
#' @return List containing output_quesc
#' 
#' @import LUMENSR
#' @importFrom dplyr %>% rename rename_with right_join left_join mutate
#' @importFrom terra writeRaster
#' 
#' @export
run_quesc_analysis <- function(lc_t1_input, lc_t2_input, admin_z_input,
                               c_lookup_input, zone_lookup_input,
                               time_points, output_dir, progress_callback = NULL) {
  
  map1_rast <- lc_t1_input %>% spatial_sync_raster(admin_z_input)
  map2_rast <- lc_t2_input %>% spatial_sync_raster(admin_z_input)
  
  if (!is.null(progress_callback)) progress_callback(0.2, "load maps")
  
  lc_t1 <- map1_rast %>% rast() %>%
    add_legend_to_categorical_raster(lookup_table = c_lookup_input, year = as.numeric(time_points$t1))
  lc_t2 <- map2_rast %>% rast() %>%
    add_legend_to_categorical_raster(lookup_table = c_lookup_input, year = as.numeric(time_points$t2))  
  zone <- admin_z_input %>% rast() %>%
    add_legend_to_categorical_raster(lookup_table = zone_lookup_input)
  
  preques <- ques_pre(lc_t1, lc_t2, zone)
  period_year <- as.numeric(time_points$t1) - as.numeric(time_points$t2)
  lucDummy <- generate_dummy_crosstab(c_lookup_input, zone_lookup_input)
  
  if (!is.null(progress_callback)) progress_callback(0.5, "create QUES-C database")

  # join table
  df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC1 = 1, C_T1 = 3) %>% 
    rename_with(.cols = 2, ~time_points$t1) %>% right_join(lucDummy, by="ID_LC1")
  df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC2 = 1, C_T2 = 3) %>% 
    rename_with(.cols = 2, ~time_points$t2) %>% right_join(df_lucdb, by="ID_LC2")
  df_lucdb <- zone_lookup_input %>% dplyr::rename(ID_PU = 1) %>% 
    rename_with(.cols = 2, ~names(zone)) %>% right_join(df_lucdb, by="ID_PU") 
  df_lucdb <- df_lucdb %>% 
    left_join(
      preques[["landscape_level"]][["crosstab_long"]], 
      by = c(names(zone), time_points$t1, time_points$t2)
    ) 
  # the full version of preques database from preques analysis combined with all possible landcover listed in the lookup table
  df_lucdb <- df_lucdb %>% replace(is.na(df_lucdb), 0) %>% dplyr::rename(PU = names(zone))
  
  # create new matrix reclassification 
  reclassify_matrix <- as.matrix(c_lookup_input[,1]) %>% 
    cbind(., as.matrix(c_lookup_input[,3])) %>%
    rbind(., c(0, NA))
  
  if (!is.null(progress_callback)) progress_callback(0.7, "generate carbon, emission, and sequestration maps")
  
  # create all maps
  map_carbon1 <- lc_t1 %>% classify(reclassify_matrix)
  map_carbon2 <- lc_t2 %>% classify(reclassify_matrix)
  map_emission <- ((map_carbon1 - map_carbon2) * 3.67) * (map_carbon1 > map_carbon2)
  map_sequestration <- ((map_carbon2 - map_carbon1) * 3.67) * (map_carbon1 < map_carbon2)
  
  # quescdatabase
  df_lucdb <- df_lucdb %>% mutate(
    EM = (C_T1 - C_T2) * (C_T1 > C_T2) * Ha * 3.67,
    SQ = (C_T2 - C_T1) * (C_T1 < C_T2) * Ha * 3.67,
    LU_CHG = do.call(paste, c(df_lucdb[c(time_points$t1, time_points$t2)], sep = " to "))
  )
  
  out <- list(
    map_c1 = map_carbon1,
    map_c2 = map_carbon2,
    map_em = map_emission,
    map_sq = map_sequestration,
    ques_db = df_lucdb,
    p1 = time_points$t1,
    p2 = time_points$t2
  )
  
  if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")
  write.table(df_lucdb,
              paste0(output_dir, "/quesc_database.csv"), 
              quote=FALSE, 
              row.names=FALSE, 
              sep=",")
  writeRaster(map_carbon1,
              paste0(output_dir, "/carbon_map_t1.tif"), overwrite = T)
  writeRaster(map_carbon2,
              paste0(output_dir, "/carbon_map_t2.tif"), overwrite = T)
  writeRaster(map_emission,
              paste0(output_dir, "/emission_map.tif"), overwrite = T)
  writeRaster(map_sequestration,
              paste0(output_dir, "/sequestration_map.tif"), overwrite = T)
  
  if (!is.null(progress_callback)) progress_callback(1, "generate report")
  generate_quesc_report(output_quesc = out, dir = output_dir)
  
  return(out)
}

# Shiny App Related -------------------------------------------------------