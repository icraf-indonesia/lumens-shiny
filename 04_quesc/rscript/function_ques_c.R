summary_text_en <- c("Period",
               "Total area (ha)",
               "Total emission (tonne CO2-eq)",
               "Total sequestration (tonne CO2-eq)",
               "Nett emission (tonne CO2-eq)",
               "Emission rate (tonne CO2-eq/year)",
               "Emission rate per-unit area (tonne CO2-eq/ha.year)")

summary_text_id <- c("Periode", 
                     "Total area (ha)", 
                     "Total Emisi (Ton CO2-eq)", 
                     "Total Sekuestrasi (Ton CO2-eq)", 
                     "Emisi Bersih (Ton CO2-eq)", 
                     "Laju Emisi (Ton CO2-eq/tahun)",
                     "Laju emisi per-unit area (Ton CO2-eq/ha.tahun)")

summary_zonal_text_en <- list(ID = 1,
                          "Planning Unit" = 2, 
                          "Area (Ha)" = 3, 
                          "Carbon Avg. (Periode 1)" = 4, 
                          "Carbon Avg. (Periode 2)" = 5, 
                          "Nett Emission" = 6, 
                          "Emission Rate" = 7
                          )
summary_zonal_text_id <- list( ID = 1,
                          "Unit Perencanaan" = 2, 
                          "Luas (Ha)" = 3, 
                          "Rerata Karbon Periode 1" = 4, 
                          "Rerata Karbon Periode 2" = 5, 
                          "Emisi bersih" = 6, 
                          "Laju emisi" = 7
                          )
summary_zona_carbon_text_en <- list(ID = 1,
                                    "Planning Unit" = 2, 
                                    "Area (Ha)" = 3, 
                                    "Total emission (tonne CO2-eq)" = 4, 
                                    "Total sequestration (tonne CO2-eq)" = 5, 
                                    "Nett Emission (tonne CO2-eq)" = 6, 
                                    "Emission Rate (tonne CO2-eq)" = 7
)
summary_zona_carbon_text_id <- list(ID = 1,
                                    "Unit perencanaan" = 2, 
                                    "Luas (Ha)" = 3, 
                                    "Total emisi (ton CO2-eq)" = 4, 
                                    "Total sekuestrasi (ton CO2-eq)" = 5, 
                                    "Emisi bersih (ton CO2-eq)" = 6, 
                                    "Laju emisi (ton CO2-eq)" = 7
)
  
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
  
  # Extract .libpaths, accomodate multiple library paths
  lib_paths <- .libPaths() |> paste( collapse = "<br>")
  
  # Combine all info into a single tibble
  session_summary <- tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details = c(r_version, platform_os, lib_paths, locale_info)
  )
  return(session_summary)
}

plot_quesc_results <- function(map, legend, low, high, title_size = 8, text_size = 8, height = 0.375, width = 0.375, ...) {
  p <- gplot(map, maxpixels = 100000) + 
    geom_raster(aes(fill = value)) + 
    coord_equal() +
    scale_fill_gradient(name = legend, low = low, high = high, guide = "colourbar", ...) +
    theme(plot.title = element_text(lineheight = 5, face = "bold")) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title = element_text(size = title_size),
          legend.text = element_text(size = text_size),
          legend.key.height = unit(height, "cm"),
          legend.key.width = unit(width, "cm"))
  
  
  return(p)
}

summary_of_emission_calculation <- function(quescdb, zone, map_em, map_sq, period) {
  p <- as.numeric(period$p2) - as.numeric(period$p1)
  az <- quescdb %>% 
    melt(id.vars=c('ID_PU', 'PU'), measure.vars=c('Ha')) %>%
    dcast(formula = ID_PU + PU ~ ., fun.aggregate = sum) %>%
    dplyr::rename(
      ID = 1,
      Ha = 3
    )
  
  ze <- map_em %>% 
    raster() %>%
    zonal(zone, 'sum') %>%
    as.data.frame() %>%
    dplyr::rename(
      ID = 1,
      TOTAL_EM = 2
    )
  zs <- map_sq %>%
    raster() %>%
    zonal(zone, 'sum') %>% 
    as.data.frame() %>%
    dplyr::rename(
      ID = 1,
      TOTAL_SQ = 2
    )
  
  zc <- az %>% 
    left_join(ze, by = "ID") %>% 
    left_join(zs, by = "ID") %>% 
    mutate(
      NET_EM = TOTAL_EM - TOTAL_SQ
    ) %>% 
    mutate(
      NET_EM_RATE = round(NET_EM / Ha / p, 2)
    ) %>% 
    mutate(
      TOTAL_EM = round(TOTAL_EM, 2),
      TOTAL_SQ = round(TOTAL_SQ, 2),
      NET_EM = round(NET_EM, 2)
    ) 
  
  zc_plot <- zc %>% ggplot(aes(x = reorder(PU, -NET_EM_RATE), y = (zc$NET_EM_RATE))) + 
    geom_bar(stat = "identity", fill = "red") +
    geom_text(data = zc, aes(label = round(NET_EM_RATE, 1)), size = 4) +
    ggtitle(paste("Average of nett emission ", period$p1,"-", period$p2)) +
    guides(fill = FALSE) + 
    ylab("CO2-eq/ha.yr") +
    theme(plot.title = element_text(lineheight = 5, face = "bold")) +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 20),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  zc <- zc %>% 
    dplyr::rename(
      unlist(summary_zona_carbon_text_en)
    )
  
  total_area <- sum(az$Ha)
  total_emission <- sum(zc$TOTAL_EM)
  total_sequestration <- sum(zc$TOTAL_SQ)
  total_net_emission <- total_emission - total_sequestration
  total_rate_emission <- total_net_emission / p
  total_rate_emission_ha <- total_rate_emission / total_area
  
  summary_df <- data.frame(
    ID = c(1:7),
    Category = summary_text_en,
    Summary = as.character(
      c(paste0(period$p1, "-", period$p2),
        round(total_area, 2),
        round(total_emission, 2),
        round(total_sequestration, 2),
        round(total_net_emission, 2),
        round(total_rate_emission, 2),
        round(total_rate_emission_ha, 2)
        )
    )
  )
  
  out <- list(
    area_zone = az,
    zone_emission = ze,
    zone_sequestration = zs,
    zone_carbon = zc,
    plot_zone_carbon = zc_plot,
    total_area = total_area,
    total_emission = total_emission,
    total_sequestration = total_sequestration,
    total_net_emission = total_net_emission,
    total_rate_emission = total_rate_emission,
    total_rate_emission_ha = total_rate_emission_ha,
    summary_df = summary_df
  )
  
  return(out)
}

zonal_statistic_database <- function(quescdb, period) {
  area_zone <- quescdb %>% 
    melt(id.vars=c('ID_PU', 'PU'), measure.vars=c('Ha')) %>%
    dcast(formula = ID_PU + PU ~ ., fun.aggregate = sum) %>%
    dplyr::rename(
      ID = 1,
      Ha = 3
    )
  
  data_zone <- area_zone
  data_zone$Z_CODE <- toupper(abbreviate(data_zone$PU))
  data_zone$Rate_seq <- data_zone$Rate_em <- data_zone$Avg_C_t2 <- data_zone$Avg_C_t1 <- 0
  for(a in 1:nrow(area_zone)){
    i <- area_zone$PU[a]
    data_z <- quescdb[which(quescdb$PU == i), ]
    data_zone <- within(data_zone, {
      Avg_C_t1 <- ifelse(data_zone$PU == i,
                         sum(data_z$C_T1 * data_z$Ha) / sum(data_z$Ha),
                         Avg_C_t1)
    }) 
    data_zone <- within(data_zone, {
      Avg_C_t2 <- ifelse(data_zone$PU == i,
                         sum(data_z$C_T2 * data_z$Ha) / sum(data_z$Ha),
                         Avg_C_t2)
    })
    data_zone <- within(data_zone, {
      Rate_em <- ifelse(data_zone$PU == i, 
                        sum(data_z$EM) / (sum(data_z$Ha) * period), 
                        Rate_em)
    })
    data_zone <- within(data_zone, {
      Rate_seq <- ifelse(data_zone$PU == i, 
                         sum(data_z$SQ) / (sum(data_z$Ha) * period), 
                         Rate_seq)
    }) 
  }
  
  data_zone_df <- data_zone %>% 
    select(-Z_CODE) %>%
    mutate(
      Avg_C_t1 = round(Avg_C_t1, 2),
      Avg_C_t2 = round(Avg_C_t2, 2),
      Rate_em = round(Rate_em, 2),
      Rate_seq = round(Rate_seq, 2)
    ) %>%
    dplyr::rename(
      unlist(summary_zonal_text_en)
    )
  
  # data_merge_sel <- quescdb[ which((quescdb$EM + quescdb$SQ) > 0), ]
  order_sq <- quescdb[order(-quescdb$SQ), ] %>% as.data.frame()
  order_em <- quescdb[order(-quescdb$EM), ] %>% as.data.frame()
  
  # total emission
  tb_em_total <- order_em$LU_CHG %>% 
    cbind( as.data.frame( round(order_em$EM, digits=3) ) ) %>% 
    as.data.frame() %>%
    dplyr::rename(
      LU_CHG = 1,
      EM = 2
    ) %>%
    aggregate(EM ~ LU_CHG, FUN = sum) %>%
    mutate(
      LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength=5, strict=FALSE, method = "both")))
    ) %>%
    dplyr::arrange(desc(EM)) %>%
    dplyr::relocate(LU_CODE) 
  
  tb_em_total_10 <- tb_em_total %>%
    mutate(
      PERCENTAGE = as.numeric(format(round((EM / sum(tb_em_total$EM) * 100),2), nsmall=2))
    ) %>%
    head(n=10)
  
  largest_emission <- tb_em_total_10 %>% 
    ggplot(aes(x = reorder(LU_CODE, -EM), y = (EM))) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(data = tb_em_total_10, aes(x=LU_CODE, y=EM, label = round(EM, 1)), size = 3, vjust = 0.1) +
    ggtitle(paste("Largest sources of emission")) + 
    guides(fill = FALSE) + 
    ylab("CO2-eq") +
    theme(plot.title = element_text(lineheight = 5, face = "bold")) + 
    scale_y_continuous() +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  # zonal emission
  tb_em_zonal <- as.data.frame(NULL)
  for (i in 1:nrow(area_zone)){
    tryCatch({
      tb_em <- order_em$PU %>% 
        cbind(order_em$LU_CHG, as.data.frame( round(order_em$EM, digits=3) ) ) %>% 
        as.data.frame() %>%
        dplyr::rename(
          PU = 1,
          LU_CHG = 2,
          EM = 3
        )
      
      a <- area_zone$PU[i] 
      tb_em_z <- tb_em %>% 
        dplyr::filter(PU == a) %>% 
        as.data.frame() %>%
        aggregate(EM ~ PU + LU_CHG, FUN=sum) %>%
        mutate(
          LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength=5, strict=FALSE, method = "both")))
        ) %>%
        dplyr::arrange(desc(EM)) %>%
        dplyr::relocate(LU_CODE, .before = LU_CHG) 
      tb_em_z_10 <- tb_em_z %>% 
        mutate(
          PERCENTAGE = as.numeric(format(round((EM / sum(tb_em_z$EM) * 100),2), nsmall=2)) 
        ) %>% 
        head(n=10)
      tb_em_zonal <- tb_em_zonal %>% rbind(tb_em_z_10)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  # total sequestration
  tb_sq_total <- order_sq$LU_CHG %>% 
    cbind( as.data.frame( round(order_sq$EM, digits=3) ) ) %>% 
    as.data.frame() %>%
    dplyr::rename(
      LU_CHG = 1,
      SQ = 2
    ) %>%
    aggregate(SQ ~ LU_CHG, FUN = sum) %>%
    mutate(
      LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength=5, strict=FALSE, method = "both")))
    ) %>%
    dplyr::arrange(desc(SQ)) %>%
    dplyr::relocate(LU_CODE) 
  
  tb_sq_total_10 <- tb_sq_total %>% 
    mutate(
      PERCENTAGE = as.numeric(format(round((SQ / sum(tb_sq_total$SQ) * 100),2), nsmall=2))
    ) %>%
    head(n=10)
  
  largest_sequestration <- tb_sq_total_10 %>% 
    ggplot(aes(x = reorder(LU_CODE, -SQ), y = (SQ))) +
    geom_bar(stat = "identity", fill = "green") +
    geom_text(data = tb_sq_total_10, aes(x=LU_CODE, y=SQ, label = round(SQ, 1)), size = 3, vjust = 0.1) +
    ggtitle(paste("Largest sources of sequestration")) + 
    guides(fill = FALSE) + 
    ylab("CO2-eq") +
    theme(plot.title = element_text(lineheight = 5, face = "bold")) + 
    scale_y_continuous() +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  # zonal sequestration
  tb_sq_zonal <- as.data.frame(NULL)
  for (i in 1:nrow(area_zone)){
    tryCatch({
      tb_sq <- order_sq$PU %>% 
        cbind(order_sq$LU_CHG, as.data.frame( round(order_sq$SQ, digits=3) ) ) %>% 
        as.data.frame() %>%
        dplyr::rename(
          PU = 1,
          LU_CHG = 2,
          SQ = 3
        )
      
      a <- area_zone$PU[i] 
      tb_sq_z <- tb_sq %>% 
        dplyr::filter(PU == a) %>% 
        as.data.frame() %>%
        aggregate(SQ ~ PU + LU_CHG, FUN=sum) %>%
        mutate(
          LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength=5, strict=FALSE, method = "both")))
        ) %>%
        dplyr::arrange(desc(SQ)) %>%
        dplyr::relocate(LU_CODE, .before = LU_CHG) 
      tb_sq_z_10 <- tb_sq_z %>% 
        mutate(
          PERCENTAGE = as.numeric(format(round((SQ / sum(tb_sq_z$SQ) * 100),2), nsmall=2))
        ) %>%
        head(n=10)
      tb_sq_zonal <- tb_sq_zonal %>% rbind(tb_sq_z_10)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  out <- list(
    data_zone_df = data_zone_df,
    tb_em_total_10 = tb_em_total_10,
    largest_emission = largest_emission,
    tb_em_zonal = tb_em_zonal,
    tb_sq_total_10 = tb_sq_total_10,
    largest_sequestration = largest_sequestration,
    tb_sq_zonal = tb_sq_zonal
  )
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
    start_time = output_quesc$start_time,
    end_time = output_quesc$end_time,
    map_c1 = output_quesc$map_c1,
    map_c2 = output_quesc$map_c2,
    map_em = output_quesc$map_em,
    map_sq = output_quesc$map_sq,
    ques_db = output_quesc$ques_db,
    p1 = output_quesc$p1,
    p2 = output_quesc$p2,
    inputs = output_quesc$inputs,
    session_log = output_quesc$session_log
  )
  output_file <- paste0("quesc_report_", Sys.Date(), ".html")
  
  rmarkdown::render(
    "../report_template/quesc_report_template.Rmd",
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
run_quesc_analysis <- function(lc_t1_path, lc_t2_path, admin_z_path, c_lookup_path,
                               lc_t1_input, lc_t2_input, admin_z_input,
                               c_lookup_input, zone_lookup_input,
                               time_points, output_dir, progress_callback = NULL) {
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
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
  
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  session_log <- format_session_info_table()
  
  out <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    map_c1 = map_carbon1,
    map_c2 = map_carbon2,
    map_em = map_emission,
    map_sq = map_sequestration,
    ques_db = df_lucdb,
    p1 = time_points$t1,
    p2 = time_points$t2,
    inputs = list(
      lc_t1_path = lc_t1_path,
      lc_t2_path = lc_t2_path,
      admin_z_path = admin_z_path,
      c_lookup_path = c_lookup_path,
      output_dir = output_dir
    ),
    session_log = session_log
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
