summary_text_en <- c("Period",
                     "Total area (ha)",
                     "Total emission (tonne CO2-eq)",
                     "Total sequestration (tonne CO2-eq)",
                     "Net emission (tonne CO2-eq)",
                     "Emission rate (tonne CO2-eq/ha.year)",
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
                              "Net Emission" = 6, 
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
                                    "Net Emission (tonne CO2-eq)" = 6, 
                                    "Emission Rate (tonne CO2-eq/ha.year)" = 7
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

#' Rasterize an sf MULTIPOLYGON object
#'
#' This function rasterizes an sf MULTIPOLYGON object to a SpatRaster object. The function also retains
#' an attribute table from the sf object, by assigning categorical ID values to the raster values.
#' The rasterized SpatRaster object will also contain a legend derived from the attribute table of the sf object.
#'
#' @param sf_object An sf MULTIPOLYGON object. It must contain an attribute table, with at least one categorical ID (numeric).
#' @param raster_res A numeric vector specifying the resolution of the raster. Default is c(100,100).
#' @param field A character string specifying the field name to be used for rasterization from the sf object. Default is "ID".
#' @return A SpatRaster object that is a rasterized version of the input sf object, with a legend derived from the attribute table of the sf object.
#' @importFrom sf st_drop_geometry st_geometry_type st_crs
#' @importFrom terra vect ext rast rasterize levels
#' @export
#' @examples
#' rasterise_multipolygon(sf_object = ntt_admin, raster_res = c(100,100), field = "ID")
rasterise_multipolygon_quesc <- function(sf_object, raster_res, field = "ID"){
  
  # Error checking
  if (!inherits(sf_object, "sf")) stop("sf_object must be an sf object.")
  if (!all(sf::st_geometry_type(sf_object) == "MULTIPOLYGON")) stop("All features in sf_object must be MULTIPOLYGONs.")  # Check if sf_object has UTM projection
  if (!grepl("\\+units=m", st_crs(sf_object)$proj4string)) stop("sf_object must have UTM projection system.")
  if (is.null(sf::st_drop_geometry(sf_object)) || !(field %in% names(sf::st_drop_geometry(sf_object)))) stop("sf_object must contain an attribute table with at least one numeric/factor column.")
  if (!is.numeric(sf_object[[field]]) && !is.factor(sf_object[[field]])) stop("The field must be numeric or a factor.")
  
  # Convert the sf object to a SpatVector
  spatvect <- terra::vect(sf_object)
  
  # Define the extent based on the SpatVector
  raster_extent <- terra::ext(spatvect)
  
  # Create an empty SpatRaster based on the extent, resolution, and CRS
  raster_template <- terra::rast(raster_extent, resolution = raster_res, crs = terra::crs(spatvect))
  
  # Rasterize the SpatVector based on the SpatRaster template
  # Specify the field in the rasterize function
  rasterised_spatraster <- terra::rasterize(spatvect, raster_template, field = field)
  
  # Convert the 'Kabupaten' column of the sf_object to a lookup_table
  lookup_table <- sf::st_drop_geometry(sf_object)
  
  # Add legend to the rasterized SpatRaster using the lookup_table
  levels(rasterised_spatraster) <- lookup_table
  
  # Return the rasterized SpatRaster with legend
  return(rasterised_spatraster)
}

print_area <- function(x){
  format(x, digits=15, big.mark=",")
}
print_rate <- function(x){
  format(x, digits=15, nsmall=2, decimal.mark=".", big.mark=",")
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

# Rename uploaded file
rename_uploaded_file <- function(input_file) {
  if (is.null(input_file)) return(NULL)
  
  old_path <- input_file$datapath
  new_path <- file.path(dirname(old_path), input_file$name)
  file.rename(old_path, new_path)
  return(new_path)
}

# Summary of emission calculation
summary_of_emission_calculation <- function(peat_decomposition, quescdb, zone, map_em, map_sq, period) {
  p <- as.numeric(period$p2) - as.numeric(period$p1)
  
  # Use tidyr::pivot_longer instead of melt
  az <- quescdb %>%
    pivot_longer(cols = Ha, names_to = "variable", values_to = "value") %>%
    group_by(ID_PU, PU) %>%
    summarise(Ha = sum(value), .groups = 'drop') %>%
    dplyr::rename(
      ID = ID_PU,
      Ha = Ha
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
      NET_EM = TOTAL_EM - TOTAL_SQ,
      NET_EM_RATE = round(NET_EM / Ha / p, 2),
      TOTAL_EM = round(TOTAL_EM, 2),
      TOTAL_SQ = round(TOTAL_SQ, 2),
      NET_EM = round(NET_EM, 2)
    )
  
  zc_plot <- zc %>% ggplot(aes(x = reorder(PU, -NET_EM_RATE), y = (NET_EM_RATE))) +
    geom_bar(stat = "identity", fill = "red") +
    geom_text(data = zc, aes(label = round(NET_EM_RATE, 1)), size = 4) +
    ggtitle(paste("Average of net emission rate", period$p1,"-", period$p2)) +
    guides(fill = FALSE) +
    ylab("tonne CO2-eq/ha.yr") +
    theme(plot.title = element_text(lineheight = 5, face = "bold")) +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 20),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  total_area <- sum(az$Ha)
  # total_emission <- sum(zc$TOTAL_EM)
  total_sequestration <- sum(zc$TOTAL_SQ)
  # total_net_emission <- total_emission - total_sequestration
  # total_rate_emission <- total_net_emission / p
  # total_rate_emission_ha <- total_rate_emission / total_area
  
  zc <- zc %>%
    mutate(Ha = format(round(Ha, 2), nsmall = 2, big.mark = ",", decimal.mark = ".")) %>%
    mutate_if(is.numeric, print_rate) %>%
    dplyr::rename(
      unlist(summary_zona_carbon_text_en)
    )
  
  if (peat_decomposition == "Yes") {
    total_em_mineral <- sum(quescdb %>% select(em_mineral))
    total_em_peat <- sum(quescdb %>% select(em_peat))
    total_emission <- sum(quescdb %>% select(EM))
    total_net_emission <- total_emission - total_sequestration
    total_rate_emission <- total_net_emission / p
    total_rate_emission_ha <- total_rate_emission / total_area
    
    # Ensure summary_text_en has 9 elements for this case
    summary_text_en <- c(
      "Period",
      "Total Area (Ha)",
      "Total Emission From Mineral land (tonne CO2-eq)",
      "Total Emission From Peat land (tonne CO2-eq)",
      "Total Emission (tonne CO2-eq)",
      "Total Sequestration (tonne CO2-eq)",
      "Net Emission (tonne CO2-eq)",
      "Net Emission Rate (tonne CO2-eq/yr)",
      "Net Emission Rate per Ha (tonne CO2-eq/ha.yr)"
    )
    
    summary_df <- data.frame(
      ID = c(1:9),
      Category = summary_text_en,
      Summary = as.character(
        c(paste0(period$p1, "-", period$p2),
          print_area(round(total_area, 2)),
          print_rate(round(total_em_mineral, 2)),
          print_rate(round(total_em_peat, 2)),
          print_rate(round(total_emission, 2)),
          print_rate(round(total_sequestration, 2)),
          print_rate(round(total_net_emission, 2)),
          print_rate(round(total_rate_emission, 2)),
          print_rate(round(total_rate_emission_ha, 2))
        )
      )
    )
  } else {
    total_emission <- sum(zc$TOTAL_EM)
    total_net_emission <- total_emission - total_sequestration
    total_rate_emission <- total_net_emission / p
    total_rate_emission_ha <- total_rate_emission / total_area
    
    summary_text_en <- c(
      "Period",
      "Total Area (Ha)",
      "Total Emission (tonne CO2-eq)",
      "Total Sequestration (tonne CO2-eq)",
      "Net Emission (tonne CO2-eq)",
      "Net Emission Rate (tonne CO2-eq/yr)",
      "Net Emission Rate per Ha (tonne CO2-eq/ha.yr)"
    )
    
    summary_df <- data.frame(
      ID = c(1:7),
      Category = summary_text_en,
      Summary = as.character(
        c(paste0(period$p1, "-", period$p2),
          print_area(round(total_area, 2)),
          print_rate(round(total_emission, 2)),
          print_rate(round(total_sequestration, 2)),
          print_rate(round(total_net_emission, 2)),
          print_rate(round(total_rate_emission, 2)),
          print_rate(round(total_rate_emission_ha, 2))
        )
      )
    )
  }
  
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
  # Use tidyr::pivot_longer instead of melt
  area_zone <- quescdb %>% 
    pivot_longer(cols = Ha, names_to = "variable", values_to = "value") %>%
    group_by(ID_PU, PU) %>%
    summarise(Ha = sum(value), .groups = 'drop') %>%
    dplyr::rename(
      ID = ID_PU,
      Ha = Ha
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
  
  data_zone_ori <- data_zone %>% 
    dplyr::select(-Z_CODE) %>%
    mutate(
      Avg_C_t1 = round(Avg_C_t1, 2),
      Avg_C_t2 = round(Avg_C_t2, 2),
      Rate_em = round(Rate_em, 2),
      Rate_seq = round(Rate_seq, 2)
    ) 
  data_zone_summary <- data_zone_ori %>% 
    mutate(Ha = format(round(Ha, 2), nsmall = 2, big.mark = ",", decimal.mark = ".")) %>% 
    mutate_if(is.numeric, print_rate) %>% 
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
  tb_em_total_10_summary <- tb_em_total_10 %>%
    mutate(EM = print_rate(EM)) %>%
    dplyr::rename(
      "Land Use Code" = LU_CODE,
      "Land Use Change" = LU_CHG,
      "Total Emission" = EM,
      "Percentage" = PERCENTAGE
    )
  
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
    cbind( as.data.frame( round(order_sq$SQ, digits=3) ) ) %>% 
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
  tb_sq_total_10_summary <- tb_sq_total_10 %>%
    mutate(SQ = print_rate(SQ)) %>%
    dplyr::rename(
      "Land Use Code" = LU_CODE,
      "Land Use Change" = LU_CHG,
      "Total Sequestration" = SQ,
      "Percentage" = PERCENTAGE
    )
  
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
    data_zone = data_zone_ori,
    data_zone_df = data_zone_summary,
    tb_em_total_10 = tb_em_total_10,
    tb_em_total_10_summary = tb_em_total_10_summary,
    largest_emission = largest_emission,
    tb_em_zonal = tb_em_zonal,
    tb_sq_total_10 = tb_sq_total_10,
    tb_sq_total_10_summary = tb_sq_total_10_summary,
    largest_sequestration = largest_sequestration,
    tb_sq_zonal = tb_sq_zonal
  )
}

# Define the is_numeric_str() function to check if a string is numeric
is_numeric_str <- function(s) {
  !is.na(as.numeric(s)) && nzchar(s)
}

#' Read Shapefile
#'
#' This function reads a shapefile from uploaded files, handling file renaming and validation.
#'
#' @param shp_input List. Input data for the shapefile, including file paths and names.
#'
#' @return sf object. The read shapefile.
#'
#' @importFrom sf st_read
#' @importFrom tools file_ext
#'
#' @export
read_shapefile <- function(shp_input) {
  if (is.null(shp_input)) return(NULL)
  
  prev_wd <- getwd()
  on.exit(setwd(prev_wd), add = TRUE)  # This ensures we always return to the previous working directory
  
  tryCatch({
    uploaded_dir <- dirname(shp_input$datapath[1])
    setwd(uploaded_dir)
    
    for (i in 1:nrow(shp_input)) {
      old_path <- shp_input$datapath[i]
      new_path <- file.path(uploaded_dir, shp_input$name[i])
      cat("Attempting to rename:", old_path, "to", new_path, "\n")
      rename_result <- file.rename(old_path, new_path)
      cat("Rename result:", rename_result, "\n")
      if (!rename_result) {
        cat("File exists (old):", file.exists(old_path), "\n")
        cat("File exists (new):", file.exists(new_path), "\n")
      }
    }
    
    shp_file <- shp_input$name[grep(pattern = "*.shp$", shp_input$name)]
    if (length(shp_file) == 0) {
      stop("No .shp file found in the uploaded files.")
    }
    
    required_extensions <- c("shp", "dbf", "prj", "shx")
    missing_files <- required_extensions[!required_extensions %in% tools::file_ext(list.files(uploaded_dir))]
    if (length(missing_files) > 0) {
      stop(paste("Missing required shapefile components:", paste(missing_files, collapse = ", ")))
    }
    
    cat("About to read shapefile:", shp_file, "\n")
    cat("Files in directory after renaming:\n")
    print(list.files(uploaded_dir))
    
    # Read and return the shapefile
    sf_object <- sf::st_read(shp_file)
    return(sf_object)
  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    stop(paste("Error reading shapefile:", e$message))
  })
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
  options(repos = c(CRAN = "https://cloud.r-project.org"))
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
#' 
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

cross_tabulation <- function(pu_table, luc_lut, zone, luc_1, luc_2, lu_chg) {
  # Prepare the reference table
  ref_table <- pu_table
  colnames(ref_table) <- c("IDADM", "ZONE")
  
  # Generate count_ref
  count_ref <- as.data.frame(freq(zone))
  if (ncol(count_ref) > 2){
    count_ref <- count_ref[, -1]
  }
  count_ref <- na.omit(count_ref)
  colnames(count_ref) <- c("IDADM", "COUNT")
  lookup_z <- merge(count_ref, ref_table, by = "IDADM")
  
  # Prepare the land use lookup tables
  lut.lc <- luc_lut
  lut.lc <- lut.lc[, 1:2]
  lookup_lc <- lut.lc
  colnames(lookup_lc) <- c("ID", "CLASS")
  
  # Generate the land use change map dummy
  landUseChangeMapDummy <- generate_dummy_crosstab(lookup_lc, lookup_z)
  
  # Calculate the land use change map
  lu.db <- as.data.frame(freq(lu_chg))
  if (ncol(lu.db) > 2){
    lu.db <- lu.db[, -1]
  }
  lu.db <- na.omit(lu.db)
  
  # Decompose the land use change map values
  n <- 3
  k <- 0
  lu.db$value_temp <- lu.db$value
  while (k < n) {
    eval(parse(text = (paste("lu.db$Var", n - k, "<-lu.db$value_temp %% 100", sep = ""))))
    lu.db$value_temp <- floor(lu.db$value_temp / 100)
    k = k + 1
  }
  lu.db$value_temp <- NULL
  colnames(lu.db) = c('ID_CHG', 'COUNT', 'ID_PU', 'ID_LC1', 'ID_LC2')
  lu.db$ID_PU <- as.numeric(lu.db$ID_PU)

  # Merge with the land use change map dummy
  lu.db <- merge(landUseChangeMapDummy, lu.db, by = c('ID_PU', 'ID_LC1', 'ID_LC2'), all = TRUE)
  lu.db$ID_PU <- as.numeric(as.character(lu.db$ID_PU))
  # lu.db <- na.omit(lu.db)
  lu.db$ID_CHG <- (lu.db$ID_PU * 1) + (lu.db$ID_LC1 * 100^1) + (lu.db$ID_LC2 * 100^2)
  lu.db <- replace(lu.db, is.na(lu.db), 0)
  
  return(list(lu.db = lu.db, landUseChangeMapDummy = landUseChangeMapDummy))
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
    # fill_scale <- scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F","#BAB0AC"), na.value = "white")
    fill_scale <- scale_fill_manual(
      values = c(
        "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", 
        "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
        "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
        "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
        "#67001F", "#3288BD", "#66C2A5", "#FC8D62", "#8DA0CB",
        "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3",
        "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0",
        "#F0027F", "#BF5B17", "#666666", "#A6CEE3", "#1F78B4",
        "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F",
        "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF33", "#B15928"
      ),
      na.value = "white"
    )
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
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.height = unit(0.25, "cm"),
          legend.key.width = unit(0.25, "cm"),
          legend.position = "bottom",
          legend.justification = c(0,0.5))
  return(plot_lc)
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
generate_quesc_report <- function(output_quesc, output_dir) {
  report_params <- list(
    start_time = start_time,
    end_time = end_time,
    map_c1 = map_c1,
    map_c2 = map_c2,
    map_em = map_e,
    map_sq = map_s,
    ques_db = tbl_quesc,
    p1 = rv$t1,
    p2 = rv$t2,
    if (input$peat_decomposition == "Yes"){
      tbl_quesc_peat = tbl_quesc_peat
      tbl_quesc_peat_sum = tbl_quesc_peat_sum
      map_e_peat = map_e_peat
      map_e_mineral_peat = map_e_mineral_peat
      quesc_database_mineral_peat = quesc_database_mineral_peat
    },
    inputs = list(
      lc_t1_path = lc_t1_path,
      lc_t2_path = lc_t2_path,
      admin_z_path = admin_z_path,
      c_lookup_path = c_lookup_path,
      if (input$peat_decomposition == "Yes"){
        peat_emission_factor_table_path = peat_emission_factor_table_path
      },
      output_dir = output_dir
    ),
    session_log = session_log
  )
  
  output_file <- paste0("quesc_report_", Sys.Date(), ".html")
  
  rmarkdown::render(
    "../report_template/quesc_report_template.Rmd",
    output_file = output_file,
    output_dir = output_dir,
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
                               time_points, output_dir, progress_callback = NULL) {
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }
  
  # Prepare LC raster
  lc_t1_input <- raster(lc_t1_path)
  lc_t2_input <- raster(lc_t2_path)
  
  # Prepare zone
  zone_sf <- read_shapefile(shp_input = admin_z_path)
  # zone_sf <- admin_z_path %>% st_read()
  zone_sf <- st_cast(zone_sf, "MULTIPOLYGON")
  zone <- zone_sf %>% 
    rasterise_multipolygon_quesc(
      raster_res = res(lc_t1_input), 
      field = paste0(colnames(st_drop_geometry(zone_sf[1])))
    )
  zone_lookup_input <- data.frame(ID_PU = zone_sf[[1]], PU = zone_sf[[2]])
  admin_z_input <- zone %>% raster()
  
  # Prepare C lookup table
  df_c <- read.csv(c_lookup_path)
  
  if(nrow(df_c) == 0)
    return()
  if(nrow(df_c) < 2)
    return()
  if(!is_numeric_str(df_c[1, 1]))
    return()
  
  df <- data.frame("ID_LC" = as.integer((as.character(df_c[, 1]))))
  df$LC <- df_c[, 2]
  df$CARBON <- df_c[, 3]
  c_lookup_input <- df
  
  map1_rast <- lc_t1_input %>% spatial_sync_raster(admin_z_input)
  map2_rast <- lc_t2_input %>% spatial_sync_raster(admin_z_input)
  
  # if (!is.null(progress_callback)) progress_callback(0.2, "load maps")
  
  lc_t1 <- map1_rast %>% rast() %>%
    add_legend_to_categorical_raster(lookup_table = c_lookup_input, year = as.numeric(as.character(time_points$t1)))
  lc_t2 <- map2_rast %>% rast() %>%
    add_legend_to_categorical_raster(lookup_table = c_lookup_input, year = as.numeric(as.character(time_points$t2)))  
  zone <- admin_z_input %>% rast() %>%
    add_legend_to_categorical_raster(lookup_table = zone_lookup_input)
  
  preques <- ques_pre_quesc(lc_t1, lc_t2, zone)
  period_year <- as.numeric(as.character(time_points$t1)) - as.numeric(as.character(time_points$t2))
  lucDummy <- generate_dummy_crosstab(c_lookup_input, zone_lookup_input)
  
  # if (!is.null(progress_callback)) progress_callback(0.5, "create QUES-C database")
  
  # join table
  df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC1 = 1, C_T1 = 3) %>% 
    rename_with(.cols = 2, ~as.character(time_points$t1)) %>% right_join(lucDummy, by="ID_LC1")
  df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC2 = 1, C_T2 = 3) %>% 
    rename_with(.cols = 2, ~as.character(time_points$t2)) %>% right_join(df_lucdb, by="ID_LC2")
  df_lucdb <- zone_lookup_input %>% dplyr::rename(ID_PU = 1) %>% 
    rename_with(.cols = 2, ~names(zone)) %>% right_join(df_lucdb, by="ID_PU") 
  df_lucdb <- df_lucdb %>% 
    left_join(
      preques[["landscape_level"]][["crosstab_long"]], 
      by = c(names(zone), as.character(time_points$t1), as.character(time_points$t2))
    ) 
  # the full version of preques database from preques analysis combined with all possible landcover listed in the lookup table
  df_lucdb <- df_lucdb %>% replace(is.na(df_lucdb), 0) %>% dplyr::rename(PU = names(zone))
  
  # create new matrix reclassification 
  reclassify_matrix <- as.matrix(c_lookup_input[,1]) %>% 
    cbind(., as.matrix(c_lookup_input[,3])) %>%
    rbind(., c(0, NA))
  
  # if (!is.null(progress_callback)) progress_callback(0.7, "generate carbon, emission, and sequestration maps")
  
  # create all maps
  map_carbon1 <- lc_t1 %>% classify(reclassify_matrix)
  map_carbon2 <- lc_t2 %>% classify(reclassify_matrix)
  map_emission <- ((map_carbon1 - map_carbon2) * 3.67) * (map_carbon1 > map_carbon2)
  map_sequestration <- ((map_carbon2 - map_carbon1) * 3.67) * (map_carbon1 < map_carbon2)
  
  # quescdatabase
  df_lucdb <- df_lucdb %>% mutate(
    EM = (C_T1 - C_T2) * (C_T1 > C_T2) * Ha * 3.67,
    SQ = (C_T2 - C_T1) * (C_T1 < C_T2) * Ha * 3.67,
    LU_CHG = do.call(paste, c(df_lucdb[c(as.character(time_points$t1), as.character(time_points$t2))], sep = " to "))
  )
  # session_log <- format_session_info_table()
  
  out <- list(
    map_c1 = map_carbon1,
    map_c2 = map_carbon2,
    map_em = map_emission,
    map_sq = map_sequestration,
    ques_db = df_lucdb,
    lc_t1 = lc_t1,
    lc_t2 = lc_t2, 
    zone = zone,
    df_pu = zone_lookup_input,
    df_c = df_c
  )
  
  # if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")
  # write.table(df_lucdb,
  #             paste0(output_dir, "/quesc_database.csv"), 
  #             quote=FALSE, 
  #             row.names=FALSE, 
  #             sep=",")
  # writeRaster(map_carbon1,
  #             paste0(output_dir, "/carbon_map_t1.tif"), overwrite = T)
  # writeRaster(map_carbon2,
  #             paste0(output_dir, "/carbon_map_t2.tif"), overwrite = T)
  # writeRaster(map_emission,
  #             paste0(output_dir, "/emission_map.tif"), overwrite = T)
  # writeRaster(map_sequestration,
  #             paste0(output_dir, "/sequestration_map.tif"), overwrite = T)
  
  # if (!is.null(progress_callback)) progress_callback(1, "generate report")
  # generate_quesc_report(output_quesc = out, dir = output_dir)
  
  return(out)
}

#' Run QuES-C Peat Analysis
#'
#' This function calculates peat emissions based on land use change between two time periods.
#' It processes spatial data including land cover maps, administrative zones, and peat maps
#' to compute emission changes and create related spatial outputs.
#'
#' @param output_dir Character string specifying the output directory path
#' @param lc_t1_path Character string specifying the path to land cover map for time 1
#' @param lc_t2_path Character string specifying the path to land cover map for time 2
#' @param admin_z_path Character string specifying the path to administrative zone shapefile
#' @param peat_map_path Character string specifying the path to peat distribution map
#' @param peat_emission_factor_table_path Character string specifying the path to emission factor table
#' @param t1 Numeric value specifying the first time period year
#' @param t2 Numeric value specifying the second time period year
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{chg_ptable}{Data frame containing detailed peat emission calculations per land use change}
#'   \item{chg_pdtable}{Data frame containing summarized emissions per administrative unit}
#'   \item{em_map}{SpatRaster object showing spatial distribution of emissions}
#' }
#'
#' @details
#' The function performs the following main steps:
#' \enumerate{
#'   \item Prepares input data including emission factors and spatial layers
#'   \item Creates cross tabulation of land use changes
#'   \item Calculates emissions for each pixel
#'   \item Computes total emissions and generates summary statistics
#' }
#'
#' @note
#' All spatial inputs should be in the same coordinate reference system.
#' The emission factor table should contain columns for ID and Peat values.
#'
#' @import terra
#' @import sf
#' @import data.table
#'
#' @examples
#' \dontrun{
#' results <- run_quesc_peat_analysis(
#'   output_dir = "output/",
#'   lc_t1_path = "data/landcover_2000.tif",
#'   lc_t2_path = "data/landcover_2020.tif",
#'   admin_z_path = "data/admin_zones.shp",
#'   peat_map_path = "data/peat_distribution.shp",
#'   peat_emission_factor_table_path = "data/emission_factors.csv",
#'   t1 = 2000,
#'   t2 = 2020
#' )
#'
#' # Access results
#' chg_ptable <- results$chg_ptable
#' chg_pdtable <- results$chg_pdtable
#' em_map <- results$em_map
#' }
#'
run_quesc_peat_analysis <- function(output_dir, lc_t1_path, lc_t2_path, admin_z_path, 
                                     peat_map_path, peat_emission_factor_table_path, t1, t2) {
  # 1. Data Preparation -----------------------------------------------------
  # Prepare lookup table of peat emission factor
  lookup_c.pt <- read.csv(peat_emission_factor_table_path)
  luc_lut <- lookup_c.pt
  names(lookup_c.pt)[1] <- "ID"
  names(lookup_c.pt)[ncol(lookup_c.pt)] <- "Peat"
  
  # Prepare landcover/use map
  luc_1raw <- rast(lc_t1_path)
  luc_2raw <- rast(lc_t2_path)
  
  # Prepare zone
  zone_sf <- read_shapefile(shp_input = admin_z_path)
  # zone_sf <- admin_z_path %>% st_read()
  zone_sf <- st_cast(zone_sf, "MULTIPOLYGON")
  zone <- zone_sf %>% 
    rasterise_multipolygon_quesc(
      raster_res = res(luc_1raw), 
      field = paste0(colnames(st_drop_geometry(zone_sf[1])))
    )
  pu_table <- data.frame(ID_PU = zone_sf[[1]], PU = zone_sf[[2]])
  zone <- as.factor(zone)
  for (i in 1:nrow(pu_table)) {
    zone <- subst(zone, from = pu_table$PU[i], to = pu_table$ID_PU[i])
  }
  
  luc_1 <- resample(luc_1raw, zone)
  luc_2 <- resample(luc_2raw, zone)
  
  # Prepare peat map
  peat_sf <- read_shapefile(shp_input = peat_map_path)
  # peat_sf <- peat_map_path %>% st_read()
  peat_sf <- st_cast(peat_sf, "MULTIPOLYGON")
  peat_table <- data.frame(ID = peat_sf[[1]])
  peatmap_raw <- peat_sf %>%
    rasterise_multipolygon_quesc(
      raster_res = res(luc_1), 
      field = paste0(colnames(st_drop_geometry(peat_sf[1])))
    )
  peatmap <- resample(peatmap_raw, luc_1)
  
  # Peat reclassification
  rec_value <- peat_table$ID
  rep_value <- 1
  peatmap <- classify(peatmap, matrix(c(rec_value, rep_value), ncol = 2))
  
  # 2. Create Cross Tabulation ----------------------------------------------
  lu_chg <- (zone * 1) + (luc_1 * 100^1) + (luc_2 * 100^2)
  cross_tab <- cross_tabulation(pu_table, luc_lut, zone, luc_1, luc_2, lu_chg)
  chg_db <- cross_tab$lu.db
  landUseChangeMapDummy <- cross_tab$landUseChangeMapDummy
  
  # 3. Calculate Emission Each Pixels ---------------------------------------
  # Subset the landuse change
  chg_ptmap <- lu_chg * peatmap
  chg_ptable <- as.data.frame(freq(chg_ptmap))
  chg_ptable <- chg_ptable[, -1]
  chg_ptable <- na.omit(chg_ptable)
  names(chg_ptable) <- c("ID", "COUNT")
  chg_ptable$HECT <- chg_ptable$COUNT * res(zone)[1]^2 / 10000  # area in hectare
  chg_ptable <- chg_ptable[, c("ID", "HECT")]
  sub.chg_db <- chg_db[chg_db$ID_CHG %in% chg_ptable$ID, !names(chg_db) %in% "COUNT"]
  chg_ptable <- merge(chg_ptable, sub.chg_db, by.x = "ID", by.y = "ID_CHG", all.x = TRUE)
  
  # Merge with the 'lookup_c.pt'
  for (p in 1:2) {
    chg_ptable <- merge(chg_ptable, lookup_c.pt, 
                        by.x = paste0("ID_LC", p), 
                        by.y = "ID", 
                        all.x = TRUE)
    names(chg_ptable)[names(chg_ptable) == "Peat"] <- paste0("EM_F_", eval(parse(text = paste0("t", p))))
  }
  
  # 4. Calculate Total Emission ---------------------------------------------
  # Calculate the total emission of each row
  t_mult <- abs(as.numeric(as.character(t2)) - as.numeric(as.character(t1))) / 2 # multiplier, in year
  chg_ptable$raw_em <- t_mult * eval(parse(text = paste0("chg_ptable$EM_F_", t1, "+ chg_ptable$EM_F_", t2)))
  chg_ptable$em_calc <- chg_ptable$raw_em * chg_ptable$HECT
  names(chg_ptable)[6] <- as.character(t1)
  names(chg_ptable)[8] <- as.character(t2)
  
  # Merge as data.table
  chg_pdtable <- data.table(chg_ptable[, c("ID_PU", "em_calc", "HECT")])
  chg_pdtable <- chg_pdtable[, lapply(.SD, sum), by = list(ID_PU)][!is.na(ID_PU)]
  
  # Emission map: peat area either with emission or not
  em_map <- classify(chg_ptmap, as.matrix(chg_ptable[, c("ID", "raw_em")]))
  
  # Return results
  return(list(
    chg_ptable = chg_ptable,
    chg_pdtable = chg_pdtable,
    em_map = em_map,
    peatmap = peatmap,
    lookup_c.pt = lookup_c.pt
  ))
}

# Pre-QuES Land cover change analysis -------------------------------------

#' @title Pre-QuES Land cover change analysis
#' @description This function preprocesses land cover data for visualization, calculation, and summarization.
#' It checks the consistency of input data, creates visualizations of input data, calculates land cover frequency,
#' creates a crosstabulation of land cover changes, and summarizes results at both the landscape and planning unit level.
#' The function also converts pixels to hectares if `convert_to_Ha` is set to TRUE.
#' @param lc_t1 A spatRaster object representing land cover data for period T1.
#' @param lc_t2 A spatRaster object representing land cover data for period T2.
#' @param admin_ A spatRaster object representing planning unit data. Can also be a sf multipolygon object,
#' in which case it will be rasterized.
#' @param cutoff_landscape minimum number of pixel/ area to be displayed in sankey plot at landscape level
#' @param cutoff_pu minimum number of pixel/ area to be displayed in sankey plot at planing unit level
#' @param convert_to_Ha Logical flag indicating whether to convert pixel count to hectares based on the resolution of the `lc_t1` raster.
#' @return A list of results containing input data visualizations, landscape level results, and planning unit level results.
#' @importFrom terra rast cats compareGeom
#' @importFrom dplyr select group_by_at summarise pull
#' @importFrom purrr map
#' @importFrom methods is
#' @importFrom stats setNames
#' @export
#' @examples
#' \dontrun{
#' # Load and annotate land cover data for period T1
#' lc_t1_ <- LUMENSR_example("NTT_LC90.tif") %>%
#'   terra::rast() %>%
#'   add_legend_to_categorical_raster(
#'     raster_file = .,
#'     lookup_table = lc_lookup_klhk_sequence,
#'     year = 1990)
#'
#' # Load and annotate land cover data for period T2
# lc_t2_ <- LUMENSR_example("NTT_LC20.tif") %>%
#'   terra::rast() %>%
#'   add_legend_to_categorical_raster(
#'     raster_file = .,
#'     lookup_table = lc_lookup_klhk_sequence,
#'    year = 2020)
#'
#' # Load planning unit data
#' admin_z <- LUMENSR_example("ntt_admin_spatraster.tif") %>%
#'   terra::rast()
#' ques_pre(lc_t1_, lc_t2_, admin_z)
#' }

ques_pre_quesc <- function(lc_t1, lc_t2, admin_, cutoff_landscape = 5000, cutoff_pu = 500, convert_to_Ha = TRUE) {
  
  ## Plot planning unit
  if (is(admin_, "SpatRaster")) {
    plot_admin <- plot_categorical_raster(admin_) #%>% ggplot_to_image(image_width = 20, image_height = 14)
    
  } else {
    stopifnot(is(admin_, "sf")) # Ensure admin_ is either SpatRaster or sf (multipolygon)
    plot_admin <- plot_planning_unit(admin_) #%>% ggplot_to_image(image_width = 20, image_height = 14)
    admin_ <- rasterise_multipolygon(admin_) # convert admin_ to a spatraster
  }
  
  # Guardrails to check the input types
  stopifnot(is(lc_t1, "SpatRaster"), is(lc_t2, "SpatRaster"))
  
  # Guardrails to check if the input rasters have attribute tables
  stopifnot(!is.null(cats(lc_t1)[[1]]), !is.null(cats(lc_t2)[[1]]))
  
  # Guardrail to ensure identical extent and projection system
  compareGeom(lc_t1, lc_t2, admin_, stopOnError = TRUE)
  
  # Plot land cover for both periods and the planning unit
  ## Plot land cover T1
  plot_lc_t1 <- plot_categorical_raster(lc_t1) #%>% ggplot_to_image(image_width = 20, image_height = 14)
  ## Plot land cover T2
  plot_lc_t2 <- plot_categorical_raster(lc_t2) #%>% ggplot_to_image(image_width = 20, image_height = 14)
  
  # Calculate and tabulate land cover composition in Hectares
  lc_freq_table <- calc_lc_freq(raster_list = list(lc_t1, lc_t2)) %>%
    abbreviate_by_column( "Jenis tutupan lahan", remove_vowels = FALSE)
  
  
  if (grepl("\\+units=m", st_crs(lc_t1)$proj4string)) {
    spatRes <- calc_res_conv_factor_to_ha(lc_t1)
    lc_freq_table <-
      mutate(lc_freq_table, across(c(2,3), ~(spatRes*.x))) %>% # convert to hectares
      #mutate_if(is.numeric, format, digits=3) %>% 
      
      mutate(across(c(2,3), ~units::set_units(x = .x, value = "ha")))
  } else {
    cat("Frequency is shown in number of pixels isntead of hectares in the lc_freq_table")
  }
  
  lc_composition_tbl <- lc_freq_table
  
  # Plot land cover composition
  # lc_composition_barplot <- lc_freq_table %>% 
  #   plot_lc_freq(column_lc_type = "Land-use/cover types", #"Jenis tutupan lahan",
  #                column_T1 = names(lc_freq_table)[2],
  #                column_T2 = names(lc_freq_table)[3])
  
  # Store visualization results
  input_dataviz <- list(
    plot_lc_t1 = plot_lc_t1,
    plot_lc_t2 = plot_lc_t2,
    plot_admin = plot_admin,
    tbl_lookup_lc_t1 = cats(lc_t1)[[1]],
    tbl_lookup_lc_t2 = cats(lc_t2)[[1]],
    tbl_lookup_admin = cats(admin_)[[1]],
    lc_composition_tbl = lc_composition_tbl
    # lc_composition_barplot = lc_composition_barplot
  )
  # Create crosstabulation
  crosstab_matrix_landscape <- create_crosstab(land_cover = c(lc_t1, lc_t2))[["crosstab_square"]] |>
    as.data.frame.matrix()
  
  crosstab_result <- create_crosstab(land_cover = c(lc_t1, lc_t2), zone = admin_)[["crosstab_long"]]
  
  # Get spatResolution
  if( convert_to_Ha == TRUE) {
    SpatRes <- calc_res_conv_factor_to_ha(raster_input = lc_t1)
    
    crosstab_result <- mutate(crosstab_result, Ha = Freq*SpatRes)
    crosstab_matrix_landscape <- crosstab_result
  }
  
  # Summarize crosstabulation at landscape level
  # Subsetting the crosstab_result data frame
  selected_cols <- select(crosstab_result, -names(admin_))
  
  # Getting the names of the columns to be grouped
  group_cols <- setdiff(names(selected_cols), c("Freq", "Ha"))
  
  # Grouping the data frame by the columns selected above
  grouped_df <- group_by_at(selected_cols, group_cols)
  
  # Summarizing the grouped data
  if ("Ha" %in% names(grouped_df)) {
    crosstab_landscape <- summarise(grouped_df, Freq = sum(.data[["Freq"]]), Ha = sum(.data[["Ha"]]), .groups = "drop")
  } else {
    crosstab_landscape <- summarise(grouped_df, Freq = sum(.data[["Freq"]]), .groups = "drop")
  }
  # Create Sankey diagrams at landscape level
  ## Sankey diagram showing all changes
  # sankey_landscape <- crosstab_landscape %>%
  #   create_sankey(area_cutoff = cutoff_landscape, change_only = FALSE)
  
  ## Sankey diagram showing only changes
  # sankey_landscape_chg_only<- crosstab_landscape %>%
  #   create_sankey(area_cutoff = cutoff_landscape, change_only = TRUE)
  
  # Compute 10 dominant land use changes
  luc_top_10 <- crosstab_landscape %>% calc_top_lcc(n_rows = 10)
  
  # Tabulate and plot 10 dominant land use changes
  luc_top_10_barplot <- luc_top_10 %>%
    plot_lcc_freq_bar(col_T1 = as.character(time(lc_t1)), col_T2 = as.character(time(lc_t2)),
                      Freq = if ("Ha" %in% names(luc_top_10)) "Ha" else "Freq")
  
  # Store results at landscape level
  landscape_level <- list(
    # sankey_landscape= sankey_landscape,
    # sankey_landscape_chg_only = sankey_landscape_chg_only,
    luc_top_10_tbl = luc_top_10,
    luc_top_10_barplot = luc_top_10_barplot,
    crosstab_landscape = crosstab_matrix_landscape,
    crosstab_long = crosstab_result
  )
  
  # Compute summaries for each planning unit
  pu_names <- crosstab_result %>% pull(names(admin_)) %>% unique()
  pu_level <- purrr::map(pu_names, ~ lcc_summary_by_pu(crosstab_tbl = crosstab_result, pu_column = names(admin_), pu_name = .x, sankey_area_cutoff = cutoff_pu, n_top_lcc = 10))
  pu_level <- stats::setNames(pu_level,pu_names)
  
  # Return all results
  return(list(input_dataviz = input_dataviz, landscape_level = landscape_level, pu_level = pu_level))
}
