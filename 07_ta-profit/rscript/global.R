### Required Library ####
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

install_load(
  "bslib",
  "dplyr",
  "foreign",
  "ggplot2",
  "plotly",
  "purrr",
  "raster",
  "readr",
  "remote",
  "reshape",
  "reshape2",
  "rmarkdown",
  "sf",
  "shiny",
  "shinyFiles",
  "shinyvalidate",
  "splitstackshape",
  "terra"
)

if (!("LUMENSR" %in% rownames(installed.packages()))) {
  install_github("icraf-indonesia/LUMENSR")
  do.call("library", list("LUMENSR"))
}
library(LUMENSR)

prepare_npv_lookup <- function(tbl_npv, quesc_tbl) {
  lookup_n <- tbl_npv
  colnames(lookup_n)[1] <- "ID_LC1"
  colnames(lookup_n)[2] <- "NPV1"
  quesc_tbl <- merge(quesc_tbl, lookup_n, by = "ID_LC1")
  colnames(lookup_n)[1] <- "ID_LC2"
  colnames(lookup_n)[2] <- "NPV2"
  quesc_tbl <- merge(quesc_tbl, lookup_n, by = "ID_LC2")
  
  tot_area <- sum(quesc_tbl$Ha)
  
  list(quesc_tbl = quesc_tbl, tot_area = tot_area)
}

build_opcost_table <- function(quesc_tbl, period, tot_area) {
  data_em_sel <- quesc_tbl[quesc_tbl$EM > 0, ]
  data_em_sel <- within(data_em_sel, {
    em_rate <- ((C_T1 - C_T2) * (Ha * 3.67)) / (tot_area * period)
    em_tot <- (C_T1 - C_T2) * 3.67
    sq_rate <- ((C_T1 - C_T2) * (Ha * 3.67)) / (tot_area * period)
    sq_tot <- (C_T1 - C_T2) * 3.67
    opcost <- (NPV1 - NPV2) / em_tot
    opcost_sq <- (NPV1 - NPV2) / sq_tot
    cumsum_em <- cumsum(em_rate)
    cumsum_sq <- cumsum(sq_rate)
  })
  
  opcost_tab <- data.frame(
    luchg = data_em_sel$LU_CHG,
    zone = data_em_sel$PU,
    opcost = data_em_sel$opcost,
    emrate = data_em_sel$em_rate
  )
  
  #Build Positive Opcost Table
  opcost_tab_p<- opcost_tab[ which(opcost_tab$opcost >= 0),]
  opcost_tab_p<- opcost_tab_p[order(opcost_tab_p$opcost),]
  opcost_tab_p$cum_emrate<-cumsum(opcost_tab_p$emrate)
  opcost_tab_p$opcost_log<-log10(opcost_tab_p$opcost)
  is.na(opcost_tab_p) <- sapply(opcost_tab_p, is.infinite)
  opcost_tab_p[is.na(opcost_tab_p)] <- 0

  #Build Negative Opcost Table
  opcost_tab_n<- opcost_tab[ which(opcost_tab$opcost < 0),]
  opcost_tab_n<- opcost_tab_n[order(opcost_tab_n$opcost),]
  opcost_tab_n$cum_emrate<-cumsum(opcost_tab_n$emrate)
  opcost_tab_n$opcost_log<-opcost_tab_n$opcost*-1
  opcost_tab_n$opcost_log<-log10(opcost_tab_n$opcost_log)*-1

  opcost_all <- rbind(opcost_tab_n, opcost_tab_p)
  opcost_all$cum_emrate2 <- as.factor(opcost_all$cum_emrate)
  
  list(opcost_all = opcost_all)
}

carbon_accounting <- function(map1_rast, map2_rast, tbl_npv, tbl_carbon, raster_nodata) {
  NAvalue(map1_rast) <- as.numeric(raster_nodata)
  NAvalue(map2_rast) <- as.numeric(raster_nodata)
  
  names(tbl_carbon)[names(tbl_carbon) == "ID"] <- "ID_LC"
  merged_data <- merge(tbl_npv, tbl_carbon, by = "ID_LC")
  reclassify_matrix <- as.matrix(merged_data[, c("ID_LC", "Carbon")])
  
  map_carbon1 <- reclassify(map1_rast, reclassify_matrix)
  map_carbon2 <- reclassify(map2_rast, reclassify_matrix)
  
  chk_em <- map_carbon1 > map_carbon2
  emission_map <- ((map_carbon1 - map_carbon2) * 3.67) * chk_em
  
  list(map_carbon1 = map_carbon1, map_carbon2 = map_carbon2, emission_map = emission_map)
}

npv_accounting <- function(map1_rast, map2_rast, tbl_npv) {
  npv_matrix <- as.matrix(tbl_npv[, c("ID_LC", "NPV")])
  
  map_npv1 <- reclassify(map1_rast, npv_matrix)
  map_npv2 <- reclassify(map2_rast, npv_matrix)
  
  npv_chg_map <- map_npv2 - map_npv1
  
  list(map_npv1 = map_npv1, map_npv2 = map_npv2, npv_chg_map = npv_chg_map)
}

calculate_opcost_map <- function(npv_chg_map, emission_map) {
  opcost_map <- npv_chg_map / emission_map
  opcost_map
}

generate_output_maps <- function(map_carbon1, map_carbon2, emission_map, opcost_map, wd) {
  writeRaster(map_carbon1, file.path(wd, "carbon_map_t1.tif"), overwrite = TRUE)
  writeRaster(map_carbon2, file.path(wd, "carbon_map_t2.tif"), overwrite = TRUE)
  writeRaster(emission_map, file.path(wd, "emission_map.tif"), overwrite = TRUE)
  writeRaster(opcost_map, file.path(wd, "opcost_map.tif"), overwrite = TRUE)
}

generate_opportunity_cost_curve <- function(opcost_table) {
  # Prepare data frame for the curve
  df_curve <- data.frame(
    emission = opcost_table$emrate,
    opportunity_cost = opcost_table$opcost,
    land_use_change = opcost_table$luchg
  )
  
  # Group data by land use change
  df_grouped <- df_curve %>%
    group_by(land_use_change) %>%
    summarise(emission = sum(emission),
              opportunity_cost = sum(opportunity_cost))
  
  # Filter and order data
  df_all <- df_grouped %>% filter(opportunity_cost != 0)
  df_order <- df_all[order(df_all$opportunity_cost),]
  df_order$order <- seq_len(nrow(df_order))
  
  # Create the Opportunity Cost Curve
  opcost_curve <- ggplot(df_order, aes(x = order, y = opportunity_cost)) +
    labs(x = NULL,
         y = "Opportunity Cost ($/ton CO2-eq)",
         title = "Waterfall Plot for Opportunity Cost") +
    theme_classic() %+replace%
    theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_text(face = "bold", angle = 90)) +
    coord_cartesian(ylim = c(-5000, 5000)) +
    geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.4))
  
  return(opcost_curve)
}
