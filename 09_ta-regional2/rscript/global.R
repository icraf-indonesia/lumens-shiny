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

generate_landuse_table <- function(land_req, projected_land_use, landuse_area0, landuse_lut) {
  load(land_req)
  next_luc_freq <- freq(projected_land_use)
  landuse_area_table <- as.data.frame(na.omit(next_luc_freq))
  colnames(landuse_area_table) <- c("ID", "COUNT")
  landuse_area <- as.matrix(landuse_area_table$COUNT)
  
  landuse_area0_freq <- freq(landuse_area0)
  landuse_area0_table <- as.data.frame(na.omit(landuse_area0_freq))
  colnames(landuse_area0_table) <- c("ID", "COUNT")
  landuse_area0 <- as.matrix(landuse_area0_table$COUNT)
  
  names(landuse_lut) <- as.character(landuse_lut[1,])
  landuse_lut <- landuse_lut[-1,]
  lc_list <- subset(landuse_lut, select = c(ID, LC))
  
  landuse_table <- merge(lc_list, landuse_area0_table, by = "ID")
  landuse_table <- cbind(landuse_table, landuse_area)
  landuse_table$LC <- NULL
  colnames(landuse_table)[1] <- "LAND_USE"
  colnames(landuse_table)[2] <- "T1_HA"
  colnames(landuse_table)[3] <- "T2_HA"
  landuse_table$CHANGE <- landuse_table$T2_HA - landuse_table$T1_HA
  
  return(landuse_table)
}

generate_land_use_change_graph <- function(landuse_table) {
  LC_graph <- ggplot(data = landuse_table, aes(x = LAND_USE, y = CHANGE)) +
    geom_bar(colour = "black", stat = "identity", position = "dodge") +
    guides(fill = FALSE) + xlab("Land use") + ylab("Change") +
    ggtitle("Land Use Change") + theme(axis.text.x = element_text(angle = 90, size = 6))
  
  return(LC_graph)
}

model_final_demand_and_gdp <- function(land_distribution_prop, landuse_area_diag, land_requirement_table, fin_dem, int_con, Leontief, GDP_val, demand, GDP) {
  land_distribution_scen <- land_distribution_prop %*% landuse_area_diag
  land_requirement_scen <- rowSums(land_distribution_scen)
  
  fin_dem_rtot <- rowSums(fin_dem)
  int_con_rtot <- rowSums(int_con)
  demand <- fin_dem_rtot + int_con_rtot
  
  land_requirement_coeff <- land_requirement_table$LRC
  land_productivity_coeff <- land_requirement_table$LPC
  fin_dem_scen <- land_requirement_scen / land_productivity_coeff
  fin_dem_scen[is.infinite(fin_dem_scen)] <- 0
  fin_dem_scen[is.na(fin_dem_scen)] <- 0
  
  # Final Demand and GDP Calculation
  fin_output_scen <- Leontief %*% fin_dem_scen
  fin_output_scen <- round(fin_output_scen, digits = 1)
  colnames(fin_output_scen)[1] <- "OUTPUT_Scen"
  
  GDP_prop_from_output <- GDP_val / demand
  GDP_prop_from_output[is.na(GDP_prop_from_output)] <- 0
  GDP_scen <- GDP_prop_from_output * fin_output_scen
  GDP_scen <- round(GDP_scen, digits = 1)
  GDP_scen[is.na(GDP_scen)] <- 0
  colnames(GDP_scen)[1] <- "GDP_scen"
  
  GDP_diff <- GDP_scen - GDP$GDP
  GDP_diff <- round(GDP_diff, digits = 1)
  colnames(GDP_diff)[1] <- "GDP_diff"
  
  GDP_rate <- GDP_diff / GDP_val
  GDP_rate[is.na(GDP_rate)] <- 0
  GDP_rate <- round(GDP_rate, digits = 2)
  colnames(GDP_rate)[1] <- "GDP_rate"
  
  GDP_summary <- cbind(GDP, GDP_scen, fin_output_scen, GDP_diff, GDP_rate)
  GDP_summary$P_OUTPUT <- NULL
  GDP_summary$P_GDP <- NULL
  
  return(GDP_summary)
}

generate_gdp_graph <- function(GDP_summary) {
  order_GDP_scen <- as.data.frame(GDP_summary[order(-GDP_summary$GDP_scen),])
  order_GDP_scen10 <- head(order_GDP_scen, n = 20)
  GDP_summary_melt <- melt(data = order_GDP_scen10, id.vars = c('SECTOR'), measure.vars = c('GDP', 'GDP_scen'))
  
  GDP_graph <- ggplot(data = GDP_summary_melt, aes(x = SECTOR, y = value, fill = variable)) +
    geom_bar(colour = "black", stat = "identity", position = "dodge") +
    guides(fill = FALSE) + xlab("Sectors") + ylab("GDP") +
    ggtitle("Comparison of GDP Baseline and Scenario") +
    theme(axis.text.x = element_text(angle = 90, size = 6))
  
  return(GDP_graph)
}

calculate_labour_impact <- function(Lab_multiplier, fin_output_scen, labour, sector) {
  Labour_table <- Lab_multiplier
  Labour_table$Lab.multiplier <- as.numeric(format(Labour_table$Lab.multiplier, digits = 3, width = 5))
  Labour_table <- cbind(Labour_table, fin_output_scen)
  Labour_table <- cbind(Labour_table, labour)
  colnames(Labour_table)[1] <- "SECTOR"
  colnames(Labour_table)[2] <- "CATEGORY"
  colnames(Labour_table)[4] <- "OUT_scen"
  colnames(Labour_table)[5] <- "Lab_base"
  
  Labour_table$Lab_scen <- round(Labour_table$Lab.multiplier * Labour_table$OUT_scen * 1000000, digits = 0)
  Labour_table$Lab_req <- Labour_table$Lab_scen - Labour_table$Lab_base
  
  test2 <- cbind(sector, Labour_table$Lab_req)
  
  return(list(Labour_table = Labour_table, Labour_impact = test2))
}

generate_labour_graph <- function(Labour_impact) {
  LAB_graph <- ggplot(data = Labour_impact, aes(x = SECTOR, y = Labour_impact[, 2], fill = CATEGORY)) +
    geom_bar(colour = "black", stat = "identity", position = "dodge") +
    guides(fill = FALSE) + xlab("Sector") + ylab("Labour requirement") +
    ggtitle("Impact of LU Change to Labour") + theme(axis.text.x = element_text(angle = 90, size = 6))
  
  return(LAB_graph)
}