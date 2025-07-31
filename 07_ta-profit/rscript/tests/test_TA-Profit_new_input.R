library(terra)
library(dplyr)
library(readr)
library(plotly)
library(stringr)
library(purrr)
library(RColorBrewer)
 
# INPUT ####
pathLULCT1 <- "data/raster/bungo_landcover_1990r.tif"
pathLULCT2 <- "data/raster/bungo_landcover_2000r.tif"
valueT1 <- 1990
valueT2 <- 2000
pathLookupCstock <- "data/table/carbon_bungo.csv"
pathPU <- "data/raster/bungo_zone.tif"
pathLookupPU<- "data/table/zone_table_bungo.csv"
pathLookupNPV<- "data/table/profitability_table_bungo.csv"
output_dir <- "C:/users/ykarimah/Downloads/New TA/Dry run LUMENS/"

# Start timing
start_time <- Sys.time()

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
  
  # Extract locale info, limit to first few locales if too many
  locale_info <- strsplit(si$locale, ";")[[1]]
  locale_info <- paste(locale_info, collapse = "<br>")
  
  # Extract .libPaths, limit the number of paths displayed
  lib_paths <- paste(.libPaths(), collapse = "<br>")
  
  # Combine all info into a single tibble
  session_summary <- tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details = c(r_version, platform_os, lib_paths, locale_info)
  )
  
  return(session_summary)
}

easy_to_read_numbers <- scales::label_comma()

# PREPROCESSING DATA #### 
LULCT1 <- rast(pathLULCT1)
LookupCstock <- read_csv(pathLookupCstock)
 
# Get the names of raster_file
name_rast1 <- names(LULCT1)
 
# Set the levels of raster_file to be lookup_table
levels(LULCT1) <- LookupCstock
 
# Set the names of raster_file
LULCT1 <- setNames(LULCT1, name_rast1)
 
# Set the year if year is not NULL
if (!is.null(valueT1)) {
  terra::time(LULCT1, tstep="years") <- valueT1
}
 
LULCT2 <- rast(pathLULCT2)

# Get the names of raster_file
name_rast2 <- names(LULCT2)
 
# Set the levels of raster_file to be lookup_table
levels(LULCT2) <- LookupCstock
 
# Set the names of raster_file
LULCT2 <- setNames(LULCT2, name_rast2)
 
# Set the year if year is not NULL
if (!is.null(valueT2)) {
  terra::time(LULCT2, tstep="years") <- valueT2
}
 
PU <- rast(pathPU)
LookupPU <- read_csv(pathLookupPU)
 
# Get the names of raster_file
name_PU <- names(PU)
name_PU <- "bungo_zone"
 
# Set the levels of raster_file to be lookup_table
levels(PU) <- LookupPU
 
# Set the names of raster_file
PU <- setNames(PU, name_PU)
 
# PU, LULCT1, LULCT2
# harmonise PU extent according to the LULCT1 geometry
PU <- terra::resample(PU, LULCT1, method="near")
 
combinedRaster <- c(PU, LULCT1, LULCT2)

# Calculate pixel area in Ha
res_m <- terra::res(LULCT1) # resolution in meters (width, height)
area_ha_per_pixel <- (res_m[1] * res_m[2]) / 10000  # m² to Ha

# Build the frequency and area table
combinedRasterTable <- combinedRaster %>%
  as_tibble() %>%
  tidyr::drop_na() %>%
  group_by(across(everything())) %>%
  mutate(Freq = n()) %>%
  ungroup() %>%
  distinct() %>%
  mutate(Ha = Freq * area_ha_per_pixel)

colnames(combinedRasterTable)[1:3] <- c("PU", "LC1", "LC2")

LookupNPV <- read_csv(pathLookupNPV)
# LookupNPV$CARBON<-NULL

combinedRasterTable <- combinedRasterTable %>%
  left_join(LookupNPV %>% rename_all(~paste0(., "_LC1")), by = c("LC1" = "LC_LC1")) %>%
  left_join(LookupNPV %>% rename_all(~paste0(., "_LC2")), by = c("LC2" = "LC_LC2"))

combinedRasterTable$NPV1 <- combinedRasterTable$NPV_LC1*combinedRasterTable$Ha
combinedRasterTable$NPV2 <- combinedRasterTable$NPV_LC2*combinedRasterTable$Ha
combinedRasterTable$deltaNPV <- combinedRasterTable$NPV2 - combinedRasterTable$NPV1

# DISTRIBUTION MAP OF NPV1, NPV2, DELTA NPV ####
npv_matrix <- as.matrix(LookupNPV[, c("ID", "NPV")])

npv1_map <- terra::classify(LULCT1, npv_matrix)
npv2_map <- terra::classify(LULCT2, npv_matrix)

deltaNPV_map <- npv2_map - npv1_map

# MAIN OUTPUT FUNCTIONS ####
calculate_total_values <- function(data) {
  data %>%
    summarise(
      Total_NPV1 = sum(NPV1, na.rm = TRUE),
      Total_NPV2 = sum(NPV2, na.rm = TRUE),
      Total_Delta_NPV = sum(deltaNPV, na.rm = TRUE) 
    )
}

dissolve_lc1 <- function(data, top_n = 10) {
  data %>%
    group_by(LC1) %>%
    summarise(
      Total_NPV1 = sum(NPV1, na.rm = TRUE),
      Total_Ha1 = sum(Ha, na.rm = TRUE)) %>%
    arrange(desc(Total_NPV1)) %>%
    slice_head(n = top_n)
}

dissolve_lc2 <- function(data, top_n = 10) {
  data %>%
    group_by(LC2) %>%
    summarise(
      Total_NPV2 = sum(NPV2, na.rm = TRUE),
      Total_Ha2 = sum(Ha, na.rm = TRUE)) %>%
    arrange(desc(Total_NPV2)) %>%
    slice_head(n = top_n)
}

dissolve_lulcc <- function(data, top_n = 10) {
  data %>%
    group_by(LC1, LC2) %>%
    summarise(
      Total_deltaNPV = sum(deltaNPV, na.rm = TRUE),
      Total_abs_deltaNPV = sum(abs(deltaNPV), na.rm = TRUE),
      Total_Ha2 = sum(Ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(LULCC = paste(LC1, "to", LC2)) %>%
    arrange(desc(Total_abs_deltaNPV)) %>%
    slice_head(n = top_n)
}

all_dissolve_lulcc <- function(data) {
  data %>%
    group_by(LC1, LC2) %>%
    summarise(
      Total_deltaNPV = sum(deltaNPV, na.rm = TRUE),
      Total_abs_deltaNPV = sum(abs(deltaNPV), na.rm = TRUE),
      Total_Ha2 = sum(Ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(LULCC = paste(LC1, "to", LC2)) %>%
    arrange(desc(Total_abs_deltaNPV)) 
}

# Visualization Functions
create_lc1_bar <- function(data, title = "Top 10 Total NPV by LC1") {
  plotly::plot_ly(
    data = data,
    x = ~stringr::str_wrap(LC1, width = 25),
    y = ~Total_NPV1,
    type = "bar",
    hoverinfo = "text",
    hovertext = ~paste(
      "Land Cover Class (LC 1):", LC1, "<br>", 
      "Total NPV:", format(Total_NPV1, big.mark = ",", scientific = FALSE)
    ),
    marker = list(
      color = ~Total_NPV1,
      colorscale = "Viridis",
      showscale = FALSE
    )
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = "", categoryorder = "total descending", tickangle = -270),
      yaxis = list(title = "Total NPV", tickformat = ",.0f"),
      margin = list(b = 150),
      hoverlabel = list(bgcolor = "white", font = list(color = "black"))
    )
}

create_lc2_bar <- function(data, title = "Top 10 Total NPV by LC2") {
  plotly::plot_ly(
    data = data,
    x = ~stringr::str_wrap(LC2, width = 25),
    y = ~Total_NPV2,
    type = "bar",
    hoverinfo = "text",
    hovertext = ~paste(
      "Land Cover Class (LC 2):", LC2, "<br>", 
      "Total NPV:", format(Total_NPV2, big.mark = ",", scientific = FALSE)
    ),
    marker = list(
      color = ~Total_NPV2,
      colorscale = "Plasma",
      showscale = FALSE
    )
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = "", categoryorder = "total descending", tickangle = -270),
      yaxis = list(title = "Total NPV", tickformat = ",.0f"),
      margin = list(b = 150),
      hoverlabel = list(bgcolor = "white", font = list(color = "black"))
    )
}

create_lulcc_bar <- function(data, title = "Top 10 LULCC by ΔNPV") {
  data <- data %>%
    arrange(desc(Total_abs_deltaNPV)) %>%
    mutate(LULCC = factor(LULCC, levels = unique(LULCC)))
  
  color_palette <- RColorBrewer::brewer.pal(n = 10, name = "Set3")
  
  plotly::plot_ly(
    data = data,
    x = ~Total_deltaNPV,
    y = ~LULCC,
    type = "bar",
    orientation = "h",
    marker = list(color = color_palette),
    hoverinfo = "text",
    hovertext = ~paste(
      "LULCC:", LULCC, "<br>",
      "ΔNPV:", format(Total_deltaNPV, big.mark = ",", scientific = FALSE), " IDR"
    ),
    showlegend = FALSE
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = "ΔNPV (IDR)", tickformat = ",.0f"),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(levels(data$LULCC))),
      margin = list(l = 150),
      hoverlabel = list(bgcolor = "white", font = list(color = "black"))
    )
}

# OUTPUT PER PLANNING UNIT FUNCTIONS ####

process_pu_data <- function(pu_data, pu_name) {
  # Calculate all metrics
  total_values <- calculate_total_values(pu_data) %>% 
    as.data.frame() %>% 
    rename(
      `Total NPV (Year 1)` = Total_NPV1,
      `Total NPV (Year 2)` = Total_NPV2,
      `Total ΔNPV` = Total_Delta_NPV
    ) %>% 
    t() %>% 
    `colnames<-`("Nilai (Rupiah)") 
  
  dissolved_lc1 <- dissolve_lc1(pu_data, 10)
  dissolved_lc2 <- dissolve_lc2(pu_data, 10)
  dissolved_lulcc <- dissolve_lulcc(pu_data)
  
  # Create plots
  lc1_bar <- create_lc1_bar(dissolved_lc1, paste("Top 10 NPV1 by LC1 in PU:", pu_name))
  lc2_bar <- create_lc2_bar(dissolved_lc2, paste("Top 10 NPV2 by LC2 in PU:", pu_name))
  lulcc_bar <- create_lulcc_bar(dissolved_lulcc, paste("Top 10 ΔNPV in PU:", pu_name))
  
  # Return all results
  list(
    total_values = total_values,
    lc1_bar = lc1_bar,
    lc2_bar = lc2_bar,
    lulcc_bar = lulcc_bar
  )
}

# USAGE EXAMPLE ####

# For main output
main_total_values <- calculate_total_values(combinedRasterTable) %>% 
  as.data.frame() %>% 
  rename(
    `Total NPV (Year 1)` = Total_NPV1,
    `Total NPV (Year 2)` = Total_NPV2,
    `Total ΔNPV` = Total_Delta_NPV
  ) %>% 
  t() %>% 
  `colnames<-`("Nilai (Rupiah)") 
main_dissolved_lc1 <- dissolve_lc1(combinedRasterTable)
main_dissolved_lc2 <- dissolve_lc2(combinedRasterTable) 
main_dissolved_lulcc <- dissolve_lulcc(combinedRasterTable) 
all_dissolved_lulcc <- all_dissolve_lulcc(combinedRasterTable)

main_lc1_bar <- create_lc1_bar(main_dissolved_lc1)
main_lc2_bar <- create_lc2_bar(main_dissolved_lc2)
main_lulcc_bar <- create_lulcc_bar(main_dissolved_lulcc)

# For planning units
pu_list <- unique(combinedRasterTable$PU)
pu_outputs <- list()

for (pu_name in pu_list) {
  pu_data <- combinedRasterTable %>% filter(PU == pu_name)
  pu_outputs[[pu_name]] <- process_pu_data(pu_data, pu_name)
}


# REPORT ####
report_params <- list(
  session_log = format_session_info_table(),
  start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
  end_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  total_table = main_total_values,
  npv1_table = main_dissolved_lc1,
  npv2_table = main_dissolved_lc2,
  deltaNPV_table = main_dissolved_lulcc,
  npv1_chart = main_lc1_bar,
  npv2_chart = main_lc2_bar,
  deltaNPV_chart = main_lulcc_bar,
  map1_file_path = pathLULCT1,
  map2_file_path = pathLULCT2,
  npv_file_path = pathLookupNPV,
  carbon_file_path = pathLookupCstock,
  pu_table_path = pathLookupPU,
  npv1_map = npv1_map,
  npv2_map = npv2_map,
  deltaNPV_map = deltaNPV_map,
  year1 = valueT1,
  year2 = valueT2,
  output_dir = output_dir
)

# Render report
output_file <- paste0("ta-profit_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
rmarkdown::render(
  input = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/07_ta-profit/report_template/ta-profit-manual.Rmd",
  output_file = output_file,
  output_dir = output_dir,
  params = report_params,
  envir = new.env(parent = globalenv())
)
