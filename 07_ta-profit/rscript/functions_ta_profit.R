# Utility Functions
format_session_info_table <- function() {
  si <- sessionInfo()
  
  r_version <- si$R.version[c("major", "minor", "year", "month", "day", "nickname")]
  r_version <- paste0(
    "R ", r_version$major, ".", r_version$minor,
    " (", r_version$year, "-", r_version$month, "-", r_version$day, ")",
    " '", r_version$nickname, "'"
  )
  
  platform_os <- paste(si$platform, "|", si$running)
  locale_info <- strsplit(si$locale, ";")[[1]]
  locale_info <- paste(locale_info, collapse = "<br>")
  lib_paths <- paste(.libPaths(), collapse = "<br>")
  
  session_summary <- tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details = c(r_version, platform_os, lib_paths, locale_info)
  )
  
  return(session_summary)
}

plot_categorical_raster <- function(raster_object) {
  # Check if raster_object has a color_pallete column and it contains hex color codes
  if ("color_palette" %in% names(terra::cats(raster_object)[[1]]) && all(grepl("^#[0-9A-Fa-f]{6}$", terra::cats(raster_object)$color_pallete))) {
    fill_scale <- ggplot2::scale_fill_manual(values = terra::cats(raster_object)[[1]]$color_palette, na.value = "white")
  } else {
    # fill_scale <- ggplot2::scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F","#BAB0AC"), na.value = "white")
    fill_scale <- ggplot2::scale_fill_manual(values = c(
      "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
      "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
      "#86BCB6", "#FFB84D", "#A5C1DC", "#D37295", "#C4AD66",
      "#7B8D8E", "#B17B62", "#8CD17D", "#DE9D9C", "#5A5A5A",
      "#A0A0A0", "#D7B5A6", "#6D9EEB", "#E69F00", "#56B4E9",
      "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
      "#999999", "#E51E10", "#FF7F00", "#FFFF33", "#A65628",
      "#F781BF", "#999933", "#8DD3C7", "#FFFFB3", "#BEBADA",
      "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
      "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#E41A1C"
    ), na.value = "white")
  }
  if (!is.na(terra::time(raster_object))) {
    plot_title <- terra::time(raster_object)
  } else {
    plot_title <- names(raster_object)
  }
  # Generate the plot
  plot_lc <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = raster_object) +
    fill_scale +
    ggplot2::theme_bw() +
    ggplot2::labs(title = plot_title, fill = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", ncol = 3)) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.height = ggplot2::unit(0.25, "cm"),
      legend.key.width = ggplot2::unit(0.25, "cm"),
      legend.position = "bottom",
      legend.justification = c(0, 0.5)
    )
  return(plot_lc)
}

# Helper function for consistent number formatting
easy_to_read_numbers <- scales::label_comma()

# Fungsi untuk transformasi logaritmik dengan penanganan nilai nol/negatif
log_transform <- function(x, base = 10, offset = 1) {
  if (is.numeric(x)) {
    # Handle negative values and zeros by adding offset
    sign(x) * log(abs(x) + offset, base = base)
  } else {
    x
  }
}

# Data Processing Functions
# preprocess_data <- function(pathLULCT1, pathLULCT2, pathPU, 
#                             pathLookupCstock, pathLookupPU, pathLookupNPV,
#                             valueT1, valueT2) {

preprocess_data <- function(pathLULCT1, pathLULCT2, pathPU, 
                            pathLookupPU, pathLookupNPV,
                            valueT1, valueT2) {  
    
  # Load and process LULC T1
  LULCT1 <- terra::rast(pathLULCT1)
  LookupNPV <- readr::read_csv(pathLookupNPV)
  # LookupCstock <- readr::read_csv(pathLookupCstock)
  levels(LULCT1) <- LookupNPV
  LULCT1 <- setNames(LULCT1, "LC1")
  if (!is.null(valueT1)) terra::time(LULCT1, tstep = "years") <- as.numeric(valueT1)
  
  # Load and process LULC T2
  LULCT2 <- terra::rast(pathLULCT2)
  levels(LULCT2) <- LookupNPV
  LULCT2 <- setNames(LULCT2, "LC2")
  if (!is.null(valueT2)) terra::time(LULCT2, tstep = "years") <- as.numeric(valueT2)
  
  # Load and process PU
  PU <- terra::rast(pathPU)
  LookupPU <- readr::read_csv(pathLookupPU)
  levels(PU) <- LookupPU
  PU <- terra::resample(PU, LULCT1, method="near")
  
  # Combine rasters and calculate areas
  combinedRaster <- c(PU, LULCT1, LULCT2)
  res_m <- terra::res(LULCT1)
  area_ha_per_pixel <- (res_m[1] * res_m[2]) / 10000
  
  combinedRasterTable <- combinedRaster %>%
    as_tibble() %>%
    tidyr::drop_na() %>%
    group_by(across(everything())) %>%
    mutate(Freq = n()) %>%
    ungroup() %>%
    distinct() %>%
    mutate(Ha = Freq * area_ha_per_pixel)
  
  colnames(combinedRasterTable)[1:3] <- c("PU", "LC1", "LC2")
  
  LookupCARBON<- read_csv(pathLookupCARBON)
  LookupCARBON <- LookupCARBON %>% dplyr::select(ID = 1, LC = 2, Carbon = 3)
  
  # Join with NPV and CARBON data
  combinedRasterTable <- combinedRasterTable %>%
    # Join NPV lookup for LC1 and LC2
    left_join(LookupNPV %>% rename_all(~paste0(., "_LC1")), by = c("LC1" = "LC_LC1")) %>%
    left_join(LookupNPV %>% rename_all(~paste0(., "_LC2")), by = c("LC2" = "LC_LC2")) %>% 
    # Join Carbon lookup for LC1 and LC2
    left_join(LookupCARBON %>% rename(C_T1 = Carbon), by = c("LC1" = "LC")) %>%
    left_join(LookupCARBON %>% rename(C_T2 = Carbon), by = c("LC2" = "LC")) %>%
    select(-ID.x, -ID.y) %>%
    mutate(
      NPV1 = NPV_LC1 * Ha,
      NPV2 = NPV_LC2 * Ha,
      deltaNPV = NPV2 - NPV1,
      LULCC = paste(LC1, "to", LC2)
    )
  
  total_area <- sum(combinedRasterTable$Ha, na.rm = TRUE)
  
  # Create NPV maps
  npv_matrix <- as.matrix(LookupNPV[, c("ID", "NPV")])
  npv1_map <- terra::classify(LULCT1, npv_matrix)
  npv2_map <- terra::classify(LULCT2, npv_matrix)
  deltaNPV_map <- npv2_map - npv1_map
  
  return(list(
    combinedRasterTable = combinedRasterTable,
    npv1_map = npv1_map,
    npv2_map = npv2_map,
    deltaNPV_map = deltaNPV_map,
    LULCT1 = LULCT1,
    LULCT2 = LULCT2,
    PU = PU,
    total_area = total_area
  ))
}

build_opcost_table <- function(dt_quesc_npv, period, total_area) {
  data_em_sel <- combinedRasterTable
  
  data_em_sel <- within(data_em_sel, {
    em_rate <- ((C_T1 - C_T2) * (Ha * 3.67)) / (total_area * period)
    em_tot <- (C_T1 - C_T2) * 3.67
    opcost <- (NPV2 - NPV1) / em_tot
  })
  
  opcost_tab <- data.frame(
    luchg = data_em_sel$LULCC,
    zone = data_em_sel$PU,
    opcost = data_em_sel$opcost,
    emrate = data_em_sel$em_rate,
    area = data_em_sel$Ha
  ) %>%
    filter(!is.nan(opcost), !is.na(opcost))
  
  opcost_tab_p <- opcost_tab[opcost_tab$opcost >= 0, ]
  opcost_tab_p <- opcost_tab_p[order(opcost_tab_p$opcost), ]
  opcost_tab_p$cum_emrate <- cumsum(opcost_tab_p$emrate)
  opcost_tab_p$opcost_log <- log10(opcost_tab_p$opcost)
  is.na(opcost_tab_p) <- sapply(opcost_tab_p, is.infinite)
  opcost_tab_p[is.na(opcost_tab_p)] <- 0
  
  opcost_tab_n <- opcost_tab[opcost_tab$opcost < 0, ]
  opcost_tab_n <- opcost_tab_n[order(opcost_tab_n$opcost), ]
  opcost_tab_n$cum_emrate <- cumsum(opcost_tab_n$emrate)
  opcost_tab_n$opcost_log <- log10(-1 * opcost_tab_n$opcost) * -1
  
  opcost_all <- rbind(opcost_tab_n, opcost_tab_p)
  opcost_all$cum_emrate2 <- as.factor(opcost_all$cum_emrate)
  list(opcost_all = opcost_all)
}

# Analysis Functions
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
create_lc1_bar <- function(data, title = "Top 10 Total NPV by LC1", currency = "IDR") {
  plotly::plot_ly(
    data = data,
    x = ~stringr::str_wrap(LC1, width = 25),
    y = ~Total_NPV1,
    type = "bar",
    hoverinfo = "text",
    hovertext = ~paste(
      "Land Cover Class (LC 1):", LC1, "<br>", 
      "Total NPV:", format(Total_NPV1, big.mark = ",", scientific = FALSE), " ", currency
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
      yaxis = list(title = paste("Total NPV (", currency, ")"), type="log"),
      margin = list(b = 150),
      hoverlabel = list(bgcolor = "white", font = list(color = "black"))
    )
}

create_lc2_bar <- function(data, title = "Top 10 Total NPV by LC2", currency = "IDR") {
  plotly::plot_ly(
    data = data,
    x = ~stringr::str_wrap(LC2, width = 25),
    y = ~Total_NPV2,
    type = "bar",
    hoverinfo = "text",
    hovertext = ~paste(
      "Land Cover Class (LC 2):", LC2, "<br>", 
      "Total NPV:", format(Total_NPV2, big.mark = ",", scientific = FALSE), " ", currency
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
      yaxis = list(title = paste("Total NPV (", currency, ")"), type="log"),
      margin = list(b = 150),
      hoverlabel = list(bgcolor = "white", font = list(color = "black"))
    )
}

create_lulcc_bar <- function(data, title = "Top 10 LULCC by ΔNPV", currency = "IDR") {
  data <- data %>%
    arrange(desc(Total_abs_deltaNPV)) %>%
    mutate(LULCC = factor(LULCC, levels = unique(LULCC)),
           positive = ifelse(Total_deltaNPV > 0, Total_deltaNPV, 0),
           negative = ifelse(Total_deltaNPV < 0, Total_deltaNPV, 0))
  
  plotly::plot_ly(data = data) %>%
    add_bars(x = ~positive, y = ~LULCC, name = "Positive ΔNPV", 
             marker = list(color = "lightgreen"), orientation = "h") %>%
    add_bars(x = ~negative, y = ~LULCC, name = "Negative ΔNPV", 
             marker = list(color = "red"), orientation = "h") %>%
    layout(
      title = title,
      xaxis = list(title = paste("ΔNPV (", currency, ")")),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(levels(data$LULCC))),
      barmode = "relative",
      margin = list(l = 150),
      hoverlabel = list(bgcolor = "white", font = list(color = "black"))
    )
}

# Planning Unit Analysis
process_pu_data <- function(pu_data, pu_name, currency = "IDR") {
  # Helper function to format column names with currency
  format_currency_col <- function(col_name, currency) {
    paste0(col_name, " (", currency, ")")
  }
  
  total_values <- calculate_total_values(pu_data) %>% 
    as.data.frame() %>% 
    rename(
      !!format_currency_col("Total NPV (Year 1)", currency) := Total_NPV1,
      !!format_currency_col("Total NPV (Year 2)", currency) := Total_NPV2,
      !!format_currency_col("Total ΔNPV", currency) := Total_Delta_NPV
    ) %>% 
    t() %>% 
    `colnames<-`("Value")
  
  dissolved_lc1 <- dissolve_lc1(pu_data, 10)
  dissolved_lc2 <- dissolve_lc2(pu_data, 10)
  dissolved_lulcc <- dissolve_lulcc(pu_data, 10)
  all_dissolved_lulcc_pu <- all_dissolve_lulcc(pu_data)
  
  lc1_bar <- create_lc1_bar(dissolved_lc1, paste("Top 10 NPV by LC 1 in PU:", pu_name), currency)
  lc2_bar <- create_lc2_bar(dissolved_lc2, paste("Top 10 NPV by LC 2 in PU:", pu_name), currency)
  lulcc_bar <- create_lulcc_bar(dissolved_lulcc, paste("Top 10 ΔNPV in PU:", pu_name), currency)
  
  list(
    total_values = total_values,
    lc1_bar = lc1_bar,
    lc2_bar = lc2_bar,
    lulcc_bar = lulcc_bar,
    all_dissolved_lulcc_pu = all_dissolved_lulcc_pu,
    currency = currency  # Store currency for later use
  )
}

# Report Generation - Modified version
generate_report_params <- function(data, maps, paths, times, output_dir, pu_outputs, currency) {
  # Helper function to format column names with currency
  format_currency_col <- function(col_name, currency) {
    paste0(col_name, " (", currency, ")")
  }
  
  main_total_values <- calculate_total_values(data$combinedRasterTable) %>% 
    as.data.frame() %>% 
    rename(
      !!format_currency_col("Total NPV (Year 1)", currency) := Total_NPV1,
      !!format_currency_col("Total NPV (Year 2)", currency) := Total_NPV2,
      !!format_currency_col("Total ΔNPV", currency) := Total_Delta_NPV
    ) %>% 
    t() %>% 
    `colnames<-`("Value")
  
  main_dissolved_lc1 <- dissolve_lc1(data$combinedRasterTable)
  main_dissolved_lc2 <- dissolve_lc2(data$combinedRasterTable)
  main_dissolved_lulcc <- dissolve_lulcc(data$combinedRasterTable)
  all_dissolved_lulcc <- all_dissolve_lulcc(data$combinedRasterTable)
  
  list(
    session_log = format_session_info_table(),
    start_time = format(times$start_time, "%Y-%m-%d %H:%M:%S"),
    end_time = format(times$end_time, "%Y-%m-%d %H:%M:%S"),
    total_table = main_total_values,
    npv1_table = main_dissolved_lc1,
    npv2_table = main_dissolved_lc2,
    deltaNPV_table = all_dissolved_lulcc,
    npv1_chart = create_lc1_bar(main_dissolved_lc1, currency = currency),
    npv2_chart = create_lc2_bar(main_dissolved_lc2, currency = currency),
    deltaNPV_chart = create_lulcc_bar(main_dissolved_lulcc, currency = currency),
    LULCT1 = maps$LULCT1,
    LULCT2 = maps$LULCT2,
    PU = maps$PU,
    map1_file_path = paths$pathLULCT1,
    map2_file_path = paths$pathLULCT2,
    pu_file_path = paths$pathPU,
    npv_file_path = paths$pathLookupNPV,
    pu_table_path = paths$pathLookupPU,
    npv1_map = maps$npv1_map,
    npv2_map = maps$npv2_map,
    deltaNPV_map = maps$deltaNPV_map,
    year1 = times$valueT1,
    year2 = times$valueT2,
    pu_outputs = pu_outputs,
    output_dir = output_dir,
    currency = currency
  )
}
