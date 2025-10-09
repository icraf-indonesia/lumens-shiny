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

# Data Processing Functions
preprocess_data <- function(pathLULCT1, pathLULCT2, pathPU, 
                            pathLookupPU, pathLookupNPV, pathLookupCstock,
                            valueT1, valueT2) {  
  
  # Load and process LULC T1
  LULCT1 <- terra::rast(pathLULCT1)
  LookupNPV <- readr::read_csv(pathLookupNPV)
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
  
  LookupCstock <- readr::read_csv(pathLookupCstock)
  LookupCstock <- LookupCstock %>% dplyr::select(ID = 1, LC = 2, Carbon = 3)
  
  # Join with NPV and CARBON data
  combinedRasterTable <- combinedRasterTable %>%
    # Join NPV lookup for LC1 and LC2
    left_join(LookupNPV %>% rename_all(~paste0(., "_LC1")), by = c("LC1" = "LC_LC1")) %>%
    left_join(LookupNPV %>% rename_all(~paste0(., "_LC2")), by = c("LC2" = "LC_LC2")) %>% 
    # Join Carbon lookup for LC1 and LC2
    left_join(LookupCstock %>% rename(C_T1 = Carbon), by = c("LC1" = "LC")) %>%
    left_join(LookupCstock %>% rename(C_T2 = Carbon), by = c("LC2" = "LC")) %>%
    select(-ID.x, -ID.y) %>%
    mutate(
      NPV1 = NPV_LC1 * Ha,
      NPV2 = NPV_LC2 * Ha,
      deltaNPV = NPV2 - NPV1,
      LULCC = paste(LC1, "to", LC2)
    )
  
  total_area <- sum(combinedRasterTable$Ha, na.rm = TRUE)
  period <- as.numeric(valueT2) - as.numeric(valueT1)
  
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
    total_area = total_area,
    period = period
  ))
}

build_opcost_table <- function(dt_quesc_npv, period, total_area) {
  
  data_em_sel <- dt_quesc_npv
  
  data_em_sel <- data_em_sel %>%
    mutate(
      em_rate = ((C_T1 - C_T2) * (Ha * 3.67)) / (total_area * period),
      em_tot  = (C_T1 - C_T2) * 3.67,
      opcost  = ifelse(em_tot != 0, (NPV2 - NPV1) / em_tot, NA)
    )
  
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

#----------------------------------------------------------
# 1. Prepare base data
#----------------------------------------------------------
prepare_curve_data <- function(opcost_table) {
  df_curve <- data.frame(
    emission_rate = opcost_table$emrate,
    opportunity_cost = opcost_table$opcost,
    log_opportunity_cost = opcost_table$opcost_log,
    land_use_change = opcost_table$luchg,
    planning_unit = opcost_table$zone,
    area = opcost_table$area
  )
  return(df_curve)
}

#----------------------------------------------------------
# 2. Group and transform for main abatement curve
#----------------------------------------------------------
build_grouped_data <- function(df_curve) {
  print("Column names in df_curve:")
  print(colnames(df_curve))
  
  df_grouped <- df_curve %>%
    group_by(land_use_change) %>% 
    summarise(
      emission_rate = sum(emission_rate),
      opportunity_cost = sum(opportunity_cost),
      .groups = "drop"
    ) %>%
    filter(opportunity_cost != 0) %>%
    mutate(
      opportunity_cost_log = case_when(
        opportunity_cost > 0 ~ log10(opportunity_cost),
        opportunity_cost < 0 ~ -log10(abs(opportunity_cost)),
        TRUE ~ 0
      )
    ) %>%
    arrange(opportunity_cost_log)
  
  return(df_grouped)
}

#----------------------------------------------------------
# 3. Split into positive and negative emissions
#----------------------------------------------------------
split_emission_direction <- function(df_s) {
  df_pos <- df_s %>%
    filter(emission_rate >= 0) %>%
    mutate(
      xmin = lag(cumsum(emission_rate), default = 0),
      xmax = cumsum(emission_rate)
    )
  
  df_neg <- df_s %>%
    filter(emission_rate < 0) %>%
    mutate(
      xmax = lag(cumsum(emission_rate), default = 0),
      xmin = cumsum(emission_rate)
    )
  
  df_split <- bind_rows(df_pos, df_neg)
  
  return(df_split)
}

#----------------------------------------------------------
# 4. Calculate dominance per planning unit
#----------------------------------------------------------
calculate_pu_dominance <- function(df_curve) {
  df_pu_dominance <- df_curve %>%
    group_by(land_use_change, planning_unit) %>%
    summarise(total_area = sum(area), .groups = "drop") %>%
    group_by(land_use_change) %>%
    mutate(
      land_use_total_area = sum(total_area),
      pct_of_largest_pu = total_area / land_use_total_area
    ) %>%
    slice_max(total_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(land_use_change, planning_unit, pct_of_largest_pu)
  
  return(df_pu_dominance)
}

#----------------------------------------------------------
# 5. Combine and prepare final dataset
#----------------------------------------------------------
prepare_final_dataset <- function(df_split, df_pu_dominance) {
  df_s_final <- df_split %>%
    left_join(df_pu_dominance, by = "land_use_change") %>%
    mutate(
      hover_text = paste0(
        "Perubahan Lahan: ", land_use_change, "<br>",
        "Opportunity Cost: ", scales::comma(opportunity_cost), "<br>",
        "Laju Emisi: ", scales::comma(emission_rate), "<br>",
        "Dominasi Unit Perencanaan: ", planning_unit, " (", scales::percent(pct_of_largest_pu, accuracy = 0.1), ")"
      )
    )
  return(df_s_final)
}

#----------------------------------------------------------
# 6. Plot Abatement Curve (Main)
#----------------------------------------------------------
plot_abatement_curve <- function(df_s_final, currency) {
  p <- ggplot(df_s_final) +
    geom_rect(aes(
      xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost_log,
      fill = land_use_change,
      text = hover_text
    ), color = "black") +
    labs(
      x = "Laju Emisi (ton CO<sub>2</sub>-eq/ha.tahun)",
      y = paste0("Opportunity Cost (", currency, "/ton CO<sub>2</sub>-eq)"),
      title = "Kurva Abatement Cost"
    ) +
    scale_x_continuous(
      breaks = function(x) pretty(x, n = 6),
      labels = function(x) scales::comma(x, accuracy = 0.01)
    ) +
    scale_y_continuous(
      breaks = function(x) floor(min(x)):ceiling(max(x)),
      labels = function(x) {
        values <- ifelse(x >= 0, 10^x, -10^abs(x))
        ifelse(values == floor(values),
               scales::comma(values, accuracy = 1),
               scales::comma(values))
      }
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplotly(p, tooltip = "text")
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
    currency = currency 
  )
}

#----------------------------------------------------------
# 7. Per-Planning Unit Processing
#----------------------------------------------------------
process_unit <- function(df) {
  df_pos <- df %>%
    filter(emission_rate >= 0) %>%
    arrange(opportunity_cost_log) %>%
    mutate(
      xmin = lag(cumsum(emission_rate), default = 0),
      xmax = cumsum(emission_rate)
    )
  
  df_neg <- df %>%
    filter(emission_rate < 0) %>%
    arrange(opportunity_cost_log) %>%
    mutate(
      xmax = lag(cumsum(emission_rate), default = 0),
      xmin = cumsum(emission_rate)
    )
  
  bind_rows(df_pos, df_neg) %>%
    mutate(
      hover_text = paste0(
        "Perubahan Lahan: ", land_use_change, "<br>",
        "Opportunity Cost: ", scales::comma(opportunity_cost), "<br>",
        "Laju Emisi: ", scales::comma(emission_rate)
      )
    )
}

#----------------------------------------------------------
# 8. Generate all plots per planning unit
#----------------------------------------------------------
generate_plots_by_pu <- function(df_curve, currency) {
  df_pu <- df_curve %>%
    filter(opportunity_cost != 0) %>%
    mutate(
      opportunity_cost_log = case_when(
        opportunity_cost > 0 ~ log10(opportunity_cost),
        opportunity_cost < 0 ~ -log10(abs(opportunity_cost)),
        TRUE ~ 0
      )
    )
  
  df_pu_processed <- df_pu %>%
    group_split(planning_unit) %>%
    purrr::map_df(process_unit)
  
  plots_list <- df_pu_processed %>%
    split(.$planning_unit) %>%
    purrr::map(~ {
      p <- ggplot(.x) +
        geom_rect(aes(
          xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost_log,
          fill = land_use_change,
          text = hover_text
        ), color = "black") +
        labs(
          x = "Laju Emisi (ton CO<sub>2</sub>-eq/ha.tahun)",
          y = paste0("Opportunity Cost (", currency, "/ton CO<sub>2</sub>-eq)"),
          title = paste("Kurva Abatement Cost -", unique(.x$planning_unit))
        ) +
        scale_x_continuous(
          breaks = function(x) pretty(x, n = 6),
          labels = function(x) scales::comma(x, accuracy = 0.01)
        ) +
        scale_y_continuous(
          breaks = function(x) floor(min(x)):ceiling(max(x)),
          labels = function(x) {
            values <- ifelse(x >= 0, 10^x, -10^abs(x))
            ifelse(values == floor(values),
                   scales::comma(values, accuracy = 1),
                   scales::comma(values))
          }
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p, tooltip = "text")
    })
  
  return(plots_list)
}

generate_abatement_outputs <- function(opcost_table, currency) {
  df_curve <- prepare_curve_data(opcost_table)
  df_grouped <- build_grouped_data(df_curve)
  df_split <- split_emission_direction(df_grouped)
  df_pu_dominance <- calculate_pu_dominance(df_curve)
  df_final <- prepare_final_dataset(df_split, df_pu_dominance)
  
  main_plot <- plot_abatement_curve(df_final, currency)
  pu_plots <- generate_plots_by_pu(df_curve, currency)
  
  return(list(main_plot = main_plot, pu_plots = pu_plots))
}

# Report Generation - Modified version with Abatement Curve integration
generate_report_params <- function(data, maps, paths, times, output_dir, pu_outputs, currency) {

  # Helper function to format column names with currency
  format_currency_col <- function(col_name, currency) {
    paste0(col_name, " (", currency, ")")
  }
  
  # --- Main NPV Summary Tables ---
  main_total_values <- calculate_total_values(data$combinedRasterTable) %>% 
    as.data.frame() %>% 
    rename(
      !!format_currency_col("Total NPV (Year 1)", currency) := Total_NPV1,
      !!format_currency_col("Total NPV (Year 2)", currency) := Total_NPV2,
      !!format_currency_col("Total ΔNPV", currency) := Total_Delta_NPV
    ) %>% 
    t() %>% 
    `colnames<-`("Value")
  
  # --- Dissolved Layers for Visualization ---
  main_dissolved_lc1 <- dissolve_lc1(data$combinedRasterTable)
  main_dissolved_lc2 <- dissolve_lc2(data$combinedRasterTable)
  main_dissolved_lulcc <- dissolve_lulcc(data$combinedRasterTable)
  all_dissolved_lulcc <- all_dissolve_lulcc(data$combinedRasterTable)
  
  # --- Generate Abatement Curve Outputs ---
  opcost_results <- build_opcost_table(data$combinedRasterTable, data$period, data$total_area)
  opcost_table <- opcost_results$opcost_all
  abatement_outputs <- generate_abatement_outputs(opcost_table, currency)
  
  # --- Return All Parameters for Report Rendering ---
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
    currency = currency,
    abatement_main_plot = abatement_outputs$main_plot,
    abatement_pu_plots = abatement_outputs$pu_plots
  )
}