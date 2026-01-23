#' Format R Session Information Table
#'
#' Creates a summary table containing R version, platform, library paths, and locale settings.
#'
#' @return A tibble with two columns: `Category` and `Details`, containing formatted session info.
#' @examples
#' format_session_info_table()
#' @export
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

#' Plot Categorical Raster
#'
#' Generates a ggplot for categorical raster data, with either predefined or extracted color palettes.
#'
#' @param raster_object A `SpatRaster` object containing categorical values.
#'
#' @return A `ggplot` object visualizing the raster with color legend.
#' @examples
#' plot_categorical_raster(my_raster)
#' @export
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

#' Preprocess Land Use and Planning Unit Data
#'
#' Loads and prepares raster and lookup data for NPV and carbon stock analysis.
#'
#' @param pathLULCT1 File path to land-use/cover raster for time 1.
#' @param pathLULCT2 File path to land-use/cover raster for time 2.
#' @param pathPU File path to planning unit raster.
#' @param pathLookupPU File path to planning unit lookup table (CSV).
#' @param pathLookupNPV File path to NPV lookup table (CSV).
#' @param pathLookupCstock File path to carbon stock lookup table (CSV).
#' @param valueT1 Numeric year for time 1.
#' @param valueT2 Numeric year for time 2.
#'
#' @return A list containing processed raster layers, lookup tables, and combined summary data:
#' \itemize{
#'   \item combinedRasterTable — summarized data table with NPV and carbon info
#'   \item npv1_map, npv2_map, deltaNPV_map — raster maps
#'   \item LULCT1, LULCT2, PU — original rasters
#'   \item total_area, period — metadata for analysis
#' }
#' @examples
#' preprocess_data("lulc_t1.tif", "lulc_t2.tif", "pu.tif",
#'                 "lookup_pu.csv", "lookup_npv.csv", "lookup_cstock.csv",
#'                 2000, 2020)
#' @export
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

generate_output_maps <- function(npv1_map, npv2_map, deltaNPV_map, npv_table, wd) {
  writeRaster(npv1_map, file.path(wd, "npv1_distribution_map.tif"), overwrite = TRUE)
  writeRaster(npv2_map, file.path(wd, "npv2_distribution_map.tif"), overwrite = TRUE)
  writeRaster(deltaNPV_map, file.path(wd, "deltaNPV_distribution_map.tif"), overwrite = TRUE)
  write.xlsx(npv_table, file.path(wd, "tabel_npv.xlsx"), overwrite = TRUE)
}

#' Build Opportunity Cost Table
#'
#' Calculates opportunity costs and emission rates based on NPV and carbon data.
#'
#' @param dt_quesc_npv Data frame from `preprocess_data()` output.
#' @param period Time period between T1 and T2 (in years).
#' @param total_area Total analysis area in hectares.
#'
#' @return A list with one element:
#' \itemize{
#'   \item `opcost_all` — combined positive and negative opportunity cost table.
#' }
#' @examples
#' build_opcost_table(data$combinedRasterTable, data$period, data$total_area)
#' @export
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
  list(opcost_all = opcost_all, data_em_sel = data_em_sel)
}

#' Prepare Curve Data for Abatement Analysis
#'
#' Formats opportunity cost data for abatement curve visualization.
#'
#' @param opcost_table Output from `build_opcost_table()` function.
#'
#' @return A data frame with formatted columns for emission rates, opportunity costs,
#' land use changes, planning units, and areas.
#' @examples
#' prepare_curve_data(opcost_table)
#' @export
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

#' Build Grouped Data for Abatement Curve
#'
#' Aggregates curve data by land use change and calculates logarithmic opportunity costs.
#'
#' @param df_curve Data frame from `prepare_curve_data()` function.
#'
#' @return A grouped and summarized data frame ready for abatement curve plotting.
#' @examples
#' build_grouped_data(curve_data)
#' @export
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

#' Split Emission Data by Direction
#'
#' Separates positive and negative emissions and calculates cumulative ranges.
#'
#' @param df_s Data frame from `build_grouped_data()` function.
#'
#' @return A data frame with emission data split into positive and negative components
#' with calculated xmin and xmax values for plotting.
#' @examples
#' split_emission_direction(grouped_data)
#' @export
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

#' Calculate Planning Unit Dominance
#'
#' Determines the dominant planning unit for each land use change type.
#'
#' @param df_curve Data frame from `prepare_curve_data()` function.
#'
#' @return A data frame showing the percentage dominance of planning units
#' for each land use change category.
#' @examples
#' calculate_pu_dominance(curve_data)
#' @export
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

#' Prepare Final Dataset for Abatement Curve
#'
#' Combines split emission data with planning unit dominance information.
#'
#' @param df_split Data frame from `split_emission_direction()` function.
#' @param df_pu_dominance Data frame from `calculate_pu_dominance()` function.
#'
#' @return A complete dataset with hover text and all necessary columns
#' for interactive abatement curve visualization.
#' @examples
#' prepare_final_dataset(split_data, dominance_data)
#' @export
prepare_final_dataset <- function(df_split, df_pu_dominance) {
  df_s_final <- df_split %>%
    left_join(df_pu_dominance, by = "land_use_change") %>%
    mutate(
      hover_text = paste0(
        "Perubahan Lahan: ", land_use_change, "<br>",
        "Opportunity Cost: ", scales::comma(opportunity_cost), "<br>",
        "Laju Emisi: ", scales::comma(emission_rate), "<br>",
        "Unit Perencanaan Dominan: ", planning_unit, " (", scales::percent(pct_of_largest_pu, accuracy = 0.1), ")"
      )
    )
  return(df_s_final)
}

#' Plot Abatement Cost Curve
#'
#' Creates an interactive abatement cost curve plot using Plotly.
#'
#' @param df_s_final Complete dataset from `prepare_final_dataset()` function.
#' @param currency Character string specifying the currency symbol (e.g., "IDR", "USD").
#'
#' @return An interactive Plotly graph showing the abatement cost curve.
#' @examples
#' plot_abatement_curve(final_data, "IDR")
#' @export
plot_abatement_curve <- function(df_s_final, currency) {
  # Calculate the x-axis limits from your data
  x_limits <- range(c(df_s_final$xmin, df_s_final$xmax), na.rm = TRUE)
  max_abs <- max(abs(x_limits))
  x_range <- c(x_limits[1], max_abs)
  
  # Calculate y-axis limits
  y_limits <- range(c(0, df_s_final$opportunity_cost_log), na.rm = TRUE)
  
  p <- ggplot(df_s_final) +
    # Add background for x < 0 (emissions/negative side)
    geom_rect(
      data = data.frame(xmin = x_range[1], xmax = 0, ymin = y_limits[1], ymax = y_limits[2]),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#ffe6e6", alpha = 0.8, inherit.aes = FALSE
    ) +
    # Add background for x >= 0 (sequestration/positive side)
    geom_rect(
      data = data.frame(xmin = 0, xmax = x_range[2], ymin = y_limits[1], ymax = y_limits[2]),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#e6f7e6", alpha = 0.8, inherit.aes = FALSE
    ) +
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
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      hovermode = "x+y",
      xaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey'),
      yaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey')
      # hoverlabel = list(bgcolor = "white")
    ) %>%
    config(
      displaylogo = FALSE,
      displayModeBar = TRUE,
      modeBarButtonsToAdd = list(
        list(
          name = "Fullscreen",
          icon = list(
            width = 20,
            height = 20,
            path = "M7 14H5v5h5v-2H7v-3zm-2-4h2V7h3V5H5v5zm12 7h-3v2h5v-5h-2v3zM14 5v2h3v3h2V5h-5z",
            transform = "scale(1)"
          ),
          click = htmlwidgets::JS("
          function(gd) {
            var fullscreenElement = document.fullscreenElement || document.webkitFullscreenElement || document.mozFullScreenElement || document.msFullscreenElement;
            if (!fullscreenElement) {
              var el = gd;
              if (el.requestFullscreen) {
                el.requestFullscreen();
              } else if (el.webkitRequestFullscreen) {
                el.webkitRequestFullscreen();
              } else if (el.mozRequestFullScreen) {
                el.mozRequestFullScreen();
              } else if (el.msRequestFullscreen) {
                el.msRequestFullscreen();
              }
            } else {
              if (document.exitFullscreen) {
                document.exitFullscreen();
              } else if (document.webkitExitFullscreen) {
                document.webkitExitFullscreen();
              } else if (document.mozCancelFullScreen) {
                document.mozCancelFullScreen();
              } else if (document.msExitFullscreen) {
                document.msExitFullscreen();
              }
            }
          }
        ")
        )
      ),
      scrollZoom = TRUE
    )
}

' Calculate Total NPV Values
#'
#' Computes summary statistics for NPV across the entire dataset.
#'
#' @param data Data frame from `preprocess_data()` output.
#'
#' @return A data frame with total NPV values for time 1, time 2, and their difference.
#' @examples
#' calculate_total_values(processed_data)
#' @export
calculate_total_values <- function(data) {
  data %>%
    summarise(
      Total_NPV1 = sum(NPV1, na.rm = TRUE),
      Total_NPV2 = sum(NPV2, na.rm = TRUE),
      Total_Delta_NPV = sum(deltaNPV, na.rm = TRUE) 
    )
}

#' Dissolve Land Cover Data for Time 1
#'
#' Aggregates NPV and area data by land cover class for the first time period.
#'
#' @param data Data frame from `preprocess_data()` output.
#' @param top_n Number of top land cover classes to return (default: 10).
#'
#' @return A data frame with summarized NPV and area data for top land cover classes at T1.
#' @examples
#' dissolve_lc1(processed_data, 10)
#' @export
dissolve_lc1 <- function(data, top_n = 10) {
  data %>%
    group_by(LC1) %>%
    summarise(
      Total_NPV1 = sum(NPV1, na.rm = TRUE),
      Total_Ha1 = sum(Ha, na.rm = TRUE)) %>%
    arrange(desc(Total_NPV1)) %>%
    slice_head(n = top_n)
}

#' Dissolve Land Cover Data for Time 2
#'
#' Aggregates NPV and area data by land cover class for the second time period.
#'
#' @param data Data frame from `preprocess_data()` output.
#' @param top_n Number of top land cover classes to return (default: 10).
#'
#' @return A data frame with summarized NPV and area data for top land cover classes at T2.
#' @examples
#' dissolve_lc2(processed_data, 10)
#' @export
dissolve_lc2 <- function(data, top_n = 10) {
  data %>%
    group_by(LC2) %>%
    summarise(
      Total_NPV2 = sum(NPV2, na.rm = TRUE),
      Total_Ha2 = sum(Ha, na.rm = TRUE)) %>%
    arrange(desc(Total_NPV2)) %>%
    slice_head(n = top_n)
}

#' Dissolve Land Use Land Cover Change Data
#'
#' Aggregates NPV change data by land use change transitions.
#'
#' @param data Data frame from `preprocess_data()` output.
#' @param top_n Number of top land use changes to return (default: 10).
#'
#' @return A data frame with summarized NPV change data for top land use transitions.
#' @examples
#' dissolve_lulcc(processed_data, 10)
#' @export
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

#' Dissolve All Land Use Land Cover Change Data
#'
#' Aggregates all NPV change data by land use change transitions without filtering.
#'
#' @param data Data frame from `preprocess_data()` output.
#'
#' @return A complete data frame with all land use transitions and their NPV changes.
#' @examples
#' all_dissolve_lulcc(processed_data)
#' @export
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

#' Create Land Cover 1 Bar Chart
#'
#' Generates an interactive bar chart for NPV by land cover class at time 1.
#'
#' @param data Data frame from `dissolve_lc1()` function.
#' @param title Chart title (default: "Top 10 Total NPV by LC1").
#' @param currency Currency symbol for display (default: "IDR").
#'
#' @return An interactive Plotly bar chart.
#' @examples
#' create_lc1_bar(lc1_data, "NPV by Land Cover T1", "USD")
#' @export
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
      color = "lightblue"
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

#' Create Land Cover 2 Bar Chart
#'
#' Generates an interactive bar chart for NPV by land cover class at time 2.
#'
#' @param data Data frame from `dissolve_lc2()` function.
#' @param title Chart title (default: "Top 10 Total NPV by LC2").
#' @param currency Currency symbol for display (default: "IDR").
#'
#' @return An interactive Plotly bar chart.
#' @examples
#' create_lc2_bar(lc2_data, "NPV by Land Cover T2", "USD")
#' @export
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
      color = "steelblue"
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

#' Create Land Use Change Bar Chart
#'
#' Generates an interactive diverging bar chart for NPV changes by land use transition.
#'
#' @param data Data frame from `dissolve_lulcc()` function.
#' @param title Chart title (default: "Top 10 LULCC by ΔNPV").
#' @param currency Currency symbol for display (default: "IDR").
#'
#' @return An interactive Plotly diverging bar chart.
#' @examples
#' create_lulcc_bar(lulcc_data, "Land Use Change NPV Differences", "USD")
#' @export
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

#' Process Planning Unit Data
#'
#' Performs comprehensive analysis for a specific planning unit including
#' NPV calculations and visualization generation.
#'
#' @param pu_data Subset of data for a specific planning unit.
#' @param pu_name Name of the planning unit for labeling.
#' @param currency Currency symbol for display (default: "IDR").
#'
#' @return A list containing:
#' \itemize{
#'   \item total_values - Summary NPV statistics
#'   \item lc1_bar, lc2_bar, lulcc_bar - Interactive charts
#'   \item all_dissolved_lulcc_pu - Complete land use change data
#'   \item currency - Currency used for formatting
#' }
#' @examples
#' process_pu_data(pu_data, "Forest Zone", "USD")
#' @export
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

#' Process Unit Data for Abatement Curves
#'
#' Prepares emission and opportunity cost data for individual planning units.
#'
#' @param df Data frame containing emission rate and opportunity cost data.
#'
#' @return A processed data frame with cumulative emission ranges and hover text.
#' @examples
#' process_unit(unit_data)
#' @export
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

#' Generate Abatement Plots by Planning Unit
#'
#' Creates individual abatement cost curves for each planning unit.
#'
#' @param df_curve Data frame from `prepare_curve_data()` function.
#' @param currency Currency symbol for display.
#'
#' @return A list of interactive Plotly abatement curves, one for each planning unit.
#' @examples
#' generate_plots_by_pu(curve_data, "IDR")
#' @export
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
      # Calculate the x-axis limits from the data for this planning unit
      x_limits <- range(c(.x$xmin, .x$xmax), na.rm = TRUE)
      max_abs <- max(abs(x_limits))
      x_range <- c(x_limits[1], max_abs)
      
      # Calculate y-axis limits
      y_limits <- range(c(0, .x$opportunity_cost_log), na.rm = TRUE)
      
      p <- ggplot(.x) +
        # Add background for x < 0 (emissions/negative side)
        geom_rect(
          data = data.frame(xmin = x_range[1], xmax = 0, ymin = y_limits[1], ymax = y_limits[2]),
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "#ffe6e6", alpha = 0.8, inherit.aes = FALSE
        ) +
        # Add background for x >= 0 (sequestration/positive side)
        geom_rect(
          data = data.frame(xmin = 0, xmax = x_range[2], ymin = y_limits[1], ymax = y_limits[2]),
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "#e6f7e6", alpha = 0.8, inherit.aes = FALSE
        ) +
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
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          hovermode = "x+y",
          xaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey'),
          yaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey')
          # hoverlabel = list(bgcolor = "white")
        ) %>%
        config(
          displaylogo = FALSE,
          displayModeBar = TRUE,
          modeBarButtonsToAdd = list(
            list(
              name = "Fullscreen",
              icon = list(
                width = 20,
                height = 20,
                path = "M7 14H5v5h5v-2H7v-3zm-2-4h2V7h3V5H5v5zm12 7h-3v2h5v-5h-2v3zM14 5v2h3v3h2V5h-5z",
                transform = "scale(1)"
              ),
              click = htmlwidgets::JS("
          function(gd) {
            var fullscreenElement = document.fullscreenElement || document.webkitFullscreenElement || document.mozFullScreenElement || document.msFullscreenElement;
            if (!fullscreenElement) {
              var el = gd;
              if (el.requestFullscreen) {
                el.requestFullscreen();
              } else if (el.webkitRequestFullscreen) {
                el.webkitRequestFullscreen();
              } else if (el.mozRequestFullScreen) {
                el.mozRequestFullScreen();
              } else if (el.msRequestFullscreen) {
                el.msRequestFullscreen();
              }
            } else {
              if (document.exitFullscreen) {
                document.exitFullscreen();
              } else if (document.webkitExitFullscreen) {
                document.webkitExitFullscreen();
              } else if (document.mozCancelFullScreen) {
                document.mozCancelFullScreen();
              } else if (document.msExitFullscreen) {
                document.msExitFullscreen();
              }
            }
          }
        ")
            )
          ),
          scrollZoom = TRUE
        )
    })
  
  return(plots_list)
}

#' Generate Complete Abatement Outputs
#'
#' Orchestrates the generation of both main and planning unit-specific abatement curves.
#'
#' @param opcost_table Output from `build_opcost_table()` function.
#' @param currency Currency symbol for display.
#'
#' @return A list containing:
#' \itemize{
#'   \item main_plot - Overall abatement cost curve
#'   \item pu_plots - Individual abatement curves per planning unit
#' }
#' @examples
#' generate_abatement_outputs(opcost_data, "USD")
#' @export
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

#' Generate Report Parameters
#'
#' Prepares all necessary parameters for RMarkdown report generation.
#'
#' @param data Main processed data from `preprocess_data()` function.
#' @param maps Spatial data objects from `preprocess_data()` function.
#' @param paths File paths used in the analysis.
#' @param times Timing information for the analysis.
#' @param output_dir Directory where outputs will be saved.
#' @param pu_outputs Processed planning unit data from `process_pu_data()` function.
#' @param currency Currency symbol for display.
#'
#' @return A comprehensive list of parameters ready for RMarkdown report rendering.
#' @examples
#' generate_report_params(data, maps, paths, times, output_dir, pu_outputs, "IDR")
#' @export
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
  npv_output_table <- opcost_results$data_em_sel %>% 
    select(-Freq, -ID_LC1, -ID_LC2, -C_T1, -C_T2, -NPV_LC1, -NPV_LC2) %>%
    filter(!is.nan(opcost), !is.na(opcost))
  abatement_outputs <- generate_abatement_outputs(opcost_table, currency)
  
  generate_output_maps(maps$npv1_map, maps$npv2_map, maps$deltaNPV_map, npv_output_table, output_dir)
  
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
    cstock_table_path = paths$pathLookupCstock,
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