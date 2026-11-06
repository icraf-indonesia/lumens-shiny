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

#' Read Table (CSV or Excel)
#'
#' Reads a lookup table from .csv, .xlsx, or .xls and returns a clean data.frame.
#'
#' @param filepath Path to the file.
#' @param sheet Sheet name or index for Excel (default: 1).
#'
#' @return A cleaned data.frame.
#' @export
read_lookup_file <- function(filepath, sheet = 1) {
  
  ext <- tolower(tools::file_ext(filepath))
  
  if (ext == "csv") {
    
    df <- readr::read_csv(
      filepath,
      show_col_types = FALSE
    )
    
  } else if (ext %in% c("xlsx", "xls")) {
    
    df <- readxl::read_excel(
      filepath,
      sheet = sheet
    )
    
  } else {
    
    stop(
      paste(
        "Unsupported file format:", ext,
        "\nSupported formats: .csv, .xlsx, .xls"
      )
    )
    
  }
  
  df <- as.data.frame(df)
  
  # --- Pembersihan ---
  # 1. Buang kolom yang seluruhnya NA
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
  
  # 2. Buang kolom bernama ...N (artefak CSV dengan kolom kosong)
  df <- df[, !grepl("^\\.\\.\\.\\d+$", names(df)), drop = FALSE]
  
  # 3. Trim whitespace pada nama kolom
  names(df) <- trimws(names(df))
  
  # 4. Pastikan kolom pertama numerik (ID untuk terra::levels)
  df[[1]] <- suppressWarnings(as.numeric(df[[1]]))
  
  # 5. Peringatkan kalau ada NA di kolom pertama
  if (any(is.na(df[[1]]))) {
    warning("Kolom pertama lookup table berisi nilai non-numerik atau NA. ",
            "Periksa kembali file: ", basename(filepath))
  }
  
  df
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
                            valueT1, valueT2,
                            puIsVector = FALSE) {
  
  years <- suppressWarnings(as.numeric(c(valueT1, valueT2)))
  if (length(years) != 2 || any(!is.finite(years)) || years[2] <= years[1]) {
    stop("T1 and T2 must be valid years, and T2 must be later than T1.", call. = FALSE)
  }

  # ---------- LULC T1 ----------
  LULCT1 <- terra::rast(pathLULCT1)
  LookupNPV <- read_lookup_file(pathLookupNPV)
  levels(LULCT1) <- LookupNPV
  LULCT1 <- setNames(LULCT1, "LC1")
  if (!is.null(valueT1)) terra::time(LULCT1, tstep = "years") <- as.numeric(valueT1)
  
  # ---------- LULC T2 ----------
  LULCT2 <- terra::rast(pathLULCT2)
  levels(LULCT2) <- LookupNPV
  LULCT2 <- setNames(LULCT2, "LC2")
  if (!is.null(valueT2)) terra::time(LULCT2, tstep = "years") <- as.numeric(valueT2)
  
  # ---------- PU ----------
  if (puIsVector) {
    PU_vect <- terra::vect(pathPU)
    
    # 1. SAMAKAN CRS (Proyeksi) dengan LULCT1
    if (!terra::same.crs(PU_vect, LULCT1)) {
      message("CRS berbeda. Memproyeksikan shapefile PU ke CRS Raster...")
      PU_vect <- terra::project(PU_vect, terra::crs(LULCT1))
    }
    
    # 2. Ekstrak atribut ke data.frame
    attr_df <- as.data.frame(PU_vect)
    
    # 3. Cari kolom ID (numeric)
    id_col_name <- NULL
    for (nm in names(attr_df)) {
      vals <- attr_df[[nm]]
      if (is.numeric(vals) && length(unique(vals[!is.na(vals)])) > 1) {
        id_col_name <- nm
        break
      }
    }
    if (is.null(id_col_name)) stop("Tidak ada kolom ID numerik di shapefile.")
    
    # 4. Cari kolom Nama PU (character)
    name_col_name <- NULL
    for (nm in names(attr_df)) {
      vals <- attr_df[[nm]]
      if (is.character(vals) || is.factor(vals)) {
        name_col_name <- nm
        break
      }
    }
    if (is.null(name_col_name)) name_col_name <- id_col_name
    
    # 5. Buat LookupPU
    LookupPU <- data.frame(
      ID = as.numeric(attr_df[[id_col_name]]),
      PU = as.character(attr_df[[name_col_name]]),
      stringsAsFactors = FALSE
    )
    LookupPU <- LookupPU[!is.na(LookupPU$ID), ]
    LookupPU <- LookupPU[!duplicated(LookupPU$ID), ]
    
    # 6. Rasterize (Sekarang CRS sudah sama)
    PU <- terra::rasterize(
      PU_vect,
      LULCT1,
      field = id_col_name,
      background = NA
    )
    
    # 7. Pastikan hasilnya SpatRaster
    if (!inherits(PU, "SpatRaster")) {
      stop("Gagal melakukan rasterize. Pastikan shapefile berisi poligon yang valid.")
    }
    
  } else {
    # Bagian Raster PU (yang Anda gunakan saat ini)
    PU <- terra::rast(pathPU)
    
    # Cek dan samakan CRS untuk Raster PU
    if (!terra::same.crs(PU, LULCT1)) {
      message("CRS berbeda. Memproyeksikan Raster PU ke CRS Raster LULC...")
      PU <- terra::project(PU, terra::crs(LULCT1), method = "near")
    }
    
    LookupPU <- read_lookup_file(pathLookupPU)
    LookupPU[[1]] <- as.numeric(LookupPU[[1]])
  }
  
  # Set levels + resample
  levels(PU) <- LookupPU
  PU <- terra::resample(PU, LULCT1, method = "near")
  
  # ---------- Combine rasters ----------
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
  
  # ---------- Lookup Carbon ----------
  if (!is.null(pathLookupCstock)) {
    LookupCstock <- read_lookup_file(pathLookupCstock)
    LookupCstock <- LookupCstock %>% dplyr::select(ID = 1, LC = 2, Carbon = 3)
    LookupCstock[[1]] <- as.numeric(LookupCstock[[1]])
    LookupCstock$Carbon <- suppressWarnings(as.numeric(LookupCstock$Carbon))
  } else {
    # Jika tidak ada input karbon, buat tabel dummy agar join tidak error
    LookupCstock <- data.frame(ID = 0, LC = "NoData", Carbon = 0)
  }
  
  # ---------- Join ----------
  combinedRasterTable <- combinedRasterTable %>%
    left_join(LookupNPV %>% rename_all(~paste0(., "_LC1")), by = c("LC1" = "LC_LC1")) %>%
    left_join(LookupNPV %>% rename_all(~paste0(., "_LC2")), by = c("LC2" = "LC_LC2")) %>% 
    left_join(LookupCstock %>% rename(C_T1 = Carbon), by = c("LC1" = "LC")) %>%
    left_join(LookupCstock %>% rename(C_T2 = Carbon), by = c("LC2" = "LC")) %>%
    select(-ID.x, -ID.y) %>%
    mutate(
      NPV1 = EAE_LC1 * Ha,
      NPV2 = EAE_LC2 * Ha,
      deltaNPV = NPV2 - NPV1,
      LULCC = paste(LC1, "to", LC2)
    )

  if (!is.null(pathLookupCstock)) {
    missing_carbon <- unique(c(
      as.character(combinedRasterTable$LC1[!is.finite(combinedRasterTable$C_T1)]),
      as.character(combinedRasterTable$LC2[!is.finite(combinedRasterTable$C_T2)])
    ))
    if (length(missing_carbon) > 0) {
      stop("Nilai karbon tidak tersedia atau tidak valid untuk kelas: ",
           paste(sort(missing_carbon), collapse = ", "), call. = FALSE)
    }
  } else {
    combinedRasterTable$C_T1 <- 0
    combinedRasterTable$C_T2 <- 0
  }
  
  total_area <- sum(combinedRasterTable$Ha, na.rm = TRUE)
  period <- as.numeric(valueT2) - as.numeric(valueT1)
  
  # ---------- NPV maps ----------
  npv_matrix <- as.matrix(LookupNPV[, c("ID", "EAE")])
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
  writeRaster(npv1_map, file.path(wd, "EAE_T1_distribution_map.tif"), overwrite = TRUE)
  writeRaster(npv2_map, file.path(wd, "EAE_T2_distribution_map.tif"), overwrite = TRUE)
  writeRaster(deltaNPV_map, file.path(wd, "deltaEAE_distribution_map.tif"), overwrite = TRUE)
  write.xlsx(npv_table, file.path(wd, "tabel_perhitungan_keuntungan_usaha_tani.xlsx"), overwrite = TRUE)
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
  if (length(period) != 1 || !is.finite(period) || period <= 0) {
    stop("The analysis period must be a positive number of years.", call. = FALSE)
  }
  required_values <- dt_quesc_npv[c("C_T1", "C_T2", "Ha", "NPV1", "NPV2")]
  if (any(!vapply(required_values, function(x) all(is.finite(x)), logical(1)))) {
    stop("Opportunity cost inputs contain missing or non-finite values.")
  }
  
  data_em_sel <- dt_quesc_npv
  
  data_em_sel <- data_em_sel %>%
    mutate(
      em_rate = ((C_T1 - C_T2) * (Ha * 3.67)) / (total_area * period),
      economic_change = NPV2 - NPV1,
      em_tot = (C_T1 - C_T2) * Ha * 3.67,
      opcost = ifelse(em_tot != 0, economic_change / em_tot, NA_real_)
    )
  
  opcost_tab <- data.frame(
    luchg = data_em_sel$LULCC,
    zone = data_em_sel$PU,
    opcost = data_em_sel$opcost,
    emrate = data_em_sel$em_rate,
    area = data_em_sel$Ha,
    economic_change = data_em_sel$economic_change,
    emissions_tco2 = data_em_sel$em_tot
  ) %>%
    filter(!is.nan(opcost), !is.na(opcost))
  
  opcost_tab_p <- opcost_tab[opcost_tab$opcost >= 0, ]
  opcost_tab_p <- opcost_tab_p[order(opcost_tab_p$opcost), ]
  opcost_tab_p$cum_emrate <- cumsum(opcost_tab_p$emrate)
  
  opcost_tab_n <- opcost_tab[opcost_tab$opcost < 0, ]
  opcost_tab_n <- opcost_tab_n[order(opcost_tab_n$opcost), ]
  opcost_tab_n$cum_emrate <- cumsum(opcost_tab_n$emrate)
  
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
    land_use_change = opcost_table$luchg,
    planning_unit = opcost_table$zone,
    area = opcost_table$area,
    economic_change = opcost_table$economic_change,
    emissions_tco2 = opcost_table$emissions_tco2
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
  df_grouped <- df_curve %>%
    mutate(carbon_direction = sign(emissions_tco2)) %>%
    group_by(land_use_change, carbon_direction) %>%
    summarise(
      emission_rate = sum(emission_rate),
      economic_change = sum(economic_change),
      emissions_tco2 = sum(emissions_tco2),
      .groups = "drop"
    ) %>%
    mutate(opportunity_cost = ifelse(emissions_tco2 != 0, economic_change / emissions_tco2, NA_real_)) %>%
    arrange(opportunity_cost)
  
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
    mutate(carbon_direction = sign(emissions_tco2)) %>%
    group_by(land_use_change, carbon_direction, planning_unit) %>%
    summarise(total_area = sum(area), .groups = "drop") %>%
    group_by(land_use_change, carbon_direction) %>%
    mutate(
      land_use_total_area = sum(total_area),
      pct_of_largest_pu = total_area / land_use_total_area
    ) %>%
    slice_max(total_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(land_use_change, carbon_direction, planning_unit, pct_of_largest_pu)
  
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
    left_join(df_pu_dominance, by = c("land_use_change", "carbon_direction")) %>%
    mutate(
      hover_text = paste0(
        "Perubahan Lahan: ", land_use_change, "<br>",
        "Abatement cost (berdasarkan EAE tahunan): ", scales::comma(opportunity_cost), "<br>",
        "Laju perubahan stok karbon ternormalisasi: ", scales::comma(emission_rate), "<br>",
        "Unit Perencanaan Dominan: ", planning_unit, " (", scales::percent(pct_of_largest_pu, accuracy = 0.1), ")"
      )
    )
  return(df_s_final)
}

responsive_plot <- function(plot) {
  plot$width <- "100%"
  plot
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
  y_limits <- range(c(0, df_s_final$opportunity_cost), na.rm = TRUE)
  
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
      xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost,
      fill = land_use_change,
      text = hover_text
    ), color = "black") +
    labs(
      x = "Laju Emisi (ton CO<sub>2</sub>-eq/ha.tahun)",
      y = paste0("Abatement cost (", currency, "/year per t CO<sub>2</sub>e)"),
      title = "Kurva Abatement Cost"
    ) +
    scale_x_continuous(
      breaks = function(x) pretty(x, n = 6),
      labels = function(x) scales::comma(x, accuracy = 0.01)
    ) +
    scale_y_continuous(
      trans = scales::pseudo_log_trans(sigma = 1, base = 10),
      labels = scales::label_comma()
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplotly(p, tooltip = "text") %>%
    responsive_plot() %>%
    layout(
      hovermode = "x+y",
      margin = list(l = 90),
      xaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey'),
      yaxis = list(automargin = TRUE, showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey')
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
create_lc1_bar <- function(data, title = "Top 10 Total Keuntungan Usaha Tani pada LC1", currency = "IDR") {
  plotly::plot_ly(
    data = data,
    x = ~stringr::str_wrap(LC1, width = 25),
    y = ~Total_NPV1,
    type = "bar",
    hoverinfo = "text",
    hovertext = ~paste0(
      "Land Cover Class (LC 1): ", LC1, "<br>", 
      "Total Keuntungan Usaha Tani: ", currency, " ", format(Total_NPV1, big.mark = ",", scientific = FALSE)
    ),
    marker = list(
      color = "lightblue"
    )
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = "", categoryorder = "total descending", tickangle = -270),
      yaxis = list(title = paste("Total Keuntungan Usaha Tani (", currency, ")"), type="log"),
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
create_lc2_bar <- function(data, title = "Top 10 Total Keuntungan Usaha Tani pada LC2", currency = "IDR") {
  plotly::plot_ly(
    data = data,
    x = ~stringr::str_wrap(LC2, width = 25),
    y = ~Total_NPV2,
    type = "bar",
    hoverinfo = "text",
    hovertext = ~paste0(
      "Land Cover Class (LC 2): ", LC2, "<br>", 
      "Total Keuntungan Usaha Tani: ", currency, " ", format(Total_NPV2, big.mark = ",", scientific = FALSE)
    ),
    marker = list(
      color = "steelblue"
    )
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = "", categoryorder = "total descending", tickangle = -270),
      yaxis = list(title = paste("Total Keuntungan Usaha Tani (", currency, ")"), type="log"),
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
create_lulcc_bar <- function(data, title = "Top 10 LULCC by ΔKeuntungan Usaha Tani", currency = "IDR") {
  data <- data %>%
    arrange(desc(Total_abs_deltaNPV)) %>%
    mutate(LULCC = factor(LULCC, levels = unique(LULCC)),
           positive = ifelse(Total_deltaNPV > 0, Total_deltaNPV, 0),
           negative = ifelse(Total_deltaNPV < 0, Total_deltaNPV, 0))
  
  plotly::plot_ly(data = data) %>%
    responsive_plot() %>%
    add_bars(x = ~positive, y = ~LULCC, name = "Positive ΔKeuntungan Usaha Tani", 
             marker = list(color = "lightgreen"), orientation = "h") %>%
    add_bars(x = ~negative, y = ~LULCC, name = "Negative ΔKeuntungan Usaha Tani", 
             marker = list(color = "red"), orientation = "h") %>%
    layout(
      title = title,
      xaxis = list(title = paste0("ΔEAE total (", currency, "/tahun)")),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(levels(data$LULCC))),
      barmode = "relative",
      legend = list(orientation = "h", x = 0, y = -0.25, xanchor = "left", yanchor = "top"),
      margin = list(l = 150, b = 120),
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
    paste0(col_name, " (", currency, "/tahun)")
  }
  
  total_values <- calculate_total_values(pu_data) %>% 
    as.data.frame() %>% 
    rename(
      !!format_currency_col("Total Keuntungan Usaha Tani T1", currency) := Total_NPV1,
      !!format_currency_col("Total Keuntungan Usaha Tani T2", currency) := Total_NPV2,
      !!format_currency_col("Total ΔKeuntungan Usaha Tani", currency) := Total_Delta_NPV
    ) %>% 
    t() %>% 
    `colnames<-`("Nilai")
  
  dissolved_lc1 <- dissolve_lc1(pu_data, 10)
  dissolved_lc2 <- dissolve_lc2(pu_data, 10)
  dissolved_lulcc <- dissolve_lulcc(pu_data, 10)
  all_dissolved_lulcc_pu <- all_dissolve_lulcc(pu_data)
  
  lc1_bar <- create_lc1_bar(dissolved_lc1, paste("Top 10 Keuntungan Usaha Tani pada LC 1 di PU:", pu_name), currency)
  lc2_bar <- create_lc2_bar(dissolved_lc2, paste("Top 10 Keuntungan Usaha Tani pada LC 2 di PU:", pu_name), currency)
  lulcc_bar <- create_lulcc_bar(dissolved_lulcc, paste("Top 10 ΔKeuntungan Usaha Tani di PU:", pu_name), currency)
  
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
    arrange(opportunity_cost) %>%
    mutate(
      xmin = lag(cumsum(emission_rate), default = 0),
      xmax = cumsum(emission_rate)
    )
  
  df_neg <- df %>%
    filter(emission_rate < 0) %>%
    arrange(opportunity_cost) %>%
    mutate(
      xmax = lag(cumsum(emission_rate), default = 0),
      xmin = cumsum(emission_rate)
    )
  
  bind_rows(df_pos, df_neg) %>%
    mutate(
      hover_text = paste0(
        "Perubahan Lahan: ", land_use_change, "<br>",
        "Abatement cost (berdasarkan EAE tahunan): ", scales::comma(opportunity_cost), "<br>",
        "Laju perubahan stok karbon ternormalisasi: ", scales::comma(emission_rate)
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
  df_pu <- df_curve
  
  df_pu_processed <- df_pu %>%
    group_split(planning_unit) %>%
    purrr::map_df(process_unit)
  
  plots_list <- df_pu_processed %>%
    split(.$planning_unit) %>%
    purrr::map(~ {
      # --- VALIDASI: Jika data kosong atau semua NA, lewati ---
      if (nrow(.x) == 0 || all(is.na(.x$xmin)) || all(is.na(.x$xmax))) {
        return(NULL)
      }
      
      # Calculate the x-axis limits from the data for this planning unit
      x_limits <- range(c(.x$xmin, .x$xmax), na.rm = TRUE)
      max_abs <- max(abs(x_limits))
      x_range <- c(x_limits[1], max_abs)
      
      # Calculate y-axis limits
      y_limits <- range(c(0, .x$opportunity_cost), na.rm = TRUE)
      
      # Pastikan y_limits valid (tidak Inf)
      if (any(is.infinite(y_limits))) {
        y_limits <- c(-1, 1)
      }
      
      pu_title <- unique(.x$planning_unit)
      if(length(pu_title) == 0) pu_title <- "Tidak Diketahui"
      
      p <- ggplot(.x) +
        geom_rect(
          data = data.frame(xmin = x_range[1], xmax = 0, ymin = y_limits[1], ymax = y_limits[2]),
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "#ffe6e6", alpha = 0.8, inherit.aes = FALSE
        ) +
        geom_rect(
          data = data.frame(xmin = 0, xmax = x_range[2], ymin = y_limits[1], ymax = y_limits[2]),
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "#e6f7e6", alpha = 0.8, inherit.aes = FALSE
        ) +
        geom_rect(aes(
          xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost,
          fill = land_use_change,
          text = hover_text
        ), color = "black") +
        labs(
          x = "Laju karbon (t CO<sub>2</sub>e/ha/tahun)",
          y = paste0("Abatement cost (", currency, "/year per t CO<sub>2</sub>e)"),
          title = paste("Kurva abatement cost -", pu_title)
        ) +
        scale_x_continuous(
          breaks = function(x) pretty(x, n = 6),
          labels = function(x) scales::comma(x, accuracy = 0.01)
        ) +
        scale_y_continuous(
          trans = scales::pseudo_log_trans(sigma = 1, base = 10),
          labels = scales::label_comma()
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p, tooltip = "text") %>%
        responsive_plot() %>%
        layout(
          hovermode = "x+y",
          margin = list(l = 90),
          xaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey'),
          yaxis = list(automargin = TRUE, showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey')
        ) %>%
        config(
          displaylogo = FALSE,
          displayModeBar = TRUE,
          modeBarButtonsToAdd = list(
            list(
              name = "Fullscreen",
              icon = list(
                width = 20, height = 20,
                path = "M7 14H5v5h5v-2H7v-3zm-2-4h2V7h3V5H5v5zm12 7h-3v2h5v-5h-2v3zM14 5v2h3v3h2V5h-5z",
                transform = "scale(1)"
              ),
              click = htmlwidgets::JS("
          function(gd) {
            var fullscreenElement = document.fullscreenElement || document.webkitFullscreenElement || document.mozFullScreenElement || document.msFullscreenElement;
            if (!fullscreenElement) {
              var el = gd;
              if (el.requestFullscreen) { el.requestFullscreen(); } 
              else if (el.webkitRequestFullscreen) { el.webkitRequestFullscreen(); } 
              else if (el.mozRequestFullScreen) { el.mozRequestFullScreen(); } 
              else if (el.msRequestFullscreen) { el.msRequestFullscreen(); }
            } else {
              if (document.exitFullscreen) { document.exitFullscreen(); } 
              else if (document.webkitExitFullscreen) { document.webkitExitFullscreen(); } 
              else if (document.mozCancelFullScreen) { document.mozCancelFullScreen(); } 
              else if (document.msExitFullscreen) { document.msExitFullscreen(); }
            }
          }
        ")
            )
          ),
          scrollZoom = TRUE
        )
    })
  
  # Hapus elemen NULL dari list agar tidak error saat dirender
  plots_list <- plots_list[!sapply(plots_list, is.null)]
  
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
# generate_abatement_outputs <- function(opcost_table, currency) {
#   df_curve <- prepare_curve_data(opcost_table)
#   df_grouped <- build_grouped_data(df_curve)
#   df_split <- split_emission_direction(df_grouped)
#   df_pu_dominance <- calculate_pu_dominance(df_curve)
#   df_final <- prepare_final_dataset(df_split, df_pu_dominance)
#   
#   main_plot <- plot_abatement_curve(df_final, currency)
#   pu_plots <- generate_plots_by_pu(df_curve, currency)
#   
#   return(list(main_plot = main_plot, pu_plots = pu_plots))
# }

prepare_curve_positions <- function(df) {
  
  df %>%
    arrange(opportunity_cost) %>%
    mutate(
      xmin = lag(
        cumsum(width),
        default = 0
      ),
      xmax = cumsum(width)
    )
  
}

plot_emission_curve <- function(df, currency){
  
  df_emission <- df %>%
    filter(emission_rate > 0) %>%
    mutate(
      width = emission_rate
    ) %>%
    prepare_curve_positions()
  
  p <- ggplot(df_emission) +
    
    geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = opportunity_cost,
        fill = land_use_change,
        text = hover_text
      ),
      color = "black"
    ) +
    
    labs(
      title = "Abatement cost curve - carbon-stock loss",
      x = "Carbon loss (t CO<sub>2</sub>e/ha/year)",
      y = paste0("Abatement cost (", currency, "/year per t CO<sub>2</sub>e)")
    ) +
    
    scale_y_continuous(
      trans = scales::pseudo_log_trans(sigma = 1, base = 10),
      labels = scales::label_comma()
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "none"
    )
  
  ggplotly(
    p,
    tooltip = "text"
  ) %>% responsive_plot() %>% layout(yaxis = list(automargin = TRUE), margin = list(l = 90))
}

plot_sequestration_curve <- function(df, currency){
  
  df_seq <- df %>%
    filter(emission_rate < 0) %>%
    mutate(
      width = abs(emission_rate)
    ) %>%
    prepare_curve_positions()
  
  p <- ggplot(df_seq) +
    
    geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = opportunity_cost,
        fill = land_use_change,
        text = hover_text
      ),
      color = "black"
    ) +
    
    labs(
      title = "Abatement cost curve - carbon-stock increase",
      x = "Carbon gain (t CO<sub>2</sub>e/ha/year)",
      y = paste0("Abatement cost (", currency, "/year per t CO<sub>2</sub>e)")
    ) +
    
    scale_y_continuous(
      trans = scales::pseudo_log_trans(sigma = 1, base = 10),
      labels = scales::label_comma()
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "none"
    )
  
  ggplotly(
    p,
    tooltip = "text"
  ) %>% responsive_plot() %>% layout(yaxis = list(automargin = TRUE), margin = list(l = 90))
}

generate_abatement_outputs <- function(opcost_table, currency) {
  
  df_curve <- prepare_curve_data(opcost_table)
  
  df_grouped <- build_grouped_data(df_curve)
  
  df_split <- split_emission_direction(df_grouped)
  
  df_pu_dominance <- calculate_pu_dominance(df_curve)
  
  df_final <- prepare_final_dataset(
    df_split,
    df_pu_dominance
  )
  
  emission_plot <- plot_emission_curve(
    df_final %>% filter(emission_rate > 0),
    currency
  )
  
  sequestration_plot <- plot_sequestration_curve(
    df_final %>% filter(emission_rate < 0),
    currency
  )
  
  pu_plots <- generate_plots_by_pu(
    df_curve,
    currency
  )
  
  list(
    emission_plot = emission_plot,
    sequestration_plot = sequestration_plot,
    pu_plots = pu_plots
  )
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
    paste0(col_name, " (", currency, "/tahun)")
  }
  
  # --- Main NPV Summary Tables ---
  main_total_values <- calculate_total_values(data$combinedRasterTable) %>% 
    as.data.frame() %>% 
    rename(
      !!format_currency_col("Total Keuntungan Usaha Tani T1", currency) := Total_NPV1,
      !!format_currency_col("Total Keuntungan Usaha Tani T2", currency) := Total_NPV2,
      !!format_currency_col("Total ΔKeuntungan Usaha Tani", currency) := Total_Delta_NPV
    ) %>% 
    t() %>% 
    `colnames<-`("Nilai")
  
  # --- Dissolved Layers for Visualization (TABLE: Tampilkan 50) ---
  main_dissolved_lc1 <- dissolve_lc1(data$combinedRasterTable, top_n = 50)
  main_dissolved_lc2 <- dissolve_lc2(data$combinedRasterTable, top_n = 50)
  main_dissolved_lulcc <- dissolve_lulcc(data$combinedRasterTable, top_n = 50)
  all_dissolved_lulcc <- all_dissolve_lulcc(data$combinedRasterTable)
  
  # --- Generate Data KHUSUS untuk Grafik (CHART: Hanya 10) ---
  chart_dissolved_lc1 <- dissolve_lc1(data$combinedRasterTable, top_n = 10)
  chart_dissolved_lc2 <- dissolve_lc2(data$combinedRasterTable, top_n = 10)
  chart_dissolved_lulcc <- dissolve_lulcc(data$combinedRasterTable, top_n = 10)
  
  # --- Inisialisasi Abatement Outputs ---
  abatement_outputs <- list(
    emission_plot = NULL, 
    sequestration_plot = NULL, 
    pu_plots = NULL
  )
  
  # --- Generate Abatement Curve Outputs (HANYA JIKA ADA FILE KARBON) ---
  if (!is.null(paths$pathLookupCstock)) {
    # 1. Hitung opportunity cost
    opcost_results <- build_opcost_table(data$combinedRasterTable, data$period, data$total_area)
    opcost_table <- opcost_results$opcost_all
    
    # 2. Generate grafik abatement (utama & per PU)
    abatement_outputs <- generate_abatement_outputs(opcost_table, currency)
    
    # 3. Ambil tabel untuk export Excel
    npv_output_table <- opcost_results$data_em_sel %>% 
      select(-Freq, -ID_LC1, -ID_LC2, -C_T1, -C_T2, -EAE_LC1, -EAE_LC2) %>%
      filter(!is.nan(opcost), !is.na(opcost))
    
  } else {
    # Jika tidak ada file karbon, gunakan data mentah (tanpa em_rate, em_tot, opcost)
    npv_output_table <- data$combinedRasterTable
  }
  
  # --- Siapkan Tabel Excel (Readable & Conditional) ---
  if (!is.null(paths$pathLookupCstock)) {
    # SKENARIO A: Ada file karbon (Ambil dari hasil opcost)
    npv_output_table <- opcost_results$data_em_sel %>% 
      select(
        PU, LC1, LC2, Ha, NPV1, NPV2, deltaNPV, LULCC, 
        em_rate, em_tot, opcost
      ) %>%
      filter(!is.nan(opcost), !is.na(opcost)) %>%
      # Rename seluruh kolom termasuk kolom abatement
      dplyr::rename(
        `Unit Perencanaan (PU)` = PU,
        `Tutupan Lahan T1` = LC1,
        `Tutupan Lahan T2` = LC2,
        `Luas (Ha)` = Ha,
        `Keuntungan Usaha Tani T1` = NPV1,
        `Keuntungan Usaha Tani T2` = NPV2,
        `Delta Keuntungan Usaha Tani` = deltaNPV,
        `Perubahan Penggunaan Lahan` = LULCC,
        `Laju Emisi (ton CO2-eq/ha/tahun)` = em_rate,
        `Total Emisi (ton CO2-eq)` = em_tot,
        !!paste0("Biaya Peluang (", currency, "/tahun per ton CO2-eq)") := opcost
      )
  } else {
    # SKENARIO B: Tidak ada file karbon (Pilih kolom esensial saja)
    npv_output_table <- data$combinedRasterTable %>%
      select(
        PU, LC1, LC2, Ha, NPV1, NPV2, deltaNPV, LULCC
      ) %>%
      # Rename kolom dasar
      dplyr::rename(
        `Unit Perencanaan (PU)` = PU,
        `Tutupan Lahan T1` = LC1,
        `Tutupan Lahan T2` = LC2,
        `Luas (Ha)` = Ha,
        `Keuntungan Usaha Tani T1` = NPV1,
        `Keuntungan Usaha Tani T2` = NPV2,
        `Delta Keuntungan Usaha Tani` = deltaNPV,
        `Perubahan Penggunaan Lahan` = LULCC
      )
  }
  
  # Generate output maps dan Excel
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
    npv1_chart = create_lc1_bar(chart_dissolved_lc1, currency = currency),
    npv2_chart = create_lc2_bar(chart_dissolved_lc2, currency = currency),
    deltaNPV_chart = create_lulcc_bar(chart_dissolved_lulcc, currency = currency),
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
    eae_year = times$eae_year,
    pu_outputs = pu_outputs,
    output_dir = output_dir,
    currency = currency,
    emission_plot = abatement_outputs$emission_plot,
    sequestration_plot = abatement_outputs$sequestration_plot,
    abatement_pu_plots = abatement_outputs$pu_plots,
    has_cstock = !is.null(paths$pathLookupCstock)
  )
}

shrink_raster <- function(r, target = 3e5, fun = "modal") {
  n <- terra::ncell(r)
  if (n <= target) return(r)
  terra::aggregate(r, fact = ceiling(sqrt(n / target)), fun = fun, na.rm = TRUE)
}

#' Process Scenario Comparison Data (Adapted to Actual Excel Format)
#' @export
process_scenario_comparison <- function(pathScenarioExcel, pathPU, pathLookupPU, puIsVector, currency) {
  
  # --- HELPER 1: Baca sheet format wide & ubah ke long ---
  read_wide_sheet <- function(path, sheet_num, value_name) {
    df <- readxl::read_excel(path, sheet = sheet_num)
    
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = -c(1, 2), # Kolom 1 = ID, Kolom 2 = KELAS
        names_to = "Tahun",
        values_to = value_name
      ) %>%
      rename(ID = 1, Land_Cover = 2) %>%
      mutate(
        Tahun = as.numeric(Tahun),
        ID = as.numeric(ID),
        !!value_name := as.numeric(.data[[value_name]])
      )
    
    return(df_long)
  }
  
  # --- HELPER 2: Baca sheet EAE (wide tapi tanpa kolom Tahun) ---
  read_eae_sheet <- function(path, sheet_num) {
    df <- readxl::read_excel(path, sheet = sheet_num)
    
    df_clean <- df %>%
      rename(ID = 1, Land_Cover = 2, EAE_BAU = 3, EAE_Intervensi = 4) %>%
      mutate(
        ID = as.numeric(ID),
        EAE_BAU = as.numeric(EAE_BAU),
        EAE_Intervensi = as.numeric(EAE_Intervensi)
      )
    
    return(df_clean)
  }
  
  # =====================================================
  # 1. Baca Sheet Data (dimulai dari Sheet 2 karena Sheet 1 README)
  # =====================================================
  
  # Sheet 2: Luas Proyeksi BAU
  df_area_bau <- read_wide_sheet(pathScenarioExcel, 2, "Luas_BAU")
  
  # Sheet 3: Luas Proyeksi Intervensi
  df_area_int <- read_wide_sheet(pathScenarioExcel, 3, "Luas_Intervensi")
  
  # Sheet 4: Persentase BAU
  df_pct_bau <- read_wide_sheet(pathScenarioExcel, 4, "Pct_BAU")
  
  # Sheet 5: Persentase Intervensi
  df_pct_int <- read_wide_sheet(pathScenarioExcel, 5, "Pct_Intervensi")
  
  # Sheet 6: EAE (BAU & Intervensi dalam satu sheet, tanpa Tahun)
  df_eae <- read_eae_sheet(pathScenarioExcel, 6)
  
  # =====================================================
  # 2. Gabungkan Semua Data
  # =====================================================
  
  # Gabungkan data yang memiliki Tahun (Luas & Persentase)
  df_proj <- df_area_bau %>%
    full_join(df_area_int, by = c("ID", "Land_Cover", "Tahun")) %>%
    full_join(df_pct_bau, by = c("ID", "Land_Cover", "Tahun")) %>%
    full_join(df_pct_int, by = c("ID", "Land_Cover", "Tahun")) %>%
    # Gabungkan EAE (berdasarkan ID & Land_Cover saja, karena EAE berlaku untuk semua tahun)
    left_join(df_eae, by = c("ID", "Land_Cover")) %>%
    arrange(Tahun, ID) %>%
    # Ganti NA dengan 0
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  # =====================================================
  # 3. Hitung Keuntungan Usaha Tani
  # =====================================================
  
  df_proj <- df_proj %>%
    mutate(
      # Keuntungan BAU = EAE BAU * Luas BAU
      Keuntungan_BAU = EAE_BAU * Luas_BAU,
      
      # Keuntungan Intervensi = (Keuntungan BAU * Pct BAU) + (EAE Intervensi * Pct Intervensi * Luas Intervensi)
      Keuntungan_Intervensi = (Keuntungan_BAU * Pct_BAU) + (EAE_Intervensi * Pct_Intervensi * Luas_Intervensi),
      
      # Selisih dan Persentase
      Selisih = Keuntungan_Intervensi - Keuntungan_BAU,
      Persentase_Selisih = ifelse(Keuntungan_BAU > 0, (Selisih / Keuntungan_BAU) * 100, 0)
    )
  
  # =====================================================
  # 4. Agregasi per Tahun untuk Grafik Garis
  # =====================================================
  
  df_yearly <- df_proj %>%
    group_by(Tahun) %>%
    summarise(
      Total_Luas_BAU = sum(Luas_BAU, na.rm = TRUE),
      Total_Luas_Intervensi = sum(Luas_Intervensi, na.rm = TRUE),
      Total_Keuntungan_BAU = sum(Keuntungan_BAU, na.rm = TRUE),
      Total_Keuntungan_Intervensi = sum(Keuntungan_Intervensi, na.rm = TRUE),
      Total_Selisih = sum(Selisih, na.rm = TRUE),
      .groups = "drop"
    )
  
  # =====================================================
  # 5. Hitung Luas per PU (dari Raster/Vector)
  # =====================================================
  
  if (puIsVector) {
    PU_vect <- terra::vect(pathPU)
    
    # Deteksi nama kolom ID dan Nama dari shapefile
    attr_df <- as.data.frame(PU_vect)
    
    id_col <- NULL
    for (nm in names(attr_df)) {
      if (is.numeric(attr_df[[nm]]) && length(unique(attr_df[[nm]])) > 1) {
        id_col <- nm
        break
      }
    }
    if (is.null(id_col)) id_col <- names(attr_df)[1]
    
    name_col <- NULL
    for (nm in names(attr_df)) {
      if (is.character(attr_df[[nm]]) || is.factor(attr_df[[nm]])) {
        name_col <- nm
        break
      }
    }
    if (is.null(name_col)) name_col <- id_col
    
    # Hitung luas per poligon
    PU_vect$Area_Ha <- terra::expanse(PU_vect, unit = "ha")
    
    df_pu_area <- as.data.frame(PU_vect) %>%
      dplyr::rename(
        PU_ID = !!id_col,
        PU_Name = !!name_col,
        Area_Ha = Area_Ha
      ) %>%
      dplyr::select(PU_ID, PU_Name, Area_Ha) %>%
      dplyr::distinct()
    
  } else {
    # Mode Raster
    PU_rast <- terra::rast(pathPU)
    LookupPU <- read_lookup_file(pathLookupPU)
    
    # Deteksi nama kolom nama PU di LookupPU (ambil kolom ke-2 atau cari yang bertipe karakter)
    name_col_pu <- NULL
    for (nm in names(LookupPU)[-1]) {
      if (is.character(LookupPU[[nm]]) || is.factor(LookupPU[[nm]])) {
        name_col_pu <- nm
        break
      }
    }
    if (is.null(name_col_pu)) name_col_pu <- names(LookupPU)[2] # Fallback kolom ke-2
    
    # Hitung luas per kelas di raster PU
    res_m <- terra::res(PU_rast)
    area_per_pixel <- (res_m[1] * res_m[2]) / 10000
    
    freq_table <- as.data.frame(terra::freq(PU_rast))
    
    df_pu_area <- freq_table %>%
      dplyr::rename(PU_ID = value, Count = count) %>%
      dplyr::filter(!is.na(PU_ID)) %>%
      dplyr::mutate(Area_Ha = Count * area_per_pixel) %>%
      # Join dengan lookup menggunakan nama kolom yang sudah dideteksi
      dplyr::left_join(
        LookupPU %>% dplyr::rename(PU_ID = 1, PU_Name = !!name_col_pu), 
        by = "PU_ID"
      ) %>%
      dplyr::select(PU_ID, PU_Name, Area_Ha)
  }
  
  # =====================================================
  # 6. Alokasikan Keuntungan ke Setiap PU
  # =====================================================
  
  # Hitung total luas keseluruhan
  total_area <- sum(df_pu_area$Area_Ha, na.rm = TRUE)
  
  # Hitung proporsi luas tiap PU
  df_pu_area <- df_pu_area %>%
    dplyr::mutate(Proporsi = Area_Ha / total_area)
  
  # Gabungkan dengan data yearly_summary (total keuntungan per tahun)
  # Gunakan nama kolom dengan garis bawah (sesuai output df_yearly asli)
  df_yearly_pu <- df_yearly %>%
    tidyr::crossing(df_pu_area) %>%
    dplyr::mutate(
      `Keuntungan BAU per PU` = Total_Keuntungan_BAU * Proporsi,
      `Keuntungan Intervensi per PU` = Total_Keuntungan_Intervensi * Proporsi,
      `Selisih per PU` = Total_Selisih * Proporsi
    )
  
  return(list(
    projection_table = df_proj,
    yearly_summary = df_yearly,
    pu_summary = df_yearly_pu,
    pu_area = df_pu_area
  ))
}

#' Generate Scenario Report Parameters
#'
#' Prepares all necessary parameters for RMarkdown report generation
#' for the Scenario Comparison module.
#'
#' @param data List output from `process_scenario_comparison()` containing
#'   `projection_table` and `yearly_summary`.
#' @param paths List of file paths used in the analysis.
#' @param times List containing start_time, end_time, etc.
#' @param output_dir Directory where output files are saved.
#' @param currency Currency symbol for display (e.g., "IDR", "USD").
#'
#' @return A comprehensive list of parameters ready for RMarkdown rendering.
#' @export
generate_scenario_report_params <- function(data, paths, times, output_dir, currency) {
  
  # 1. Hitung Total Keuntungan Keseluruhan (untuk Kesimpulan)
  total_keuntungan_bau <- sum(data$projection_table$Keuntungan_BAU, na.rm = TRUE)
  total_keuntungan_intervensi <- sum(data$projection_table$Keuntungan_Intervensi, na.rm = TRUE)
  total_selisih <- total_keuntungan_intervensi - total_keuntungan_bau
  total_persentase_selisih <- ifelse(
    total_keuntungan_bau > 0, 
    (total_selisih / total_keuntungan_bau) * 100, 
    0
  )
  
  # 2. Siapkan Tabel Detail (Projection Table) dengan nama kolom yang rapi
  #    (Opsional: di-rename di sini agar RMarkdown lebih bersih)
  projection_table <- data$projection_table %>%
    dplyr::rename(
      `ID` = ID,
      `Tutupan Lahan` = Land_Cover,
      `Tahun` = Tahun,
      `Luas BAU (Ha)` = Luas_BAU,
      `Luas Intervensi (Ha)` = Luas_Intervensi,
      `EAE BAU` = EAE_BAU,
      `EAE Intervensi` = EAE_Intervensi,
      `Persentase BAU` = Pct_BAU,
      `Persentase Intervensi` = Pct_Intervensi,
      `Keuntungan BAU` = Keuntungan_BAU,
      `Keuntungan Intervensi` = Keuntungan_Intervensi,
      `Selisih` = Selisih,
      `Persentase Selisih (%)` = Persentase_Selisih
    )
  
  # 3. Siapkan Tabel Agregat Tahunan (Yearly Summary) dengan nama kolom yang rapi
  yearly_summary <- data$yearly_summary %>%
    dplyr::rename(
      `Tahun` = Tahun,
      `Total Luas BAU (Ha)` = Total_Luas_BAU,
      `Total Luas Intervensi (Ha)` = Total_Luas_Intervensi,
      `Total Keuntungan BAU` = Total_Keuntungan_BAU,
      `Total Keuntungan Intervensi` = Total_Keuntungan_Intervensi,
      `Total Selisih` = Total_Selisih
    )
  
  # 4. Return semua parameter untuk RMarkdown
  list(
    session_log = format_session_info_table(),
    start_time = format(times$start_time, "%Y-%m-%d %H:%M:%S"),
    end_time = format(times$end_time, "%Y-%m-%d %H:%M:%S"),
    scenario_file_path = paths$pathScenarioExcel,
    pu_file_path = paths$pathPU,
    output_dir = output_dir,
    currency = currency,
    
    # Data Utama
    projection_table = projection_table,
    yearly_summary = yearly_summary,
    
    # Statistik Ringkasan
    total_keuntungan_bau = total_keuntungan_bau,
    total_keuntungan_intervensi = total_keuntungan_intervensi,
    total_selisih = total_selisih,
    total_persentase_selisih = total_persentase_selisih,
    
    pu_summary = data$pu_summary,
    pu_area = data$pu_area
  )
}


# ==============================================================================
# UI HELP DICTIONARY & HELPER FUNCTIONS
# ==============================================================================

#' Profitability Analysis UI Help Text Dictionary
#'
#' Named list of help dialog content (title + HTML body) for each input in the
#' Profitability Analysis Shiny module.
#'
#' @format A named list of lists with \code{title} and \code{body}.
#' @export
profit_help_texts <- list(
  analysis_mode = list(
    title = "Pilih Jenis Analisis",
    body = "<p>Pilih mode analisis profitabilitas yang akan dijalankan:</p>
            <ul>
              <li><b>Analisis Historis:</b> Menganalisis perubahan tutupan lahan berbasis spasial antara dua titik waktu (T1 dan T2), menghitung perubahan profitabilitas (&Delta;EAE), emisi/sekuestrasi karbon, serta kurva biaya peluang (<i>abatement cost curve</i>).</li>
              <li><b>Komparasi Skenario:</b> Membandingkan proyeksi keuntungan ekonomi antara skenario <i>Business As Usual</i> (BAU) dan skenario intervensi berdasarkan tabel data proyeksi Excel multi-sheet.</li>
            </ul>"
  ),
  lulc_t1 = list(
    title = "Peta Tutupan Lahan Awal",
    body = "<p>Berkas raster kategorikal GeoTIFF (.tif) yang merepresentasikan kelas tutupan/penggunaan lahan pada tahun awal (T1).</p>
            <p>Sistem koordinat harus berupa proyeksi (misalnya UTM), dan nilai integer piksel harus bersesuaian dengan ID kelas pada tabel referensi EAE dan tabel cadangan karbon.</p>"
  ),
  year1 = list(
    title = "Tahun Awal",
    body = "<p>Tahun kalender pengamatan peta tutupan lahan awal (T1), misalnya <code>1990</code>.</p>
            <p>Digunakan untuk menentukan selang waktu periode analisis perubahan lahan.</p>"
  ),
  lulc_t2 = list(
    title = "Peta Tutupan Lahan Akhir",
    body = "<p>Berkas raster kategorikal GeoTIFF (.tif) yang merepresentasikan kelas tutupan/penggunaan lahan pada tahun akhir (T2).</p>
            <p>Wajib memiliki batas spasial (<i>extent</i>), resolusi ukuran piksel, dan sistem koordinat yang sama persis dengan peta T1.</p>"
  ),
  year2 = list(
    title = "Tahun Akhir",
    body = "<p>Tahun kalender pengamatan peta tutupan lahan akhir (T2), misalnya <code>2000</code>.</p>
            <p>Nilai tahun akhir (T2) harus lebih besar daripada tahun awal (T1).</p>"
  ),
  scenario_excel = list(
    title = "Data Proyeksi Skenario",
    body = "<p>Berkas spreadsheet Excel (.xlsx) multi-sheet yang memuat proyeksi luasan dan nilai EAE untuk skenario BAU dan Intervensi.</p>
            <p>Format berkas harus memiliki 6 sheet: <code>Luas BAU</code>, <code>Luas Intervensi</code>, <code>EAE BAU</code>, <code>EAE Intervensi</code>, <code>Persentase BAU</code>, dan <code>Persentase Intervensi</code>.</p>"
  ),
  pu_type = list(
    title = "Tipe Peta Unit Perencanaan",
    body = "<p>Pilih format data batas unit perencanaan/wilayah analisis:</p>
            <ul>
              <li><b>Raster (.tif):</b> Menggunakan peta raster kategorikal integer yang didampingi dengan tabel lookup nama unit.</li>
              <li><b>Vektor (.shp):</b> Menggunakan berkas poligon ESRI Shapefile yang mendelineasi batas-batas zona/wilayah.</li>
            </ul>"
  ),
  pu_raster = list(
    title = "Peta Unit Perencanaan",
    body = "<p>Berkas raster kategorikal GeoTIFF (.tif) di mana setiap nilai piksel mewakili ID integer unik unit perencanaan (misalnya batas administrasi kabupaten/kecamatan atau daerah aliran sungai).</p>"
  ),
  pu_table = list(
    title = "Tabel Referensi Unit Perencanaan",
    body = "<p>Berkas tabel (.csv, .xlsx, atau .xls) yang memetakan ID unit perencanaan ke nama deskriptif unit perencanaan.</p>
            <p>Kolom pertama berisi ID numerik unit dan kolom kedua berisi nama unit perencanaan.</p>"
  ),
  pu_vector = list(
    title = "Peta Unit Perencanaan",
    body = "<p>Berkas ESRI Shapefile poligon pembagi unit perencanaan wilayah.</p>
            <p>Unggah seluruh komponen berkas secara bersamaan: <code>.shp</code>, <code>.shx</code>, <code>.dbf</code>, dan <code>.prj</code>.</p>"
  ),
  npv_table = list(
    title = "Tabel Equal Annual Equivalent (EAE)",
    body = "<p>Berkas tabel (.csv, .xlsx, atau .xls) yang memuat nilai keuntungan bersih tahunan yang setara (<i>Equal Annual Equivalent</i>) per hektare (mata uang/ha/tahun) untuk setiap kelas tutupan lahan.</p>
            <p>Wajib memiliki kolom: <code>ID</code> (kode kelas tutupan lahan), <code>LC</code> (nama kelas tutupan lahan), dan <code>EAE</code> (nilai EAE terdiskonto).</p>"
  ),
  cstock_table = list(
    title = "Tabel Referensi Cadangan Karbon (Opsional)",
    body = "<p>Berkas tabel (.csv, .xlsx, atau .xls) yang berisi nilai rata-rata cadangan/stok karbon per kelas tutupan lahan dalam satuan ton C/ha.</p>
            <p>Tiga kolom pertama harus berupa ID kelas, nama kelas, dan nilai stok karbon. Bersifat opsional, namun wajib disediakan jika Anda ingin menghasilkan kurva biaya peluang pengurangan emisi (<i>abatement cost curve</i>).</p>"
  ),
  npv_table_year = list(
    title = "Tahun Acuan Tabel EAE",
    body = "<p>Menentukan tahun acuan nilai moneter yang digunakan dalam tabel EAE:</p>
            <ul>
              <li><b>T1:</b> Nilai EAE dihitung atau mengacu pada tahun awal analisis.</li>
              <li><b>T2:</b> Nilai EAE mengacu pada tahun akhir analisis.</li>
              <li><b>Lainnya:</b> Masukkan tahun survei/penyusunan tabel EAE secara manual jika berbeda dari T1 maupun T2.</li>
            </ul>"
  ),
  currency = list(
    title = "Pilih Mata Uang",
    body = "<p>Satuan mata uang yang digunakan dalam tabel masukan EAE serta penyajian laporan ringkasan analisis (pilihan: <b>IDR</b> untuk Rupiah atau <b>USD</b> untuk Dolar Amerika Serikat).</p>"
  ),
  wd = list(
    title = "Folder Output",
    body = "<p>Direktori tempat seluruh hasil analisis akan disimpan, meliputi:</p>
            <ul>
              <li>Peta spasial sebaran nilai EAE (T1 dan T2) serta perubahannya (&Delta;EAE)</li>
              <li>Tabel ringkasan data perubahan dan biaya pengurangan emisi</li>
              <li>Laporan analisis komprehensif dalam format HTML interaktif</li>
            </ul>"
  )
)

#' Clickable Help Icon for Profitability UI
#'
#' @param id Help topic ID (matches a key in [profit_help_texts]).
#' @return A `shiny.tag` containing an `actionLink` with a circle-question icon.
#' @export
profit_help_icon <- function(id) {
  tags$span(
    class = "alloc-help-icon",
    actionLink(
      inputId = paste0("profit_help_", id),
      label   = NULL,
      icon    = icon("circle-question")
    )
  )
}

#' Label With a Right-Aligned Help Icon
#'
#' @param text Label text.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` combining the label and a help icon.
#' @export
label_with_help <- function(text, help_id) {
  tags$div(
    class = "alloc-label",
    tags$span(text),
    profit_help_icon(help_id)
  )
}

#' Directory-Picker Button With an Inline Help Icon
#'
#' @param id Shiny input ID.
#' @param label Button label.
#' @param title Button title tooltip.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` containing a directory-picker button and help icon.
#' @export
dir_button_with_help <- function(id, label, title, help_id) {
  tags$div(
    class = "dir-btn-wrapper",
    shinyDirButton(id, label, title),
    tags$div(
      class = "dir-help-overlay",
      profit_help_icon(help_id)
    )
  )
}
