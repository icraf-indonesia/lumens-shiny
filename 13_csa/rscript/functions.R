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

format_dt <- function(dt, data) {
  num_cols <- names(data)[sapply(data, is.numeric)]
  
  dt %>%
    formatRound(columns = num_cols, digits = 2) %>%
    formatCurrency(
      columns = num_cols,
      currency = "",
      interval = 3,
      mark = ".",
      dec.mark = ","
    )
}

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
    sf_object1 <- sf::st_read(shp_file)
    sf_object <- st_cast(sf_object1, "MULTIPOLYGON")
    return(sf_object)
  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    stop(paste("Error reading shapefile:", e$message))
  })
}

rasterise_multipolygon <- function(sf_object, raster_res = c(100,100), field = "ID"){
  
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

reclassify_to_binary <- function(raster, target_value) {
  reclass <- function(x) {
    # Use nested ifelse for the reclassification logic:
    # If x equals target_value, return 1
    # Else, if x is NA, return NA
    # Otherwise, return 0
    ifelse(x %in% target_value, 1, ifelse(is.na(x), NA, 0))
  }
  
  app(raster, reclass)
}

preprocess_data <- function(
    pathLULCT,
    zone_type,
    pathPU,
    pathLookupLC,
    pathLookupPU = NULL,
    pathLookupConversion,
    pathLookupPupuk,
    year = NULL
) {
  
  # --- Load packages ---
  library(terra)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(plotly)
  library(tidyterra)
  library(scales)
  library(readxl)
  library(leaflet)
  library(sf)
  library(htmltools)
  library(ggtext)
  
  # -------------------------------
  # 1. READ INPUT DATA
  # -------------------------------
  get_lookup_value <- function(tbl, variable_name) {
    result <- tbl %>%
      mutate(Variable_clean = toupper(trimws(Variable))) %>%
      filter(Variable_clean == toupper(variable_name)) %>%
      pull(Value)
    
    if (length(result) == 0) {
      stop(paste("Variable tidak ditemukan di lookup:", variable_name))
    }
    
    return(result)
  }
  
  get_rotation_factor <- function(tbl, var_name, rotation_vars) {
    value <- get_lookup_value(tbl, var_name)
    total <- tbl %>%
      filter(Name %in% rotation_vars) %>%
      pull(Value) %>%
      sum()
    
    value / total
  }
  
  read_lookup <- function(path) {
    ext <- tools::file_ext(path)
    
    if (ext == "csv") {
      return(readr::read_csv(path, show_col_types = FALSE))
      
    } else if (ext %in% c("xlsx", "xls")) {
      return(readxl::read_excel(path))
      
    } else {
      stop(paste("Format file tidak didukung:", ext))
    }
  }
  
  LookupLC <- read_lookup(pathLookupLC)
  LookupConversion <- read_lookup(pathLookupConversion)
  LookupPupuk <- read_lookup(pathLookupPupuk)
  
  LULCT <- rast(pathLULCT)
  LULCT[LULCT == 0] <- NA
  
  # Set names and levels for LULCT
  name_rast <- names(LULCT)
  levels(LULCT) <- LookupLC
  LULCT <- setNames(LULCT, name_rast)
  
  # Add year if provided
  if (!is.null(year)) {
    year <- as.numeric(year)
    terra::time(LULCT, tstep = "years") <- year
  }
  
  # -------------------------------
  # 1B. READ PLANNING UNITS (PU)
  # -------------------------------
  if (zone_type == "raster") {
    
    PU <- rast(pathPU)
    LookupPU <- read_lookup(pathLookupPU)
    
    name_PU <- names(PU)
    levels(PU) <- LookupPU
    PU <- setNames(PU, name_PU)
    
  } else if (zone_type == "shapefile") {
    
    sf_object <- read_shapefile(pathPU)
    
    if (is.null(sf_object)) {
      stop("Failed to read shapefile. Please check your input.")
    }
    
    # Rename columns
    sf_object <- sf_object %>%
      dplyr::rename(Value = 1, planning_unit = 2)
    
    # Create lookup table from shapefile attributes
    LookupPU <- sf::st_drop_geometry(sf_object)
    
    lc_res <- terra::res(LULCT)
    PU <- rasterise_multipolygon(sf_object, raster_res = lc_res, field = "Value")
    
    levels(PU) <- LookupPU
  }
  
  # -------------------------------
  # 2. HARMONIZE RASTERS
  # -------------------------------
  PU <- terra::resample(PU, LULCT, method = "near")
  
  combinedRaster <- c(PU, LULCT)
  
  # -------------------------------
  # 3A. BUILD FREQUENCY TABLE
  # -------------------------------
  res_m <- terra::res(LULCT)
  area_ha_per_pixel <- (res_m[1] * res_m[2]) / 10000
  
  combinedRasterTable <- combinedRaster %>%
    as_tibble() %>%
    tidyr::drop_na() %>%
    setNames(c("PU", "LC")) %>%
    left_join(
      LookupLC,
      by = c("LC" = "LC")
    ) %>%
    dplyr::filter(sawah == 1) %>%
    group_by(across(c(PU, LC))) %>%
    mutate(Freq = n()) %>%
    ungroup() %>%
    distinct() %>%
    mutate(Ha = Freq * area_ha_per_pixel)
  
  # -------------------------------
  # 3B. LULC COMPOSITION TABLES
  # -------------------------------
  
  # Full LULC (tanpa filter "Sawah")
  lc_table <- combinedRaster %>%
    as_tibble() %>%
    tidyr::drop_na() %>%
    setNames(c("PU", "LC")) %>%
    group_by(PU, LC) %>%
    summarise(n_pixel = n(), .groups = "drop") %>%
    mutate(area_ha = n_pixel * area_ha_per_pixel)
  
  # -------------------------------
  # 3B.1 KOMPOSISI PER PU
  # -------------------------------
  lc_comp_per_PU <- lc_table %>%
    group_by(PU) %>%
    mutate(
      total_area = sum(area_ha),
      percentage = (area_ha / total_area) * 100
    ) %>%
    left_join(
      LookupLC,
      by = c("LC" = "LC")
    ) %>%
    dplyr::filter(sawah == 1) %>% 
    ungroup()
  
  lc_comp_per_PU_tbl <- lc_comp_per_PU %>%
    mutate(
      area_ha = round(area_ha, 2),
      total_area = round(total_area, 2),
      percentage = round(percentage, 2)
    )
  
  # -------------------------------
  # 3B.2 KOMPOSISI TOTAL AREA
  # -------------------------------
  lc_comp_total <- lc_table %>%
    group_by(LC) %>%
    summarise(
      total_area = sum(area_ha),
      .groups = "drop"
    ) %>%
    mutate(
      percentage = (total_area / sum(total_area)) * 100
    )
  
  lc_comp_total_tbl <- lc_comp_total %>%
    mutate(
      total_area = round(total_area, 2),
      percentage = round(percentage, 2)
    )
  
  # -------------------------------
  # 4. PREPARE LOOKUP TABLES
  # -------------------------------
  Lookup_wide <- LookupConversion %>%
    select(Variable, Value) %>%
    pivot_wider(names_from = Variable, values_from = Value) %>%
    mutate(
      Total_EF = EF * SFw * SFs * SFr
    )
  
  # -------------------------------
  # 5. CALCULATE CH4 + N2O -> CO2-eq
  # -------------------------------
  combinedRasterTable <- combinedRasterTable %>%
    mutate(
      CH4_emission = Lookup_wide$Total_EF * Lookup_wide$t * Ha * 1e-6,
      CH4_emission_CO2 = CH4_emission * LookupConversion$Value[LookupConversion$Variable == "GWP_CH4"] * 1000
    )
  combinedRasterTable_clean <- combinedRasterTable
  CH4_table <- combinedRasterTable_clean %>%
    select(-Freq) %>% 
    rename(
      `CH4_emission (Gg CH4/th)` = CH4_emission,
      `CH4_emission_CO2 (ton CO2-eq/tahun)` = CH4_emission_CO2
    )
  
  #### PERHITUNGAN N2O ####
  # Ambil hanya kolom pupuk (berdasarkan pola nama)
  fertilizer_names <- names(LookupPupuk) %>%
    grep("^(Tunggal|Majemuk)_", ., value = TRUE)
  
  # Default: pupuk pertama
  selected_fertilizer <- fertilizer_names[1]
  
  # Ambil N factor
  fert_name_clean <- sub("^(Tunggal|Majemuk)_", "", selected_fertilizer)
  
  lookup_key <- paste0("N_", fert_name_clean)
  
  n_factor <- tryCatch(
    get_lookup_value(LookupConversion, lookup_key),
    error = function(e) NA_real_
  )
  
  if (is.na(n_factor)) {
    stop(paste("N factor tidak ditemukan untuk:", selected_fertilizer))
  }
  
  # Hitung N
  n_table <- LookupPupuk %>%
    mutate(
      N_selected = .data[[selected_fertilizer]] * n_factor
    )
  
  # Lookup factor names
  rotation_vars <- c(
    "Rotasi padi 1 kali",
    "Rotasi padi 2-3 kali",
    "Tidak ditanami padi"
  )
  
  # Extract factors with helper functions
  area100_factor <- get_lookup_value(LookupConversion, "dosis100")
  area50_factor  <- get_lookup_value(LookupConversion, "dosis50")
  
  rotation1_factor <- get_rotation_factor(LookupConversion, "rotasi1", rotation_vars)
  rotation2_factor <- get_rotation_factor(LookupConversion, "rotasi2", rotation_vars)
  
  N2O_emission <- combinedRasterTable %>%
    mutate(
      N2O_area_100_1 = Ha * area100_factor * rotation1_factor,
      N2O_area_100_2 = Ha * area100_factor * rotation2_factor,
      N2O_area_50_1  = Ha * area50_factor  * rotation1_factor,
      N2O_area_50_2  = Ha * area50_factor  * rotation2_factor
    )
  
  # Extract factors with helper functions
  EF_N2O <- get_lookup_value(LookupConversion, "EF_N2O")
  EF_CO2 <- get_lookup_value(LookupConversion, "EF_CO2")
  GWP_N2O <- get_lookup_value(LookupConversion, "GWP_N2O")
  
  N2O_emission_CO2 <- N2O_emission %>%
    mutate(
      N_selected_PU = n_table$N_selected
    ) %>%
    mutate(
      N2O_emission_CO2_100_1 = ((N2O_area_100_1 * N_selected_PU * EF_N2O * GWP_N2O) + (N2O_area_100_1 * N_selected_PU * EF_CO2))/1000,
      N2O_emission_CO2_100_2 = ((N2O_area_100_2 * N_selected_PU  * 2.5 * EF_N2O * GWP_N2O) + (N2O_area_100_2 * N_selected_PU * 2.5 * EF_CO2))/1000,
      N2O_emission_CO2_50_1  = ((N2O_area_50_1 * N_selected_PU  * 0.5 * EF_N2O * GWP_N2O) + (N2O_area_50_1 * N_selected_PU * 0.5 * EF_CO2))/1000,
      N2O_emission_CO2_50_2  = ((N2O_area_50_2 * N_selected_PU  * 2.5 * 0.5 * EF_N2O * GWP_N2O) + (N2O_area_50_2 * N_selected_PU * 2.5 * 0.5 * EF_CO2))/1000,
      # Total N2O emissions across all scenarios (Ton CO2-eq/tahun)
      N2O_emission_CO2_total =
        (N2O_emission_CO2_100_1 +
           N2O_emission_CO2_100_2 +
           N2O_emission_CO2_50_1  +
           N2O_emission_CO2_50_2)
    )
  
  N2O_emission_table <- N2O_emission_CO2 %>%
    select(-Freq, -CH4_emission, -CH4_emission_CO2, -ID, - ID_Traj, - Trajectory, -color_palette, -sawah) %>%
    rename(
      `Total Emisi N2O (Ton CO2-eq/tahun)` = N2O_emission_CO2_total,
      `Luasan Sawah 100% Pemupukan 1x Rotasi (Ha)` = N2O_area_100_1,
      `Luasan Sawah 100% Pemupukan 2-3x Rotasi (Ha)` = N2O_area_100_2,
      `Luasan Sawah 50% Pemupukan 1x Rotasi (Ha)` = N2O_area_50_1,
      `Luasan Sawah 50% Pemupukan 2-3x Rotasi (Ha)` = N2O_area_50_2,
      `N Terpilih` = N_selected_PU,
      `Emisi 100% Pemupukan 1x Rotasi (Ton CO2-eq/tahun)` = N2O_emission_CO2_100_1,
      `Emisi 100% Pemupukan 2-3x Rotasi (Ton CO2-eq/tahun)` = N2O_emission_CO2_100_2,
      `Emisi 50% Pemupukan 1x Rotasi (Ton CO2-eq/tahun)` = N2O_emission_CO2_50_1,
      `Emisi 50% Pemupukan 2-3x Rotasi (Ton CO2-eq/tahun)` = N2O_emission_CO2_50_2
    ) %>%
    select(
      PU,
      LC,
      Ha,
      `Total Emisi N2O (Ton CO2-eq/tahun)`,
      everything()
    )
  
  # -------------------------------
  # 6A. SUM BY PU
  # -------------------------------
  summary_by_PU <- N2O_emission_CO2 %>%
    group_by(PU) %>%
    summarise(
      CH4_emission_CO2 = sum(CH4_emission_CO2, na.rm = TRUE),
      N2O_emission_CO2_total = sum(N2O_emission_CO2_total, na.rm = TRUE),
      `Total Emisi (Ton CO2-eq/tahun)` = 
        CH4_emission_CO2 + N2O_emission_CO2_total,
      .groups = "drop"
    )
  
  PU_emission_table <-  summary_by_PU %>% 
    rename(
      `Emisi CH4 (Ton CO2-eq/tahun)` = CH4_emission_CO2,
      `Emisi N2O (Ton CO2-eq/tahun)`= N2O_emission_CO2_total
    )
  
  # -------------------------------
  # PERCENTAGE CONTRIBUTION
  # -------------------------------
  summary_by_PU <- summary_by_PU %>%
    mutate(
      CH4_percent =
        (CH4_emission_CO2 /
           `Total Emisi (Ton CO2-eq/tahun)`) * 100,
      
      N2O_percent =
        (N2O_emission_CO2_total /
           `Total Emisi (Ton CO2-eq/tahun)`) * 100
    )
  
  # -------------------------------
  # CONVERT TO LONG FORMAT
  # -------------------------------
  summary_long <- summary_by_PU %>%
    pivot_longer(
      cols = c(CH4_emission_CO2, N2O_emission_CO2_total),
      names_to = "Gas",
      values_to = "Value"
    ) %>%
    
    mutate(
      Gas = dplyr::recode(
        Gas,
        "CH4_emission_CO2" = "CH₄",
        "N2O_emission_CO2_total" = "N₂O"
      ),
      
      Percentage = dplyr::case_when(
        Gas == "CH₄" ~ CH4_percent,
        Gas == "N₂O" ~ N2O_percent
      )
    )
  
  epsilon <- 1e-6
  
  summary_long <- summary_long %>%
    mutate(
      Value_log = log10(Value + epsilon)
    )
  
  # -------------------------------
  # 6B. INTERACTIVE CHOROPLETH MAP
  # -------------------------------
  
  # Convert raster PU menjadi polygon
  PU_poly <- terra::as.polygons(PU, dissolve = TRUE)
  
  # Convert ke sf
  PU_sf <- sf::st_as_sf(PU_poly)
  
  # Rename kolom pertama menjadi PU
  names(PU_sf)[1] <- "PU"
  
  # Rapikan tipe data join
  PU_sf$PU <- trimws(as.character(PU_sf$PU))
  summary_by_PU$PU <- trimws(as.character(summary_by_PU$PU))
  
  # Join data emisi
  PU_sf <- PU_sf %>%
    left_join(summary_by_PU, by = "PU") %>%
    mutate(
      CH4_percent =
        (CH4_emission_CO2 /
           `Total Emisi (Ton CO2-eq/tahun)`) * 100,
      
      N2O_percent =
        (N2O_emission_CO2_total /
           `Total Emisi (Ton CO2-eq/tahun)`) * 100
    )
  
  # IMPORTANT:
  # Transform ke WGS84
  PU_sf <- sf::st_transform(PU_sf, 4326)
  
  # Palette warna
  pal <- leaflet::colorNumeric(
    palette = "YlOrRd",
    domain = PU_sf$`Total Emisi (Ton CO2-eq/tahun)`,
    na.color = "transparent"
  )
  
  # Tooltip
  popup_text <- paste0(
    "<strong>Unit Perencanaan:</strong> ", PU_sf$PU, "<br><br>",
    
    "<strong>Total Emisi:</strong> ",
    scales::comma(
      round(PU_sf$`Total Emisi (Ton CO2-eq/tahun)`, 2)
    ),
    " Ton CO₂-eq/tahun<br><br>",
    
    "<strong>Emisi CH₄:</strong> ",
    scales::comma(
      round(PU_sf$CH4_emission_CO2, 2)
    ),
    " Ton CO₂-eq/tahun",
    
    " (",
    round(PU_sf$CH4_percent, 2),
    "%)<br>",
    
    "<strong>Emisi N₂O:</strong> ",
    scales::comma(
      round(PU_sf$N2O_emission_CO2_total, 2)
    ),
    " Ton CO₂-eq/tahun",
    
    " (",
    round(PU_sf$N2O_percent, 2),
    "%)"
  )
  
  # Interactive map
  plot_choropleth <- leaflet::leaflet(PU_sf) %>%
    
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron
    ) %>%
    
    leaflet::addPolygons(
      fillColor = ~pal(`Total Emisi (Ton CO2-eq/tahun)`),
      color = "white",
      weight = 1,
      fillOpacity = 0.8,
      
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      
      label = lapply(popup_text, htmltools::HTML)
    ) %>%
    
    leaflet::addLegend(
      pal = pal,
      values = ~`Total Emisi (Ton CO2-eq/tahun)`,
      title = "Total Emisi<br>(Ton CO₂-eq/tahun)",
      position = "bottomright"
    )
  
  # -------------------------------
  # 7. PLOT
  # -------------------------------
  min_log <- floor(min(summary_long$Value_log, na.rm = TRUE))
  max_log <- ceiling(max(summary_long$Value_log, na.rm = TRUE))
 
  log_breaks <- seq(min_log, max_log, by = 1)
  
  log_labels <- paste0(
    "10<sup>",
    log_breaks,
    "</sup>"
  )
  
  p <- ggplot(
    summary_long,
    
    aes(
      x = reorder(PU, Value),
      y = Value,
      fill = Gas,
      
      text = paste0(
        "<b>Unit Perencanaan:</b> ", PU, "<br>",
        "<b>Gas:</b> ", Gas, "<br>",
        "<b>Emisi:</b> ",
        scales::comma(Value, accuracy = 0.01),
        " Ton CO₂-eq/tahun<br>",
        "<b>Kontribusi:</b> ",
        round(Percentage, 2),
        "%"
      )
    )
  ) +
    
    geom_col(position = "stack") +
    
    scale_y_log10(
      breaks = 10^log_breaks,
      labels = log_labels
    ) +
    
    annotation_logticks(
      sides = "b",
      
      short = unit(0.08, "cm"),
      mid   = unit(0.12, "cm"),
      long  = unit(0.18, "cm")
    ) +
    
    scale_fill_manual(
      values = c(
        "CH₄" = "#1b9e77",
        "N₂O" = "#d95f02"
      )
    ) +
    
    labs(
      title = "Komposisi Emisi CH₄ dan N₂O per Unit Perencanaan",
      x = "Unit Perencanaan",
      y = "Emisi (Ton CO₂-eq/tahun, skala log)",
      fill = "Jenis Gas"
    ) +
    
    theme_bw() +
    
    theme(
      axis.text.y = ggtext::element_markdown(),
      legend.text = ggtext::element_markdown(),
      
      panel.grid.minor = element_blank(),
      
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(0.25, "cm")
    ) +
    
    coord_flip()
  
  plot_interactive <- ggplotly(
    p,
    tooltip = "text"
  )
  
  # -------------------------------
  # 8. PADDY AND NON-PADDY MAP
  # -------------------------------
  paddy_values <- LookupLC %>% 
    filter(sawah == 1) %>% 
    pull(ID)
  
  paddy_map <- reclassify_to_binary(LULCT, paddy_values)
  
  # Convert to factor
  paddy_map_factor <- as.factor(paddy_map)
  
  fill_scale <- scale_fill_manual(
    values = c("0" = "orange",   # non-sawah
               "1" = "darkgreen"  # sawah
    ),
    na.value = "white",
    labels = c("Non-Sawah", "Sawah"),
    na.translate = FALSE
  )
  
  plot_paddy_map <- ggplot() +
    geom_spatraster(data = paddy_map_factor) +
    coord_sf() +
    fill_scale +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_rect(colour = 'grey32')
    ) +
    guides(fill = guide_legend(reverse = TRUE))
  
  # -------------------------------
  # 9. COUNT PADDY & NON-PADDY BY PU
  # -------------------------------
  
  # Combine PU and paddy raster
  combined_paddy <- c(PU, paddy_map)
  
  # Convert raster stack to table
  paddy_table <- terra::as.data.frame(combined_paddy, na.rm = TRUE) %>%
    setNames(c("PU", "sawah")) %>%
    group_by(PU, sawah) %>%
    summarise(n_pixel = n(), .groups = "drop") %>%
    mutate(area_ha = n_pixel * area_ha_per_pixel)
  
  # Convert PADDY=0/1 to labels
  paddy_table$Class <- ifelse(paddy_table$sawah == 1, "Sawah", "Non-Sawah")
  
  paddy_table <- paddy_table %>%
    group_by(PU) %>%
    mutate(total_area = sum(area_ha)) %>%
    ungroup()
  
  # -------------------------------
  # 10. PLOT STACKED BAR CHART
  # -------------------------------
  plot_paddy_bar <- ggplot(
    paddy_table,
    aes(
      x = reorder(PU, -total_area),  # ✅ ascending by total area
      y = area_ha,
      fill = Class,
      text = paste0(
        "Unit Perencanaan: ", PU, "<br>",
        "Kelas: ", Class, "<br>",
        "Area (Ha): ", scales::number(
          area_ha,
          accuracy = 0.01,
          big.mark = ",",
          decimal.mark = "."
        )
        )
      )
    ) +
    geom_col() +
    scale_fill_manual(values = c(
      "Non-Sawah" = "orange",
      "Sawah" = "darkgreen"
    )) +
    scale_y_continuous(
      labels = scales::label_comma(
        accuracy = 1,
        big.mark = ",",
        decimal.mark = "."
      )
    ) +
    labs(
      title = "Area Sawah vs Non-Sawah per Unit Perencanaan",
      x = "Unit Perencanaan",
      y = "Area (Ha)",
      fill = "Class"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  plot_paddy_bar_interactive <- plotly::ggplotly(
    plot_paddy_bar,
    tooltip = "text"
  )
  
  # -------------------------------
  # RETURN ALL RESULTS
  # -------------------------------
  return(list(
    session_log = format_session_info_table(),
    lulc_file_path = pathLULCT,
    pu_file_path = pathPU,
    lookup_pu_file_path = pathLookupPU,
    lookup_lc_file_path = pathLookupLC,
    lookup_conversion_file_path = pathLookupConversion,
    lookup_pupuk_file_path = pathLookupPupuk,
    LULCT = LULCT,
    PU = PU,
    lookup_LC = LookupLC,
    lookup_PU = LookupPU,
    lookup_Conversion = LookupConversion,
    combinedRaster = combinedRaster,
    combinedRasterTable = combinedRasterTable,
    lc_comp_per_PU = lc_comp_per_PU_tbl,
    lc_comp_total = lc_comp_total_tbl,
    CH4_table = CH4_table,
    N2O_emission_table = N2O_emission_table,
    summary_by_PU = summary_by_PU,
    PU_emission_table = PU_emission_table,
    summary_long = summary_long,
    plot = plot_interactive,
    plot_paddy_map = plot_paddy_map,
    plot_paddy_bar_interactive = plot_paddy_bar_interactive,
    plot_choropleth = plot_choropleth
  ))
}
