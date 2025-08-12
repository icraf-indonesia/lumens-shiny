library(terra)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(rmarkdown)

### Function ####
check_and_harmonise_geometry <- function(raster_map, reference_map) {
  if (!terra::compareGeom(raster_map, reference_map, stopOnError = FALSE)) {
    warning("Geometry mismatch detected. Resampling raster_map to match reference_map.")
    raster_map <- terra::resample(raster_map, reference_map, method = "near")
    message("Harmonization complete: raster_map now matches reference_map geometry.")
  } else {
    message("Geometries are already consistent.")
  }
  return(raster_map)
}

rasterise_multipolygon_quesc <- function(sf_object, raster_res, field = "ID") {
  # Error checking
  if (!inherits(sf_object, "sf")) stop("sf_object must be an sf object.")
  if (!all(sf::st_geometry_type(sf_object) == "MULTIPOLYGON")) stop("All features in sf_object must be MULTIPOLYGONs.") # Check if sf_object has UTM projection
  if (!grepl("\\+units=m", sf::st_crs(sf_object)$proj4string)) stop("sf_object must have UTM projection system.")
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

generate_dummy_crosstab <- function(landcover, zone) {
  if (!is.data.frame(landcover)) {
    stop("Land cover is not a data frame")
  }
  
  if (!is.data.frame(zone)) {
    stop("Zone is not a data frame")
  }
  
  n_lc <- nrow(landcover)
  n_pu <- nrow(zone)
  
  dummy1 <- data.frame(nPU = zone[[1]], divider = n_lc * n_lc)
  dummy1 <- splitstackshape::expandRows(dummy1, "divider")
  
  dummy2 <- data.frame(nT1 = landcover[[1]], divider = n_lc)
  dummy2 <- splitstackshape::expandRows(dummy2, "divider")
  dummy2 <- data.frame(nT1 = rep(dummy2$nT1, n_pu))
  
  dummy3 <- data.frame(nT2 = rep(rep(landcover[[1]], n_lc), n_pu))
  
  lucDummy <- cbind(dummy1, dummy2, dummy3)
  colnames(lucDummy) <- c("ID_PU", "ID_LC1", "ID_LC2")
  return(tibble::tibble(lucDummy))
}

# Start timing
start_time <- Sys.time()

# Define data directory
data_dir <- "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/07_ta-profit/rscript/tests"

# File paths
map1_path <- file.path(data_dir, "data/NTT/NTT_2000V3F.tif")
map2_path <- file.path(data_dir, "data/NTT/NTT_2010V3F.tif")
npv_path <- file.path(data_dir, "data/profitability_table.csv")
c_lookup_path <- file.path(data_dir, "data/NTT/carbon_NTT_kp.csv")
admin_z_path <- file.path(data_dir, "data/NTT/vector/Admin51s_F_join.shp")
year1 <- t1 <- 2000
year2 <- t2 <- 2010

map1_path <- "data/raster/bungo_landcover_1990r.tif"
map2_path <- "data/raster/bungo_landcover_2000r.tif"
c_lookup_path <- "data/table/carbon_bungo.csv"
admin_z_path <- "data/vector/planning_unit_bungo.shp"
year1 <- t1 <- 1990
year2 <- t2 <- 2000

# Additional parameters
period <- year2 - year1
raster_nodata <- 0
output_dir <- file.path(data_dir, "output")

# Read and process carbon data
c_lookup_input <- readr::read_csv(c_lookup_path)

lc_t1 <- map1_path %>%
  terra::rast() %>%
  LUMENSR::add_legend_to_categorical_raster(
    lookup_table = c_lookup_input,
    year = as.numeric(t1)
  )

lc_t2 <- map2_path %>%
  terra::rast() %>%
  LUMENSR::add_legend_to_categorical_raster(
    lookup_table = c_lookup_input,
    year = as.numeric(t2)
  ) %>%
  check_and_harmonise_geometry(reference_map = lc_t1)

# read polygon
zone_sf1 <- sf::st_read(admin_z_path)
zone_sf <- sf::st_cast(zone_sf1, "MULTIPOLYGON")
zone <- zone_sf %>%
  rasterise_multipolygon_quesc(
    raster_res = res(lc_t1), 
    field = paste0(colnames(sf::st_drop_geometry(zone_sf[1]))) 
  )
zone_lookup_input <- data.frame(ID_PU = zone_sf[[1]], PU = zone_sf[[2]])

zone <- zone %>%
  check_and_harmonise_geometry(reference_map = lc_t1)

preques <- LUMENSR::ques_pre(lc_t1, lc_t2, zone)
period_year <- as.numeric(t1) - as.numeric(t2)
lucDummy <- generate_dummy_crosstab(c_lookup_input, zone_lookup_input)

# join table
df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC1 = 1, C_T1 = 3) %>% dplyr::select(1:3) %>%
  dplyr::rename_with(.cols = 2, ~as.character(t1)) %>% dplyr::right_join(lucDummy, by="ID_LC1")
df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC2 = 1, C_T2 = 3) %>% dplyr::select(1:3) %>%
  dplyr::rename_with(.cols = 2, ~as.character(t2)) %>% dplyr::right_join(df_lucdb, by="ID_LC2")
df_lucdb <- zone_lookup_input %>% dplyr::rename(ID_PU = 1) %>%
  dplyr::rename_with(.cols = 2, ~names(zone)) %>% dplyr::right_join(df_lucdb, by="ID_PU") 
df_lucdb <- df_lucdb %>%
  dplyr::left_join(
    preques[["landscape_level"]][["crosstab_long"]],
    by = c(names(zone), t1, t2)
  )

# the full version of preques database from preques analysis combined with all possible landcover listed in the lookup table
df_lucdb <- df_lucdb %>%
  replace(is.na(df_lucdb), 0) %>%
  dplyr::rename(PU = names(zone))

# generate quescdatabase
df_lucdb <- df_lucdb %>% dplyr::mutate(
  EM = (C_T1 - C_T2) * (C_T1 > C_T2) * Ha * 3.67,
  SQ = (C_T2 - C_T1) * (C_T1 < C_T2) * Ha * 3.67,
  
  LU_CHG = do.call(paste, c(df_lucdb[c(as.character(t1), as.character(t2))], sep = " to "))
  
)

nodb_quesc_tbl <- df_lucdb
nodb_tbl_carbon <- nodb_quesc_tbl %>% dplyr::select(ID = 1, Carbon = C_T1) #kolom karbon yang dimaksud yg mana?

# Read and process NPV data
tbl_npv <- readr::read_csv(npv_path, show_col_types = FALSE) %>%
  dplyr::select(ID_LC = 1, NPV = 3)


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

prepare_npv_lookup <- function(tbl_npv, nodb_quesc_tbl) {
  colnames(tbl_npv)[1:2] <- c("ID_LC1", "NPV1")
  dt_quesc_npv <- merge(nodb_quesc_tbl, tbl_npv, by = "ID_LC1")
  colnames(tbl_npv)[1:2] <- c("ID_LC2", "NPV2")
  dt_quesc_npv <- merge(dt_quesc_npv, tbl_npv, by = "ID_LC2")
  tot_area <- sum(dt_quesc_npv$Ha, na.rm = TRUE)
  list(dt_quesc_npv = dt_quesc_npv, tot_area = tot_area)
}

build_opcost_table <- function(dt_quesc_npv, period, tot_area) {
  data_em_sel <- dt_quesc_npv[dt_quesc_npv$EM > 0, ]
  
  data_em_sel <- within(data_em_sel, {
    em_rate <- ((C_T1 - C_T2) * (Ha * 3.67)) / (tot_area * period)
    em_tot <- (C_T1 - C_T2) * 3.67
    opcost <- (NPV2 - NPV1) / em_tot
  })
  
  opcost_tab <- data.frame(
    luchg = data_em_sel$LU_CHG,
    zone = data_em_sel$PU,
    opcost = data_em_sel$opcost,
    emrate = data_em_sel$em_rate
  )
  
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

carbon_accounting <- function(map1, map2, tbl_npv, nodb_tbl_carbon, raster_nodata) {
  NAflag(map1) <- as.numeric(raster_nodata)
  NAflag(map2) <- as.numeric(raster_nodata)
  
  names(tbl_carbon)[names(tbl_carbon) == "ID"] <- "ID_LC"
  merged_data <- merge(tbl_npv, tbl_carbon, by = "ID_LC")
  ta_reclassify_matrix <- as.matrix(merged_data[, c("ID_LC", "Carbon")])
  
  map_carbon1 <- terra::classify(map1, reclassify_matrix)
  map_carbon2 <- terra::classify(map2, reclassify_matrix)
  
  chk_em <- map_carbon1 > map_carbon2
  emission_map <- ((map_carbon1 - map_carbon2) * 3.67) * chk_em
  
  list(map_carbon1 = map_carbon1, map_carbon2 = map_carbon2, emission_map = emission_map)
}

npv_accounting <- function(map1, map2, tbl_npv) {
  npv_matrix <- as.matrix(tbl_npv[, c("ID_LC", "NPV")])
  
  map_npv1 <- terra::classify(map1, npv_matrix)
  map_npv2 <- terra::classify(map2, npv_matrix)
  
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
    emission_rate = opcost_table$emrate,
    opportunity_cost = opcost_table$opcost,
    land_use_change = opcost_table$luchg
  )
  
  # Group data by land use change
  df_grouped <- df_curve %>%
    group_by(land_use_change) %>%
    summarise(emission_rate = sum(emission_rate),
              opportunity_cost = sum(opportunity_cost), .groups = "drop")
  
  # Filter and order data
  df_all <- df_grouped %>% filter(opportunity_cost != 0)
  df_order <- df_all[order(df_all$opportunity_cost),]
  df_order$order <- seq_len(nrow(df_order))
  
  # Create ggplot
  opcost_curve <- ggplot(df_order, aes(x = order, y = opportunity_cost, 
                                       text = paste0("Land Use Change:", land_use_change,
                                                     "<br>Opportunity Cost:", round(opportunity_cost, 3),
                                                     "<br>Emission Rate:", round(emission_rate, 6)))) +
    geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
    labs(x = NULL,
         y = "Opportunity Cost ($/ton CO2-eq)") +
    theme_classic() +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(face = "bold", angle = 90)) +
    coord_cartesian(ylim = c(-5000, 5000))
  
  # Convert to interactive plotly object
  opcost_curve_interactive <- plotly::ggplotly(opcost_curve, tooltip = "text")
  
  return(list(plot = opcost_curve_interactive, data = df_order))
}

# Prepare NPV lookup table by combining carbon and NPV data, and calculate total area
npv_result <- prepare_npv_lookup(tbl_npv, nodb_quesc_tbl)
nodb_dt_quesc_npv <- npv_result$dt_quesc_npv
tot_area <- npv_result$tot_area

# Build the opportunity cost table based on the land use change period and total area
opcost_result <- build_opcost_table(dt_quesc_npv, period, tot_area)
opcost_table <- opcost_result$opcost_all
opcost_table$order <- c(1:nrow(opcost_table))

# Perform carbon accounting and calculate carbon emissions based on land use change
carbon_result <- carbon_accounting(map1, map2, tbl_npv, tbl_carbon, raster_nodata)
map_carbon1 <- carbon_result$map_carbon1
map_carbon2 <- carbon_result$map_carbon2
emission_map <- carbon_result$emission_map

# Perform NPV accounting to calculate changes in NPV between the two time periods
npv_change <- npv_accounting(map1, map2, tbl_npv)
map_npv1 <- npv_change$map_npv1
map_npv2 <- npv_change$map_npv2
npv_chg_map <- npv_change$npv_chg_map

# Calculate the opportunity cost map by combining NPV changes with carbon emissions
opcost_map <- calculate_opcost_map(npv_chg_map, emission_map)

# Generate output maps and save them in the selected directory
generate_output_maps(map_carbon1, map_carbon2, emission_map, opcost_map, output_dir)

# Generate the opportunity cost curve for visualization
df_opcost_curve <- generate_opportunity_cost_curve(opcost_table)
opcost_curve <- df_opcost_curve$plot
opcost_curve_table <- df_opcost_curve$data

# Report parameters
report_params <- list(
  session_log = format_session_info_table(),
  start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
  end_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  opcost_table = opcost_table,
  opcost_curve_table = opcost_curve_table,
  opcost_curve = opcost_curve,
  map1_file_path = map1_path,
  map2_file_path = map2_path,
  npv_file_path = npv_path,
  npv1_map = map_npv1,
  npv2_map = map_npv2,
  delta_npv = npv_chg_map,
  carbon_file_path = carbon_path,
  year1 = year1,
  year2 = year2,
  raster_nodata = raster_nodata,
  output_dir = output_dir
)

# Render report
output_file <- paste0("ta-profit_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
rmarkdown::render(
  input = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/07_ta-profit/report_template/ta-profit_report.Rmd",
  output_file = output_file,
  output_dir = output_dir,
  params = report_params,
  envir = new.env(parent = globalenv())
)
