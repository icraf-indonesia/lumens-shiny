library(LUMENSR)
library(terra)

# Define data directory
data_dir <- "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/07_ta-profit/rscript/tests"
output_dir <- file.path(data_dir, "output")

lc_t1_path <- file.path(data_dir, "data/NTT/NTT_2000V3F.tif")
lc_t2_path <- file.path(data_dir, "data/NTT/NTT_2010V3F.tif")
c_lookup_path <- file.path(data_dir, "data/NTT/carbon_NTT_kp.csv")
admin_z_path <- file.path(data_dir, "data/NTT/vector/Admin51s_F_join.shp")

t1 <- 2000
t2 <- 2010

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

### Calculation ####
# read table
c_lookup_input <- readr::read_csv(c_lookup_path)

lc_t1 <- lc_t1_path %>%
  terra::rast() %>%
  LUMENSR::add_legend_to_categorical_raster(
    lookup_table = c_lookup_input,
    year = as.numeric(t1)
  )

lc_t2 <- lc_t2_path %>%
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

# create new matrix reclassification
ques_reclassify_matrix <- as.matrix(c_lookup_input[, 1]) %>%
  cbind(., as.matrix(c_lookup_input[, 3]) ) %>%
  rbind(., c(0, NA))
ques_reclassify_matrix <- as.data.frame(ques_reclassify_matrix)

# create all maps
ques_map_carbon1 <- lc_t1 %>% terra::classify(ques_reclassify_matrix)
ques_map_carbon2 <- lc_t2 %>% terra::classify(ques_reclassify_matrix)
ques_map_emission <- ((ques_map_carbon1 - ques_map_carbon2) * 3.67) * (ques_map_carbon1 > ques_map_carbon2)
ques_map_sequestration <- ((ques_map_carbon2 - ques_map_carbon1) * 3.67) * (ques_map_carbon1 < ques_map_carbon2)

# quescdatabase
df_lucdb <- df_lucdb %>% dplyr::mutate(
  EM = (C_T1 - C_T2) * (C_T1 > C_T2) * Ha * 3.67,
  SQ = (C_T2 - C_T1) * (C_T1 < C_T2) * Ha * 3.67,
  
  LU_CHG = do.call(paste, c(df_lucdb[c(as.character(t1), as.character(t2))], sep = " to "))
  
)


### Calculation from carbon_accounting() ####
npv_path <- file.path(data_dir, "data/profitability_table.csv")
carbon_path <- file.path(data_dir, "data/NTT/quesc_database.csv")
raster_nodata <- 0

# Read raster data
map1 <- terra::rast(lc_t1_path)
map2 <- terra::rast(lc_t2_path)

# Align map2 with map1 if needed
if (!terra::compareGeom(map1, map2, stopOnError = FALSE)) {
  map2 <- terra::resample(map2, map1)
}

# Read and process carbon data
quesc_tbl <- readr::read_csv(carbon_path, show_col_types = FALSE)
tbl_carbon <- quesc_tbl %>% dplyr::select(ID = 1, Carbon = C_T1)

# Read and process NPV data
tbl_npv <- readr::read_csv(npv_path, show_col_types = FALSE) %>%
  dplyr::select(ID_LC = 1, NPV = 3)

carbon_accounting <- function(map1, map2, tbl_npv, tbl_carbon, raster_nodata) {
  NAflag(map1) <- as.numeric(raster_nodata)
  NAflag(map2) <- as.numeric(raster_nodata)
  
  names(tbl_carbon)[names(tbl_carbon) == "ID"] <- "ID_LC"
  merged_data <- merge(tbl_npv, tbl_carbon, by = "ID_LC")
  reclassify_matrix <- as.matrix(merged_data[, c("ID_LC", "Carbon")])
  ta_reclassify_matrix <- as.data.frame(reclassify_matrix)
  
  map_carbon1 <- terra::classify(map1, reclassify_matrix)
  map_carbon2 <- terra::classify(map2, reclassify_matrix)
  
  chk_em <- map_carbon1 > map_carbon2
  emission_map <- ((map_carbon1 - map_carbon2) * 3.67) * chk_em
  
  list(map_carbon1 = map_carbon1, map_carbon2 = map_carbon2, emission_map = emission_map)
}

# Perform carbon accounting and calculate carbon emissions based on land use change
carbon_result <- carbon_accounting(map1, map2, tbl_npv, tbl_carbon, raster_nodata)
ta_map_carbon1 <- carbon_result$map_carbon1
ta_map_carbon2 <- carbon_result$map_carbon2
ta_emission_map <- carbon_result$emission_map

are_exactly_equal <- terra::all.equal(ta_map_carbon1, ques_map_carbon1)
print(are_exactly_equal)

mismatch_count <- sum(values(ta_map_carbon1) != values(ques_map_carbon1), na.rm = TRUE)
print(paste("Mismatched cells:", mismatch_count))

match_mask <- values(ta_map_carbon1) == values(ques_map_carbon1)
match_count <- sum(match_mask, na.rm = TRUE)  # Number of matching cells
print(paste("Matching cells count:", match_count))

summary(ta_map_carbon1)
summary(ques_map_carbon1)

writeRaster(ta_map_carbon1, file.path(output_dir, "ta_carbon_map_t1.tif"), overwrite = TRUE)
writeRaster(ques_map_carbon1, file.path(output_dir, "ques_carbon_map_t1.tif"), overwrite = TRUE)
