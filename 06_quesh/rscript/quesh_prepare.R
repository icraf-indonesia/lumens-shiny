library(terra)
library(dplyr)

syncGeom <- function(input, ref){
  input %>% 
    # rast() %>%
    crop(ref) %>% 
    resample(ref) %>% 
    `*`(ref)
}

#input data

r_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/RainfallErosivity_lup_EbM.tif" %>% rast()
k_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/ErodibilityUTM_EbM.tif"%>% rast()
ls_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/LS_EbM.tif"%>% rast()
cp_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/cp00_lup_EbM.tif"%>% rast()
zone <- "data/raster/Zona_Bungo.tif"
srtm <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/SRTM30m_DAS_ADM_F.tif" %>% rast()
r_gloreda <- "data/data_quesh/"

output_directory <- "data/data_quesh/climate/prec_monthly_1970-2000_wc2.1/prec_bungo/"

# load a reference map
base_map <- rast(zone) %>% classify(cbind(1:8, 1))
writeRaster(base_map, paste0(output_directory, "Base_map.tif"))

# load factors map
r_map <- syncGeom(input = rf2, ref = base_map)
writeRaster(r_map, paste0(output_directory, "R_factor_bungo.tif"))

# load factors map
k_map <- syncGeom(input = k_file, ref = base_map)
writeRaster(k_map, paste0(output_directory, "K_factor.tif"))

# load ls map
ls_map <- syncGeom(input = ls_file, ref = base_map)
writeRaster(ls_map, paste0(output_directory, "LS_factor.tif"))

# load cp map
cp_map <- syncGeom(input = cp_file, ref = base_map)
writeRaster(k_map, paste0(output_directory, "CP_factor.tif"))

# load srtm map
srtm_map <- syncGeom(input = srtm, ref = base_map)
writeRaster(srtm, paste0(output_directory, "SRTM_Bungo.tif"))

# load r gloreda map
r_gloreda_map <- syncGeom(input = r_gloreda, ref = base_map)
writeRaster(r_gloreda_map, paste0(output_directory, "R_factor_Gloreda.tif"), overwrite = TRUE)

# # calculate RUSLE
a <- r_map*k_map*ls_map*cp_map
plot(a)

a_log <- log10(a)
plot(a_log)

#########################
library(era5)
library(ecmwfr)

# Set up your API key
wf_set_key(user = "Faza Mahezs", key = "3475ea0f-d69a-46a4-bfcb-5d08dab1ffc8", service = "cds")

# Define parameters for ERA5 data download (total precipitation)
precip_data <- wf_request(
  dataset = "reanalysis-era5-single-levels",
  request = list(
    "variable" = "total_precipitation",
    "product_type" = "monthly_averaged_reanalysis",
    "year" = as.character(2000:2023),
    "month" = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    "time" = "00:00",
    "area" = c(-10, 95, 5, 141),  # Indonesia extent (south, west, north, east)
    "format" = "netcdf"
  ),
  transfer = TRUE,
  path = "path/to/store/data"
)

######################
for (i in 1:nlyr(rf3)) {
  # Get the raster for the current month
  month_raster <- rf3[[i]]
  
  # Define the file name for each monthly raster
  month_name <- sprintf("%s/prec_bungo_month_%02d.tif", output_directory, i)
  
  # Save the individual raster to a file
  writeRaster(month_raster, month_name, overwrite = TRUE)
  
  cat("Saved:", month_name, "\n")
}

######################
prec1_projected <- project(prec1, "EPSG:32748")
prec1_resampled <- resample(prec1_projected, aoi_file, method = "bilinear")

######################
aoi_file <- "data/data_quesh/Base_map.tif" %>% rast()
aoi_geo<-project(aoi_file, "+proj=longlat +datum=WGS84")

rainfalls <-
  list.files(path = "data/data_quesh/climate/prec_monthly_1970-2000_wc2.1/prec_bungo/",
             pattern = ".tif",
             full.names = TRUE) |>
  map(rast) |>
  map(~ crop(.x, aoi_geo)) |> 
  map(~project(.x, aoi_file)) 

rf_all <- list.files(path = "data/data_quesh/climate/prec_monthly_1970-2000_wc2.1/prec_bungo/",
           pattern = ".tif",
           full.names = TRUE) 
rf_all_rast <- rf_all %>% rast()
# rf <- crop(rf_all_rast, aoi_file)
rf1 <- project(rf_all_rast, "EPSG:32748")
rf2 <- resample(rf1, aoi_file, method = "bilinear")
rf3 <- syncGeom(input = rf2, ref = aoi_file)
plot(rf3)
