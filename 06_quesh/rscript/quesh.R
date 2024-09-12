# QUES-H RUSLE Script


# 0. Load functions and libraries -----------------------------------------
tryCatch({

# Load custom functions
source("06_quesh/rscript/quesh_functions.R")

required_packages <- c(
  "raster", "terra", "magrittr", "dplyr", "lattice", "rasterVis"
)

check_and_install_packages(required_packages)

# Start running QUES-H
start_time <- Sys.time()

# 1. Define input parameters ----------------------------------------------

# Define file path and parameters
path <- list(
  pu_file = "data/raster/Zona_Bungo.tif", # boundary map AOI
  dem_file = "data/data_quesh/SRTM_Bungo.tif", # raster file of DEM
  rainfall_file = "data/data_quesh/rainfall_annual_bungo_wc2.1.tif", # raster file of rainfall
  sand_file = "data/raster/soil/bungo_sand_0-5cm_mean.tif", # raster file
  silt_file = "data/raster/soil/bungo_silt_0-5cm_mean.tif", 
  clay_file = "data/raster/soil/bungo_clay_0-5cm_mean.tif", 
  orgc_file = "data/raster/soil/bungo_soc_0-5cm_mean.tif",
  lc_dir = "data/raster/tutupan_lahan_Bungo_2010r.tif", # landcover directory that consist of time series land cover map
  c_ref_file = "data/data_quesh/c_factor_bungo_usda1972_clip.csv", # csv file contained cover management factor for each landcover class
  # p_ref_file = "06_quesh/data/",
  raster.nodata = 0
)

output_dir = "06_quesh/output/"

# 2. R - Rainfall erosivity preparation -----------------------------------------------------

# Define R factor data
rainfall_annual <- syncGeom(input = path$rainfall_file, ref = path$pu_file)

# Calculate R factor - Rainfall Erosivity (Moore, 1979)
r_factor <- calculate_r_moore(p = rainfall_annual)

writeRaster(r_factor, paste0(output_dir, "r_factor.tif"), overwrite = TRUE)

# 3. K - Soil erodibility preparation -------------------------------------

# Define K factor data
sand <- syncGeom(input = path$sand_file, ref = path$pu_file)
silt <- syncGeom(input = path$silt_file, ref = path$pu_file)
clay <- syncGeom(input = path$clay_file, ref = path$pu_file)
orgc <- syncGeom(input = path$orgc_file, ref = path$pu_file)

# Calculate K factor - Soil Erodibility (Williams, 1995)
k_factor <- calculate_k_williams(
  sndprc = sand, 
  sltprc = silt, 
  clyprc = clay, 
  orcprc = orgc
)

writeRaster(k_factor, paste0(output_dir, "k_factor.tif"), overwrite = TRUE)

# 4. LS - Length and steepnes preparation --------------------------------

# Define LS factor data
dem <- syncGeom(input = path$dem_file, ref = path$pu_file)
slope_deg <- terrain(dem, v = "slope", unit="degree")  
slope_rad <- terrain(dem, v = "slope", unit="radians") 
aspect <- terrain(dem, v = "aspect", unit="radians") 
flow_acc <- terrain(dem, v = "flowdir")

# Calculate LS factor by Previous LUMENS Script
# ls_factor <- calculate_ls(
#   slope = slope_rad,
#   aspect = aspect
# )

# Calculate LS factor by Moore & Burch (1986) - BRIN 
ls_factor <- calculate_ls_moore(
  dem = dem,
  slope = slope_rad,
  flow_acc = flow_acc
)

# Calculate LS Factor by Moore and Burch (1986) and Moore and Wilson (1992)
# m <- 0.4  # value can range between 0.4 and 0.6
# n <- 1.3  # value can range between 1.22 and 1.3
# 
# ls_factor <- ((flow_acc / 22.13)^m) * ((sin(slope) / 0.0896)^n) # use radians slope

# # Calculate LS factor by Wischmeier and Smith (1978)
# # Function to classify 'm' based on the slope angle (β)
# calculate_m <- function(slope_val) {
#   if (is.na(slope_val)) {
#     return(NA)
#   } else if (slope_val > 0.05) {
#     return(0.5)
#   } else if (slope_val > 0.03) {
#     return(0.4)
#   } else if (slope_val > 0.01) {
#     return(0.3)
#   } else {
#     return(0.2)
#   }
# }
# 
# m <- app(slope, fun = function(x) sapply(x, calculate_m))
# ls_factor <- ((flow_acc / 22.13)^m) * (65.4 * (sin(slope)^2) + 4.5 * sin(slope) + 0.0654) # use radians slope

writeRaster(ls_factor, paste0(output_dir, "ls_factor.tif"), overwrite = TRUE)

# 5. C - Cover management preparation -------------------------------------

# Define C factor data
c_ref <- read.csv(path$c_ref_file)
# c_factor_data <- read.csv(path$c_ref_file)
landcover <- syncGeom(input = path$lc_dir, ref = path$pu_file)

# landcover[landcover == path$raster.nodata] <- NA
c_factor_lookup <- setNames(c_ref$C_factor, c_ref$ID)

# Define a function to replace landcover IDs with their corresponding C factor values
replace_with_c_factor <- function(x) {
  c_factor_lookup[as.character(x)] # Direct lookup without NA handling
}
# Apply the replacement function to the landcover raster
c_factor_raster <- app(landcover, replace_with_c_factor)

# c_factor_lookup <- setNames(c_ref$C_factor, c_ref$ID)
# 
# replace_with_c_factor <- function(x) {
#   ifelse(is.na(x), NA, c_factor_lookup[as.character(x)])
# }
# 
# c_factor_raster <- app(landcover, replace_with_c_factor)

landcover_c <- landcover
c_ref2 <- as.matrix(c_ref[,1])
c_ref3 <- as.matrix(c_ref[,3])
c_ref4 <- cbind(c_ref2, c_ref3)
c_ref4 <- rbind(c_ref4, c(0, NA))
c_factor <- classify(landcover_c, c_ref4)

# Calculate C factor
# calculate_c_lc(
#   landcover = landcover,
#   c_ref = c_ref
# )

writeRaster(c_factor_raster2, paste0(output_dir, "c_factor2.tif"), overwrite = TRUE)


# 6. P - Practice factor preparation --------------------------------------

slope_pct <- tan(slope_deg * pi / 180) * 100

# There are 3 option for practice factor according to Shin (1999)
# You can choose the applied practice: Contouring; Strip Cropping; Terracing
# change the parameter by the following order p_user <- c([contouring], [strip cropping], [terracing])
# The value of 1 means the corresponding practice applied and 0 means not applied
# If the value all 0, it means the P factor will be define as 1 (no practice applied)

p_user <- c(1, 0, 1) # change the value with 0 or 1 by this order: c([contouring], [strip cropping], [terracing])

p_factor <- calculate_p_shin(
  slope_pct = slope_pct, 
  p_user = p_user
)

writeRaster(p_factor, paste0(output_dir, "p_factor.tif"), overwrite = TRUE)

# 7. Calculate soil erosion RUSLE ----------------------------------------------------

# Redefine input parameters

r <- r_factor
k <- k_factor
ls <- ls_factor
c <- c_factor
p <- p_factor

# Calculate RUSLE
a <- r*k*ls*c*p

# Visulalize erosion
levelplot(a, margin = FALSE, main = "Mean Annual Erosion")
# hist(a, xlim = c(0, 10000), main = "Histogramm of mean annual erosion (arbitary input layers)")

writeRaster(a, filename = paste0(output_dir, "soil_erosion.tif"), overwrite = TRUE)


  
})
