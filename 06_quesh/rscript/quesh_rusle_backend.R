### RUSLE Model for QuES-H Module ###

# 0. Load Library ---------------------------------------------------------

# call the rusle functions
source("06_quesh/rscript/quesh_functions.R")

# load library
library(raster)
library(rasterVis)
library(rusleR)
library(Rsagacmd)
library(terra)
library(magrittr)
library(dplyr)
library(maps)

# initiate a saga object
# saga <- saga_gis(raster_backend = "terra")

# 1. Input Data -----------------------------------------------------------

# aoi_file <- "data/data_quesh/Base_map.tif" # boundary map AOI
dem_file <- "data/data_quesh/SRTM_Bungo.tif" # raster file of DEM
rainfall_file <- "data/data_quesh/climate/prec_monthly_1970-2000_wc2.1/prec_bungo/" # raster file of rainfall

sand_file <- "data/raster/soil/bungo_sand_0-5cm_mean.tif" # raster file
silt_file <- "data/raster/soil/bungo_silt_0-5cm_mean.tif" 
clay_file <- "data/raster/soil/bungo_clay_0-5cm_mean.tif" 
orgc_file <- "data/raster/soil/bungo_soc_0-5cm_mean.tif" 

lc_dir <- "data/raster/tutupan_lahan_Bungo_2010r.tif" # landcover directory that consist of time series land cover map

c_ref_file <- "data/data_quesh/c_factor_bungo_usda1972.csv" # csv file contained cover management factor for each landcover class
p_ref_file <- "06_quesh/data/slope_p-factor.csv" # csv file contained P factor values for slope
p_user <- 1

wd <- "data/data_quesh/" # define working directory

# 2. Define Parameters ----------------------------------------------------

# aoi <- vect(aoi_file) # define boundary

dem <- rast(dem_file)
slope <- terrain(dem, v = "slope", unit="radians")   # calculate slope from DEM
aspect <- terrain(dem, v = "aspect", unit="radians") # calculate aspect from slope
# hill_shade <- hillShade(slope = slope, aspect = aspect) # calculate hill shade

# define rainfall parameter
rainfall <-
  list.files(path = paste0(rainfall_file),
             pattern = ".tif",
             full.names = TRUE) 
rainfall %>% rast()
rainfall_annual <-sum(rast(rainfall)) #average of 12 months rainfall

writeRaster(rainfall_annual, "data/data_quesh/rainfall_annual_bungo_wc2.1.tif")
# define soil properties parameters
sand <- rast(sand_file)
silt <- rast(silt_file)
clay <- rast(clay_file)
orgc <- rast(orgc_file)

# 3. R - Rainfall Erosivity Data Preparation ------------------------------------------

# Prepare R factor - Rainfall Erosivity (Moore, 1979)
r_moore <- calculate_r_moore(p = rainfall_annual)

plot(r_moore)
summary(r_moore)
hist(r_moore, main = "Histogramm of rainfall erosivity after Moore")

writeRaster(r_moore, "data/data_quesh/r_factor_bungo.tif", overwrite = TRUE)
# Prepare R factor - Rainfall Erosivity (GloREDDa - Panagos et al, 2017)
# r_gloreda <- get_glored(aoi)
# plot(r_gloreda)
# summary(r_gloreda)
# hist(r_gloreda, main = "Histogramm of rainfall erosivity after GloREDa")
# 
# r_layer <- stack(list(Moore = r_moore, GloREDa = r_gloreda)) # create a combined raster stack
# levelplot(r_layer, main = "Rainfall erosivity R")
# r_diff <- calc(r_layer, fun = diff) # calculate the difference between the two R layers
# levelplot(r_diff, margin = FALSE, par.settings = RdBuTheme,
#           main = "Difference GloREDa - Moore")


# 4. K - Soil Erodibility Data Preparation --------------------------------

# Prepare K factor - Soil Erodibility (Williams, 1995)
k_williams <- calculate_k_williams(
  sndprc = sand, 
  sltprc = silt, 
  clyprc = clay, 
  orcprc = orgc
)

plot(k_williams)

writeRaster(k_williams, "data/data_quesh/k_factor_bungo.tif", overwrite = TRUE)
# 5. LS - Length & Steepnes Data Preparation ------------------------------

# Prepare LS factor - Slope length and steepness (Schmidt et al, 2019)
# ls_schmidt <- ls_alpine(dem = dem)
# plot(ls_schmidt)

# Prepare LS factor - LUMENS
# ls_calc <- calculate_ls(
#   slope = slope,
#   aspect = aspect
# )
ls1 <- (1 + (sin(slope * pi / 180) / 0.0896)^1.3)
ls2 <- ((sin((aspect - 180) * pi / 180) + 0.5) / 1.5)^0.6
ls_calc <- ls1 * ls2

plot(ls_calc)

writeRaster(ls_calc, "data/data_quesh/ls_factor_bungo.tif", overwrite = TRUE)
# 6. C - Cover Management Data Preparation --------------------------------

# Prepare C factor - Cover Management Using Landcover
c_ref1 <- read.csv(c_ref_file)
c_ref <- c_ref1[-2]
# lc <- list.files(paste0(lc_dir), full = TRUE, pattern = ".tif$")
# c <- app(lc, calculate_c_lc)

landcover1 <- rast(lc_dir)

landcover <- syncGeom(input = landcover1, ref = aoi_file)

lc_factor <- as.factor(landcover)
levels(landcover) <- c_ref
c_usda <- landcover
# c_usda <- catalyze(lc_factor)

plot(c_usda)

writeRaster(c_usda, "data/data_quesh/c_factor_bungo.tif", overwrite = TRUE)
# for (i in 1:length(lc)) {
#   landcover <- rast(lc[i])
#   lc_factor <- as.factor(landcover)
#   levels(lc_factor) <- c_ref
#   c <- catalyze(lc_factor)
# }
# 
# c$Landcover <-  NULL
# c <- project(c, aoi)

# 7. P - Practice Factor Data Preparation ---------------------------------

# Prepare P factor
# p_ref <- read.csv(p_ref_file)
# 
# if(p_user == 2){
#   slope_perc <- tan(slope * pi / 180) * 100
#   labels <- as.character(1:length(breakpoints))
#   slope_rec <- cut(slope_perc, breaks = breakpoints, right = TRUE, labels = labels)
#   # slope_rec <- classify(slope_perc, c(-Inf, 7, 1, 7, 11.3, 2, 11.3, 17.6, 3, 17.6, 26.8, 4, 26.8, Inf, 5))
#   is.factor(slope_rec)
#   slope_rec2 <- as.factor(slope_rec)
#   is.factor(slope_rec2)
#   levels(slope_rec2) <- p_ref
#   P <- catalyze(slope_rec2)
# } else if(p_user == 3){
#   lc_lookup <- lc_legend
#   lc_lookup$p_factor <- 0
#   lc_lookup$not_to_be_filled <- NA
#   lc_lookup_p <- qpcR:::cbind.na(lc_lookup, slope_managed)
#   lc_lookup_p <- edit(lc_lookup_p)
#   levels(lc) <- lc_lookup_p
#   p_class <- deratify(lc, 'p_factor')
#   slope_perc <- tan(slope * pi / 180) * 100
#   slope_rec <- classify(slope_perc, 
#                         c(-Inf, 7, 1, 
#                           7, 11.3, 2, 
#                           11.3, 17.6, 3, 
#                           17.6, 26.8, 4, 
#                           26.8, Inf, 5))
#   extent(p_class) == extent(slope_rec)
#   P <- p_class
#   P[p_class == 1 | p_class == 3] <- 1
#   P[p_class == 2 & slope_rec == 10] <- 0.5
#   P[p_class == 2 & slope_rec == 11] <- 0.75
#   P[p_class == 2 & slope_rec == 12] <- 0.9
#   P[p_class == 4] <- 0
# } else {
#   P <- 1
# }
# 
# if (p_user ==! 1) {
#   P <- project(P, aoi)
# } else {
#   P <- 1
# }

# 8. Calculate Soil Erosion RUSLE -----------------------------------------

# redefine input parameters
r <- r_moore
k <- k_williams
ls <- ls_calc
c <- lc_factor
p <- 1

a <- r*k*ls*c*p
plot(a)

writeRaster(a, filename = "data/data_quesh/erosion_estimate.tif")
