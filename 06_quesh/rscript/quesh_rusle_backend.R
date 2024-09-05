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

# initiate a saga object
saga <- saga_gis(raster_backend = "terra")

# 1. Input Data -----------------------------------------------------------

admin_file <- "C:/Users/fmahezs/Documents/LUMENS/dataset/01_Batas_Administrasi/Admin_kec_V2_Vektor.shp" # boundary map AOI
watershed_file <- "path" # vector layer of watershed boundary
streams_file <- "path" # vector layer of stream network
dem_file <- "C:/Users/fmahezs/Documents/LUMENS/dataset/05_DEM/DEM.tif" # raster file of DEM
precip_file <- "path" # raster file of rainfall

sand_file <- "path" # raster file
silt_file <- "path"
clay_file <- "path"
orgc_file <- "path"

ndvi_dir <- "path" # Listing all files of the time series for NDVI

lc_dir <- "C:/Users/fmahezs/Documents/LUMENS/dataset/02_Tutupan_Lahan/" # landcover directory that consist of time series land cover map

c_ref_file <- "06_quesh/data/lc_c-factor.csv" # csv file contained cover management factor for each landcover class
p_ref_file <- "06_quesh/data/slope_p-factor.csv" # csv file contained P factor values for slope
p_user <- 1

wd <- "path" # define working directory

# 2. Define Parameters ----------------------------------------------------

admin <- vect(admin_file) # define boundary

watershed <- shapefile(watershed_file)
streams <- shapefile(streams_file)

dem <- raster(dem_file)
slope <- terrain(dem, opt = "slope")   # calculate slope from DEM
aspect <- terrain(dem, opt = "aspect") # calculate aspect from slope
hill_shade <- hillShade(slope, aspect) # calculate hill shade

precip <- raster(precip_file)

sand <- rast(sand_file)
silt <- raster(silt_file)
clay <- raster(clay_file)
orgc <- raster(orgc_file)

# 3. Input Parameter Preparation ------------------------------------------

# Prepare R factor - Rainfall Erosivity (Moore, 1979)
r_moore <- calc(x = precip, fun = calculare_r_moore)
plot(r_moore)
summary(r_moore)
hist(r_moore, main = "Histogramm of rainfall erosivity after Moore")

# Prepare R factor - Rainfall Erosivity (GloREDDa - Panagos et al, 2017)
r_gloreda <- get_glored(admin)
plot(r_gloreda)
summary(r_gloreda)
hist(r_gloreda, main = "Histogramm of rainfall erosivity after GloREDa")

# r_layer <- stack(list(Moore = r_moore, GloREDa = r_gloreda)) # create a combined raster stack
# levelplot(r_layer, main = "Rainfall erosivity R")
# r_diff <- calc(r_layer, fun = diff) # calculate the difference between the two R layers
# levelplot(r_diff, margin = FALSE, par.settings = RdBuTheme,
#           main = "Difference GloREDa - Moore")

# Prepare K factor - Soil Erodibility (Williams, 1995)
# NAvalue(sand) <- 255    # define the NA values
# NAvalue(silt) <- 255
# NAvalue(clay) <- 255
# NAvalue(orgc) <- 255

# orgc <- orgc/10

# k_williams <- overlay(sand, silt, clay, orgc, fun = calculate_k_williams)

# Prepare LS factor - Slope length and steepness (Schmidt et al, 2019)
# ls_schmidt <- ls_alpine(dem = dem)
# plot(ls_schmidt)

# Prepare LS factor - LUMENS
ls_calc <- overlay(slope, aspect, fun = calculate_ls)
plot(ls_calc)

# Prepare C factor - Cover Management Using NDVI (Van der Knijff et al, 2000)
# ndvi_files <- list.files(path = ndvi_dir,
#                          pattern = ".tif$",
#                          full.names = TRUE)
# ndvi_timeseries <- stack(ndvi_files) # load all ndvi files into a raster stack
# ndvi_timeseries <- 0.0001 * ndvi_timeseries # scale factor for MODIS NDVI
# 
# mean_ndvi <- calc(ndvi_timeseries, mean, na.rm = TRUE)
# plot(mean_ndvi, main = "Mean rainy season NDVI")
# 
# c_ndvi <- calc(mean_ndvi, fun = calculate_c_knijff) # calculate C based of the mean NDVI
# hist(c_ndvi, main = "Histogramm of cover management factor C")

# Prepare C factor - Cover Management Using Landcover
c_ref <- read.csv(c_ref_file)
lc <- list.files(paste0(lc_dir), full = TRUE, pattern = ".tif$")
# c <- app(lc, calculate_c_lc)
for (i in 1:length(lc)) {
  landcover <- rast(lc[i])
  lc_factor <- as.factor(landcover)
  levels(lc_factor) <- c_ref
  c <- catalyze(lc_factor)
}

c$Landcover <-  NULL
c <- project(c, admin)

# Prepare P factor
p_ref <- read.csv(p_ref_file)

if(p_user == 2){
  slope_perc <- tan(slope * pi / 180) * 100
  labels <- as.character(1:length(breakpoints))
  slope_rec <- cut(slope_perc, breaks = breakpoints, right = TRUE, labels = labels)
  # slope_rec <- classify(slope_perc, c(-Inf, 7, 1, 7, 11.3, 2, 11.3, 17.6, 3, 17.6, 26.8, 4, 26.8, Inf, 5))
  is.factor(slope_rec)
  slope_rec2 <- as.factor(slope_rec)
  is.factor(slope_rec2)
  levels(slope_rec2) <- p_ref
  P <- catalyze(slope_rec2)
} else if(p_user == 3){
  lc_lookup <- lc_legend
  lc_lookup$p_factor <- 0
  lc_lookup$not_to_be_filled <- NA
  lc_lookup_p <- qpcR:::cbind.na(lc_lookup, slope_managed)
  lc_lookup_p <- edit(lc_lookup_p)
  levels(lc) <- lc_lookup_p
  p_class <- deratify(lc, 'p_factor')
  slope_perc <- tan(slope * pi / 180) * 100
  slope_rec <- classify(slope_perc, 
                        c(-Inf, 7, 1, 
                          7, 11.3, 2, 
                          11.3, 17.6, 3, 
                          17.6, 26.8, 4, 
                          26.8, Inf, 5))
  extent(p_class) == extent(slope_rec)
  P <- p_class
  P[p_class == 1 | p_class == 3] <- 1
  P[p_class == 2 & slope_rec == 10] <- 0.5
  P[p_class == 2 & slope_rec == 11] <- 0.75
  P[p_class == 2 & slope_rec == 12] <- 0.9
  P[p_class == 4] <- 0
} else {
  P <- 1
}

if (p_user ==! 1) {
  P <- project(P, admin)
} else {
  P <- 1
}

# 4. Calculate Soil Erosion RUSLE -----------------------------------------

# redefine input parameters
r <- r_gloreda
k <- k_williams
ls <- ls_calc
c <- c_ndvi
p <- 1

a <- r*k*ls*c*p


# Export File -------------------------------------------------------------

writeRaster(hill_shade, "path/hillshade.tif")
writeRaster(r_moore, filename = "path/r_moore.tif", overwrite = TRUE)
writeRaster(k_williams, filename = "path/k_williams.tif", overwrite = TRUE)
writeRaster(mean_ndvi, "path/mean_ndvi.tif", overwrite = TRUE)
writeRaster(c_ndvi, filename = "path/c_ndvi.tif", overwrite = TRUE)



