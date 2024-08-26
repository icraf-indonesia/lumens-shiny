# loading required packages====
library(dplyr)
library(terra)
library(sf)
library(DBI)
library(RSQLite)

source("05_quesb//rscript//ques_biodiv_functions.r")

# Specify path to input variables
lc_t1_path <- "data/quesb_test/lc_2005.tif"
t1 <- 2005
lulc_lut_path <- "data/quesb_test/habitat_lookup.csv"
contab_path <- "data/quesb_test/contrast_euc.fsq"
output_dir <- "data/quesb_test/"

# Load lookup tables
lulc_lut <- read.csv(lulc_lut_path)
raster.nodata <- 0

# Load land cover maps
lc_t1 <- prepare_lc_data(lc_t1_path,
                         year = t1,
                         lookup_table = lulc_lut)

NAflag(lc_t1) <- raster.nodata

teci_lc_t1 <- teci_analysis(landuse = lc_t1,
              output_dir = output_dir,
              classdesc = "data/quesb_test/descriptors.fcd",
              cont_fsq = "data/quesb_test/contrast_euc.fsq",
              fca = "data/quesb_test/teciuf.fca",
              adjacent_only = 1,
              gridres = 10000,
              windowsize = 1000,
              window.shape = 1,
              raster.nodata = 0,
              fragstats_path = "C:/Program Files/Fragstats 4.3/")

# generate a sampling grid polygon
