# input parameter
ref_data <-"C:/Users/fmahezs/Documents/LUMENS/dataset/04_RTRW/RTRW_V2Fcr_Raster.tif"
lut_ref <- "C:/Users/fmahezs/Documents/LUMENS/dataset/00_Data_Tabular/RTRW_F.csv"
ref_class <-"D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/01_pur1/ref_class.csv"
ref_mapping <-"D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/01_pur1/ref_mapping.csv"
pu_units <-"D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/01_pur1/pu_units.csv"
output_dir <-"C:/Users/fmahezs/Documents/LUMENS/lumens_ui/output/try_pur/PUR/"

# load library
library(foreign)
library(grid)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(raster)
library(terra)
library(dplyr)
library(sp)
library(leaflet)

time_start <- Sys.time () 

# set the working directory
wd_user <- output_dir
dir.create(wd_user, recursive = TRUE, mode = "0777")
setwd(wd_user)

# prepare reference data
ref <- raster(ref_data)
ref <- reclassify(ref, cbind(255, 0))

lookup_ref <- read.csv(lut_ref)
colnames(lookup_ref)[ncol(lookup_ref)] <- "REFERENCE"
ref.name <- names(ref) 

# projection handling -----------------------------------------------------

if (grepl("+units=m", as.character(crs(ref)@projargs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(crs(ref)@projargs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  statuscode<-0
  statusmessage<-"Raster map projection is unknown"
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

# merge reference data with the reference class
tabel_acuan <- read.table(ref_class, header = FALSE, sep = ",") %>%
  setNames(c("acuan_kelas", "acuan_kode"))

tabel_mapping <- read.table(ref_mapping, header = FALSE, sep = ",") %>%
  setNames(c("REFERENCE", "IDS")) %>%
  left_join(lookup_ref, by = "REFERENCE")

if("COUNT" %in% colnames(tabel_mapping)){
  tabel_mapping <- tabel_mapping %>%
    select(-COUNT) 
}

tabel_mapping <- tabel_mapping %>%
  rename(IDO = ID)

# save reference table and map to temporary folder
target_file <- paste(wd_user,"/reference.csv", sep="")
write.table(tabel_mapping, target_file, quote=FALSE, row.names=FALSE, sep=",")

# prepare planning units
pu_list <- read.table(pu_units, header=FALSE, sep=",")
n_pu_list <- nrow(pu_list)

# create an empty list to store the results
pu_lut_list <- list()
cmd <- paste()
command1 <- paste()
central_attr<-NULL

# iterate over each planning unit in the list
for (i in 1:n_pu_list) {
  # Set planning unit parameters based on the current iteration
  data_name <- as.character(pu_list[i, 1])  # Extract data name from the list
  pu_data <- as.character(pu_list[i, 1])    # Extract planning unit data
  Type <- as.character(pu_list[i, 4])       # Extract type information

  # Read the lookup table from the local file system
  lut_table <- pu_list[i, 5]
  
  # Get the raster data from a local file (assuming `pu_data` points to a file path)
  pu_raster <- raster(file.path(dirname(lut_table), paste0(pu_data, ".tif")))
  
  # Append the lookup table data to a list
  pu_lut_list[[i]] <- lut_table
  
  # Append 'data_name' to the 'central_attr' list
  central_attr <- append(central_attr, data_name)
  
  # Reclassify NA and 255 values in the raster data
  pu_raster[is.na(pu_raster)] <- 0  # Set NA values to 0
  pu_raster <- reclassify(pu_raster, cbind(255, 0))  # Reclassify 255 values to 0
  
  # Rename the raster to 'data_name'
  names(pu_raster) <- data_name
  
  # Perform calculations involving 'pu_raster' and store in 'R' variables
  j <- n_pu_list + 1 - i
  assign(paste0("R", i), pu_raster * (100^(j)))
  
  # Build a command for further calculations
  cmd <- paste0(cmd, "R", i, "+")
  
  # Construct 'command1' string for use in further processing
  if (i != n_pu_list) {
    command1 <- paste0(command1, pu_data, ",")
  } else {
    command1 <- paste0(command1, pu_data)
  }
}

# calculate a reference value and store in an 'R' variable
ref.number <- n_pu_list + 1
eval(parse(text=(paste("R", ref.number, "<-ref*1", sep=""))))

# Update the 'cmd' string with the reference value
cmd <- paste(cmd, "R", ref.number, sep="")

# Combine reference and planning units
# stacking planning unit
command1 <- paste(command1, ",ref", sep="") 
eval(parse(text=(paste("PUR_stack <- stack(", command1, ")", sep="")))) 

# create raster attribute table from combined planning unit
eval(parse(text=(paste("PUR<-", cmd, sep=""))))
PUR <- ratify(PUR, count=TRUE)
PUR_db<-levels(PUR)[[1]]

# reclassify attribute ID
ORI_ID<-PUR_db$ID
NEW_ID<-seq(nrow(PUR_db)) 
rclmat<-cbind(as.matrix(ORI_ID), as.matrix(NEW_ID)) 
PUR<-reclassify(PUR, rclmat) 
PUR<-ratify(PUR, count=TRUE)

# extract all ids
PUR_db$NEW_ID<-NEW_ID
PUR_db$TEMP_ID<-PUR_db[,1]
k<-0





































