##PUR-PostgreSQL=group
##proj.file=string
##recon_file=vector
##unresolved_table=string
##statusoutput=output table


# -------------------------------------------------------------------------
# proj.file="C:/Users/donyi/Documents/LUMENS/output/testing_pur/testing_pur.lpj"
# recon_file = rgdal::readOGR("C:/Users/donyi/AppData/Local/Temp/processing/6c8dcdddf5254b1095b02a4f97857118",layer="PURrec1shp")
# unresolved_table="c:/users/donyi/appdata/local/temp/tmpcsy7d1.csv"
# -------------------------------------------------------------------------



#=Load library
library(grid)
library(gridExtra)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(rtf)
library(foreign)
library(DBI)
library(RPostgreSQL)
library(rpostgis)
library(stringr)
library(magick)
library(raster)
library(dplyr)
library(sf)
library(terra)


#=Load active project
load(proj.file)

# set driver connection
driver <- dbDriver('PostgreSQL')
project <- as.character(proj_descr[1,2])
DB <- dbConnect(
  driver, dbname=project, host=as.character(pgconf$host), port=as.character(pgconf$port),
  user=as.character(pgconf$user), password=as.character(pgconf$pass)
)

#=Set PUR directory
working_directory<-paste(dirname(proj.file), "/PUR/", idx_PUR, "_PUR_analysis", sep="")
setwd(working_directory)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#=Load reconciliation phase 1 and attribute table
wd_user <- paste(dirname(proj.file), "/PUR/", idx_PUR, "_PUR_analysis", sep = "")
wd_usertemp<-paste(working_directory,"/temp", sep="")

attribute_dir <- paste(working_directory, "/PUR_attribute.csv", sep="")
# get resolved action and merge with attribute table
# remove rows after unresolved case
attribute <- read.table(attribute_dir, header = TRUE, sep = ",")
last_unresolved <- max(which(attribute$Rec_phase1b == "unresolved_case"))
attribute <- attribute[1:last_unresolved, ]

unresolved_edit <- read.table(unresolved_table, header = TRUE, sep = ",")
unresolved_edit.c1 <- as.data.frame(unresolved_edit$ID)
unresolved_edit.c2 <- as.data.frame(unresolved_edit$Reconcile.Action)
colnames(unresolved_edit.c1)[1] <- "ID"
colnames(unresolved_edit.c2)[1] <- "resolved"
unresolved_edit.join <- cbind(unresolved_edit.c1, unresolved_edit.c2)

#left_join(id_rec_ref, by= join_by(ID==NEW_ID)) |> dplyr::select(ID=ID_rec, resolved)

attribute_ori <- read.csv("PUR_dbfinal.csv") |> select(ID  = NEW_ID, Rec_phase1b)

attribute.edit <-
  merge(attribute_ori,
        unresolved_edit.join,
        by = "ID",
        all = TRUE)
test <- as.data.frame(unique(unresolved_edit$Reconcile.Action))
test2 <- as.data.frame(unique(attribute$Rec_phase1b))
colnames(test)[1] <- "add"
colnames(test2)[1] <- "add"
test3 <- rbind(test, test2)
levels(attribute.edit$resolved) <- levels(test3$add)
colnames(attribute.edit)[1] <- "PU_name"

len<-nrow(attribute.edit)

for(s in 1:len){
  if (is.na(attribute.edit$resolved[s])==TRUE) {
    attribute.edit$resolved[s]<-attribute.edit$Rec_phase1b[s]
    attribute.edit$res_id[s]<-attribute.edit$PU_name[s]
  }
}

unique_class <- as.data.frame(unique(attribute.edit$resolved))
colnames(unique_class)[1] <- "resolved"
countrow <- nrow(unique_class)
unique_class$PU_ID <- seq(countrow)
attribute.edit <- merge(attribute.edit, unique_class, by = "resolved")
attribute.edit <-  attribute.edit |> select(ID=PU_name, Rec_phase1b, resolved, res_id, PU_ID)


# save PUR final reconciliation shapefile
sa <- recon_file |> sf::st_as_sf()
sa <- merge(sa, attribute.edit, by = "ID", all = TRUE) 
st_write(sa, paste0(working_directory, "/pur_recon_ciliation.shp"), driver="ESRI Shapefile", append=FALSE)

# save PUR final reconciliation raster file
pur_final_recon_rast <- terra::rasterize(vect(sa), rast(ref), field="PU_ID", res=res(ref)[1], background=NA)
pur_final_recon_rast2 <- terra::rasterize(vect(sa), rast(ref), field="resolved", res=res(ref)[1], background=NA)

# create summary of final reconciliation
test4 <- raster(pur_final_recon_rast)
test4 <- ratify(
    test4,
    filename = paste0(LUMENS_path_user, '/PUR.grd'),
    count = TRUE,
    overwrite = TRUE
  )
summary_PUR <- as.data.frame(levels(test4))
colnames(summary_PUR)[1] <- "PU_ID"
summary_PUR <- merge(summary_PUR, unique_class, by = "PU_ID")

#=Write results to PostgreSQL
idx_pu <- idx_pu + 1
index1 <- idx_pu
description <- paste0("PUR Final Reconciliation ", idx_PUR)

raster_temp <- reclassify(test4, cbind(NA, 255)) # need to set as a dynamic variable
raster_temp_name <- paste0(LUMENS_path_user, "/raster_temp.tif")
writeRaster(
  raster_temp,
  filename = raster_temp_name,
  format = "GTiff",
  overwrite = TRUE
)

pur_attribute_table <- summary_PUR
colnames(pur_attribute_table) <- c("ID", "COUNT", "Legend")
eval(parse(text = (paste("in_pu_lut", idx_pu, "<-pur_attribute_table",  sep = ""))))

eval(parse(text=(paste("list_of_data_pu<-data.frame(RST_DATA='in_pu", idx_pu,"', RST_NAME='", description, "', LUT_NAME='in_pu_", "lut", idx_pu, "', row.names=NULL)", sep=""))))

InPuLUT_i <- paste("in_pu_lut", idx_pu, sep="")
InPu_i <- paste("in_pu", idx_pu, sep="")

#append list
dbWriteTable(DB, "list_of_data_pu", list_of_data_pu, append=TRUE, row.names=FALSE)
dbWriteTable(DB, InPuLUT_i, eval(parse(text=(paste(InPuLUT_i, sep="" )))), append=TRUE, row.names=FALSE)

#write to csv
list_of_data_pu<-dbReadTable(DB, c("public", "list_of_data_pu"))
csv_file<-paste(LUMENS_path_user,"/csv_planning_unit.csv", sep="")
write.table(list_of_data_pu, csv_file, quote=FALSE, row.names=FALSE, sep=",")

addRasterToPG(project, raster_temp_name, InPu_i, srid)

resave(idx_pu, file=proj.file)

# save RDS for report -----------------------------------------------------
dir.create(path = paste0(wd_user, "/report_files"))
saveRDS(pur_final_recon_rast, paste0(LUMENS_path_user, "/pur_final_recon_rast.rds"))
saveRDS(summary_PUR, paste0(LUMENS_path_user, "/summary_PUR.rds"))

# save summary as PUR final lookup table
summary_PUR$COUNT<-NULL
write.table(summary_PUR, "PUR_final_lookup_table.csv", quote=FALSE, row.names=FALSE, sep=",")

file.copy(paste0(LUMENS_path, "\\template", "\\pur_reconcile_report.qmd"), wd_user, recursive = FALSE, overwrite = TRUE)


library(quarto)
quarto_render(paste0(wd_user, "/pur_reconcile_report.qmd"), 
              output_format = "html")

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"PUR final reconciliation successfully completed!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
