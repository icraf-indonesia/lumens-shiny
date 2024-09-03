##PUR-PostgreSQL=group
##proj.file=string
##recon_file=vector
##unresolved_table=string
##statusoutput=output table

#=Load library
library(raster)
library(dplyr)
library(sf)
library(terra)

# -------------------------------------------------------------------------
ref_data <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/pur_test/RTRW_V2Fcr_Raster.tif"
recon_file = rgdal::readOGR("D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/pur_test/",layer="PURrec1shp")
unresolved_table="D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/pur_test/unresolved_table.csv"
output_dir="D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/01_pur1/output/"
# -------------------------------------------------------------------------

setwd(output_dir)

ref <- raster(ref_data)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

attribute_dir <- paste(output_dir, "/PUR_attribute.csv", sep="")
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
st_write(sa, paste0(output_dir, "/pur_recon_ciliation.shp"), driver="ESRI Shapefile", append=FALSE)

# save PUR final reconciliation raster file
pur_final_recon_rast <- terra::rasterize(vect(sa), rast(ref), field="PU_ID", res=res(ref)[1], background=NA)
pur_final_recon_rast2 <- terra::rasterize(vect(sa), rast(ref), field="resolved", res=res(ref)[1], background=NA)

# create summary of final reconciliation
test4 <- raster(pur_final_recon_rast)
test4 <- ratify(
  test4,
  filename = paste0(output_dir, '/PUR.grd'),
  count = TRUE,
  overwrite = TRUE
)
summary_PUR <- as.data.frame(levels(test4))
colnames(summary_PUR)[1] <- "PU_ID"
summary_PUR <- merge(summary_PUR, unique_class, by = "PU_ID")

raster_temp <- reclassify(test4, cbind(NA, 255)) # need to set as a dynamic variable
raster_temp_name <- paste0(output_dir, "/raster_temp.tif")
writeRaster(
  raster_temp,
  filename = raster_temp_name,
  format = "GTiff",
  overwrite = TRUE
)

pur_attribute_table <- summary_PUR
colnames(pur_attribute_table) <- c("ID", "COUNT", "Legend")

csv_file<-paste(output_dir,"/csv_planning_unit.csv", sep="")
write.table(csv_file, quote=FALSE, row.names=FALSE, sep=",")

# save RDS for report -----------------------------------------------------
dir.create(path = paste0(output_dir, "/report_files"))
saveRDS(pur_final_recon_rast, paste0(output_dir, "/pur_final_recon_rast.rds"))
saveRDS(summary_PUR, paste0(output_dir, "/summary_PUR.rds"))

# save summary as PUR final lookup table
summary_PUR$COUNT<-NULL
write.table(summary_PUR, "PUR_final_lookup_table.csv", quote=FALSE, row.names=FALSE, sep=",")

# file.copy(paste0(working_directory, "\\template", "\\pur_reconcile_report.qmd"), working_directory, recursive = FALSE, overwrite = TRUE)


#library(quarto)
#quarto_render(paste0(wd_user, "/pur_reconcile_report.qmd"), 
#              output_format = "html")

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"PUR final reconciliation successfully completed!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
