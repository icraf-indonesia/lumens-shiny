# Planning Unit Reconciliation (PUR) #1 Script

# 0. Load functions and libraries -------------------------------

# Load custom functions
source("01_pur1/rscript/functions_pur.R")

# Check and install required packages
required_packages <- c(
  "raster", "terra", "dplyr", "sf"
)

check_and_install_packages(required_packages)

# 1. Define input parameters ------------------------------------

# Define file path and parameters
path <- list(
  ref_map = "data/pur_test/vector/RTRW_F.shp",
  recon_file = rgdal::readOGR("01_pur1/output/",layer="PUR_first_phase_result"),
  unresolved_table = "data/pur_test/tabular/unresolved_table_konsesi_piaps_peat.csv",
  attribute_dir = "01_pur1/output/PUR_attribute.csv",
  attribute_db = "01_pur1/output/PUR_dbfinal.csv",
  map_resolution = 100
)

output_dir = "02_pur2/output/"

# 2. Data preparation -------------------------------------------

# Prepare reference data
ref_data <- st_read(path$ref_map)
ref <- rasterise_multipolygon(sf_object = ref_data, raster_res = c(path$map_resolution,path$map_resolution), field = "ID")

# Load and process attribute table
attribute <- read.table(path$attribute_dir, header = TRUE, sep = ",")
last_unresolved <- max(which(attribute$Rec_phase1b == "unresolved_case"))
attribute <- attribute[1:last_unresolved, ]

# Load unresolved cases and join with attribute table
unresolved_edit <- read.table(path$unresolved_table, header = TRUE, sep = ",")
unresolved_edit.c1 <- as.data.frame(unresolved_edit$ID)
unresolved_edit.c2 <- as.data.frame(unresolved_edit$Reconcile.Action)
colnames(unresolved_edit.c1)[1] <- "ID"
colnames(unresolved_edit.c2)[1] <- "resolved"
unresolved_edit.join <- cbind(unresolved_edit.c1, unresolved_edit.c2)

# Load original attribute data and merge with unresolved cases
attribute_ori <- read.csv(path$attribute_db) |> select(ID = NEW_ID, Rec_phase1b)
attribute.edit <- merge(attribute_ori, unresolved_edit.join, by = "ID", all = TRUE)

# 3. Resolve any unresolved cases --------------------------------
test <- as.data.frame(unique(unresolved_edit$Reconcile.Action))
test2 <- as.data.frame(unique(attribute$Rec_phase1b))
colnames(test)[1] <- "add"
colnames(test2)[1] <- "add"
test3 <- rbind(test, test2)
levels(attribute.edit$resolved) <- levels(test3$add)
colnames(attribute.edit)[1] <- "PU_name"

len <- nrow(attribute.edit)
for (s in 1:len) {
  if (is.na(attribute.edit$resolved[s])) {
    attribute.edit$resolved[s] <- attribute.edit$Rec_phase1b[s]
    attribute.edit$res_id[s] <- attribute.edit$PU_name[s]
  }
}

# 4. Create Unique Class IDs for Resolved Cases -----------------
unique_class <- as.data.frame(unique(attribute.edit$resolved))
colnames(unique_class)[1] <- "resolved"
countrow <- nrow(unique_class)
unique_class$PU_ID <- seq(countrow)
attribute.edit <- merge(attribute.edit, unique_class, by = "resolved")
attribute.edit <- attribute.edit |> select(ID = PU_name, Rec_phase1b, resolved, res_id, PU_ID)

# 5. Save final reconcilitation data ----------------------------

# Save final reconciliation shapefile
sa <- path$recon_file |> sf::st_as_sf()
sa <- merge(sa, attribute.edit, by = "ID", all = TRUE)
st_write(sa, paste0(output_dir, "/PUR_reconciliation_result.shp"), driver = "ESRI Shapefile", append = FALSE)

plot(sa)

# Save final reconciliation raster file
pur_final_recon_rast <- terra::rasterize(vect(sa), rast(ref), field = "PU_ID", res = res(ref)[1], background = NA)
pur_final_recon_rast2 <- terra::rasterize(vect(sa), rast(ref), field = "resolved", res = res(ref)[1], background = NA)

plot(pur_final_recon_rast2)

# Create summary of final reconciliation
test4 <- raster(pur_final_recon_rast)
test4 <- ratify(test4, filename = paste0(output_dir, '/PUR.grd'), count = TRUE, overwrite = TRUE)
summary_PUR <- as.data.frame(levels(test4))
colnames(summary_PUR)[1] <- "PU_ID"
summary_PUR <- merge(summary_PUR, unique_class, by = "PU_ID")

# Reclassify and save the raster
raster_temp <- reclassify(test4, cbind(NA, 255))
raster_temp_name <- paste0(output_dir, "/PUR_reconciliation_result.tif")
writeRaster(raster_temp, filename = raster_temp_name, format = "GTiff", overwrite = TRUE)

# Save attribute table
pur_attribute_table <- summary_PUR
colnames(pur_attribute_table) <- c("ID", "COUNT", "Legend")
csv_file <- paste0(output_dir, "/csv_planning_unit.csv")
write.table(pur_attribute_table, file = csv_file, quote = FALSE, row.names = FALSE, sep = ",")

# Save RDS for report
# dir.create(path = paste0(output_dir, "/report_files"))
saveRDS(pur_final_recon_rast, paste0(output_dir, "/PUR_final_recon_rast.rds"))
saveRDS(summary_PUR, paste0(output_dir, "/summary_PUR.rds"))

# Save summary as PUR final lookup table
summary_PUR$COUNT <- NULL
write.table(summary_PUR, paste0(output_dir, "PUR_final_lookup_table.csv"), quote = FALSE, row.names = FALSE, sep = ",")

