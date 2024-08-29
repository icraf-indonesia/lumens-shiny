# Planning Unit Reconciliation (PUR Reconcile) #2 Script

# 0. Load functions and libraries -------------------------------

# Load custom functions
source("01_pur1/rscript/functions_pur.R")

# Check and install required packages
required_packages <- c(
  "raster", "terra", "dplyr", "sf", "readxl"
)

check_and_install_packages(required_packages)

# 1. Define input parameters ------------------------------------

# Define file path and parameters
path <- list(
  recon_file = "01_pur1/output/PUR_first_phase_result.shp",
  unresolved_table = "01_pur1/output/PUR_unresolved_case.xlsx",
  map_resolution = 100
)

output_dir = "02_pur2/output/"

# 2. Data preparation -------------------------------------------

# Load shapefile
pur_sa <- path$recon_file %>% st_read %>% st_as_sf() %>% st_drop_geometry()
sa <- path$recon_file %>% st_read()

# Define attribute ori from shp
attribute_ori <- pur_sa %>% st_drop_geometry() %>% 
  rename(Rec_phase1b = Rec_phase2 ) %>% 
  select(ID, Rec_phase1b)

# Define attribute from shp
pur_attribute_top <- pur_sa %>% rename(Rec_phase1b = Rec_phase2 ) %>% 
  select(ID_rec, Rec_phase1b) %>% 
  filter(!Rec_phase1b %in% "unresolved_case") %>% 
  distinct(Rec_phase1b) %>% 
  tibble::rownames_to_column("ID") %>%
  mutate(ID = as.numeric(ID))

pur_attribute_mid <- pur_sa %>%
  rename(Rec_phase1b = Rec_phase2 ) %>% 
  select( ID, Rec_phase1b) %>% 
  filter(Rec_phase1b %in% "unresolved_case") %>% 
  arrange(ID)

pur_attribute_top %>% bind_rows(pur_attribute_mid)
attribute <- pur_attribute_top

# Load unresolved cases and join with attribute table
unresolved_edit <- readxl::read_xlsx(path$unresolved_table) # read xlsx
unresolved_edit.c1 <- as.data.frame(unresolved_edit[["ID"]]) # define data frame ID field
unresolved_edit.c2 <- as.data.frame(unresolved_edit[["Reconcile Action"]]) # define data frame Reconcile Action field
colnames(unresolved_edit.c1)[1] <- "ID"
colnames(unresolved_edit.c2)[1] <- "resolved"
unresolved_edit.join <- cbind(unresolved_edit.c1, unresolved_edit.c2)

# Load original attribute data and merge with unresolved cases
attribute.edit <- merge(attribute_ori, unresolved_edit.join, by = "ID", all = TRUE)

# 3. Resolve any unresolved cases --------------------------------
test <- as.data.frame(unique(unresolved_edit[["Reconcile Action"]]))
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
attribute.edit <- attribute.edit |> select(ID = PU_name, resolved, PU_ID)

# 5. Save final reconcilitation data ----------------------------

# Save final reconciliation shapefile
sa0 <- sa %>% select(-Referenc_1)
sa <- merge(sa0, attribute.edit, by = "ID", all = TRUE)
st_write(sa, paste0(output_dir, "/PUR_reconciliation_result.shp"), driver = "ESRI Shapefile", append = FALSE)

# Save final reconciliation raster file
ref0 <- sa %>% select(ID, REFERENCE)
ref <- rasterise_multipolygon(sf_object = ref0, raster_res = c(path$map_resolution,path$map_resolution), field = "ID")

pur_final_recon_rast <- terra::rasterize(vect(sa), rast(ref), field = "PU_ID", res = res(ref)[1], background = NA)
pur_final_recon_rast2 <- terra::rasterize(vect(sa), rast(ref), field = "resolved", res = res(ref)[1], background = NA)

# Create summary of final reconciliation
test4 <- raster(pur_final_recon_rast)
test4 <- ratify(test4, filename = paste0(output_dir, '/PUR.grd'), count = TRUE, overwrite = TRUE)
summary_PUR <- as.data.frame(levels(test4))
colnames(summary_PUR)[1] <- "PU_ID"
summary_PUR <- merge(summary_PUR, unique_class, by = "PU_ID")

# Reclassify and save the raster
raster_temp <- reclassify(test4, cbind(255, NA))
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