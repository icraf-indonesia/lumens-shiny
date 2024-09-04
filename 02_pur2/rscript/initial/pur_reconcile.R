# Planning Unit Reconciliation (PUR Reconcile) #2 Script

# 0. Load functions and libraries -------------------------------
tryCatch({
  
# Load custom functions
source("01_pur1/rscript/functions_pur.R")

# Check and install required packages
required_packages <- c(
  "raster", "terra", "dplyr", "sf", "readxl", "ggplot2", "knitr", "kableExtra", "rmarkdown"
)
  
check_and_install_packages(required_packages)

# Start running PUR
start_time <- Sys.time()

# 1. Define input parameters ------------------------------------
area_name <- 'Bungo' 

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

ref <- rasterise_multipolygon(sf_object = sa, raster_res = c(path$map_resolution,path$map_resolution), field = "ID")

# Check and handle the projection of the reference data
if (grepl("+units=m", as.character(st_crs(ref)$proj4string))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, PUR will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(st_crs(ref)$proj4string))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, PUR will automatically generate data in Ha unit")
} else{
  statuscode<-0
  statusmessage<-"Raster map projection is unknown"
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

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
colnames(summary_PUR)[2] <- "Ha"
colnames(summary_PUR)[3] <- "Final Resolved Area"
write.table(summary_PUR, paste0(output_dir, "PUR_final_lookup_table.csv"), quote = FALSE, row.names = FALSE, sep = ",")

# End of the script
end_time <- Sys.time()
cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")

# 6. Prepare parameters for report -------------------------
report_params <- list(
  start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
  end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
  output_dir = output_dir,
  raster_temp = raster_temp,
  summary_PUR = summary_PUR,
  area_name = area_name,
  sa = sa,
  dir_raster_temp = paste0(output_dir, "/PUR_reconciliation_result.tif"),
  dir_sa = paste0(output_dir, "/PUR_reconciliation_result.shp"),
  dir_summary_PUR = paste0(output_dir, "/PUR_final_lookup_table.csv")
)

# Prepare summary data for the report
summary_data <- list(
  total_area = sum(pur_attribute_table$COUNT) * Spat_res,
  resolved_area = sum(pur_attribute_table$COUNT[attribute_ori$Rec_phase1b != "unresolved_case"]) * Spat_res,
  unresolved_area = sum(pur_attribute_table$COUNT[attribute_ori$Rec_phase1b == "unresolved_case"]) * Spat_res,
  resolved_percentage = (sum(pur_attribute_table$COUNT[attribute_ori$Rec_phase1b != "unresolved_case"]) / sum(pur_attribute_table$COUNT)) * 100,
  unresolved_percentage = (sum(pur_attribute_table$COUNT[attribute_ori$Rec_phase1b == "unresolved_case"]) / sum(pur_attribute_table$COUNT)) * 100
)

report_params$summary_data <- summary_data

# Render the R markdown report
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
}

rmarkdown::render(
  input = "02_pur2/report_template/PUR2_report.Rmd",
  output_file = "PUR_reconcile_report.html",
  output_dir = output_dir,
  params = report_params
)

}, error = function(e) {
  cat("An error occurred:\n")
  print(e)
}, finally = {
  cat("Script execution completed.\n")
  
})