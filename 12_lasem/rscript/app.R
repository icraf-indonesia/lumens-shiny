# List of required packages for LaSEM
required_packages <- c(
  "terra",
  "dplyr",
  "sf",
  "purrr",
  "tidyr",
  "rlang",
  "tibble",
  "stringr",
  "readr",
  "magrittr"
)

# Install missing packages and load all
invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet =TRUE)  
    library(pkg, character.only = TRUE, quietly = TRUE)  
  }
}))

source("12_lasem/rscript/LaSEM_functions.R")

path_lookup_raster_inputs <- "data/lasem/sample_datasets/crop_suitability_spatial_input_tts.csv"
path_lookup_crop_suitability <- "data/lasem/crop_parameters/kesesuaian_alpukat.csv"
path_lookup_intervention <- "data/lasem/lookup_tables/lookup_intervention.csv"
path_output <- "output/test_lasem" 

if (file.exists(normalizePath("12_lasem/report_template/LaSEM_report.Rmd"))){
  path_report_template <- normalizePath("12_lasem/report_template/LaSEM_report.Rmd")
} else if (file.exists(normalizePath("../report_template/LaSEM_report.Rmd"))){
  path_report_template <- normalizePath("../report_template/LaSEM_report.Rmd")
} else {
  errorCondition(message = "Report template file is not found.")
}

# Load Biophysical Raster Inputs -------------------------------------------

start_time <- Sys.time()
cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

input_paths <- 
  read_csv(path_lookup_raster_inputs) %>%
  dplyr::filter(availability %in% "Yes")

input_suit_factors <- input_paths %>% read_raster_files()

stackedRasters <- stack_raster_layers(input_suit_factors, 
                                      input_paths$parameter_name)


# Load Crop Suitability Table ---------------------------------------------
cropSuitabilityData <- read_csv(path_lookup_crop_suitability)


# Load Intervention Table -------------------------------------------------
interventionTable <- read_csv(path_lookup_intervention)


# Run suitability analysis ------------------------------------------------
suitability_results <- perform_suitability_analysis(harmonised_rasters = stackedRasters,
                             suitability_parameter = cropSuitabilityData,
                             lookup_intervention = interventionTable)

# 4. Export files ------------------------------------------------------------
dir.create(path_output, recursive = TRUE, showWarnings = FALSE)

# a. harmonised raster for report generating reports
file_name_soil_climate_factors_rds <- paste0(path_output,"/soil_climatic_factors.rds")
stackedRasters |> 
  terra::wrap() |>
  saveRDS(file_name_soil_climate_factors_rds)

# b. actual and potential suitability in rds format
file_name_land_suit_rds <- paste0(path_output,"/land_suitability.rds")
suitability_results[["suitability_polygon"]] |> 
  saveRDS(file_name_land_suit_rds)

# c. Actual and potential suitability polygon (should be checked and tidied up)
file_name_land_suit_shp <- paste0(path_output,"/land_suitability.shp")


suitability_results[["suitability_polygon"]] %>% 
  mutate(across(where(is.list) & !geometry, ~sapply(., function(x) paste(x, collapse = ", ")))) %>%
  sf::st_make_valid() %>%
  rename(
    cat = categories,
    suit = suitability,
    lmt_fact_a = limiting_factor_actual,
    lmt_fact_p = limiting_factor_potential,
    suit_pot_l = suitability_potential_low,
    suit_pot_m = suitability_potential_med,
    suit_pot_h = suitability_potential_high
  ) %>%
  rename_with(~substr(gsub("[^a-zA-Z0-9]", "", .), 1, 10)) %>%
  sf::st_write(., 
               file_name_land_suit_shp,
               append = FALSE, 
               driver = "ESRI Shapefile")

# d. Actual suitability raster map 

file_name_land_suit_tif  <- paste0(path_output, "/land_suitability.tif")
writeRaster(suitability_results[["suitability_raster"]], file_name_land_suit_tif, overwrite = TRUE)

# e. Actual suitability raster lookup table

file_name_land_suit_lookup_csv  <- paste0(path_output,"/land_suitability_lookup.csv")
write_csv(suitability_results[["lookup_suitability_factors"]], file_name_land_suit_lookup_csv)

# f. suitability layers for a certain crop for each soil and climatic factors
file_name_suit_factors_tif  <- paste0(path_output, "/land_suitability_factors.tif")
writeRaster(suitability_results[["suitability_by_factors"]], file_name_suit_factors_tif, overwrite = TRUE)

# End of the script
end_time <- Sys.time()
cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")

# 5. Reporting ---------------------------------------------------------------
session_log <- format_session_info_table()

report_params <- list(
  start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
  end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    file_name_soil_climate_factors_rds =file_name_soil_climate_factors_rds,
    file_name_land_suit_rds =  file_name_land_suit_rds,
    file_name_land_suit_tif = file_name_land_suit_tif,
    file_name_land_suit_shp = file_name_land_suit_shp,
    file_name_land_suit_lookup_csv = file_name_land_suit_lookup_csv,
    file_name_suit_factors_tif = file_name_suit_factors_tif,
    path_lookup_raster_inputs = path_lookup_raster_inputs,
    path_lookup_crop_suitability = path_lookup_crop_suitability,
    path_lookup_intervention = path_lookup_intervention,
    path_output = path_output,
    session_log = session_log
)


# Render the R Markdown report
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
}

rmarkdown::render(
  input = path_report_template,
  output_file = "LaSEM_report.html",
  output_dir = path_output,
  knit_root_dir = rprojroot::find_rstudio_root_file(),
  params = report_params
)
