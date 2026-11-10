# Run from the repository root: Rscript 07_ta-profit/rscript/tests/render_bungo_review.R
# Uses the repository Bungo rasters, EAE and carbon lookup, and planning units.
suppressPackageStartupMessages({
  library(shiny)
  library(terra)
  library(tidyverse)
  library(plotly)
  library(openxlsx)
  library(RColorBrewer)
})
source('07_ta-profit/rscript/functions.R', encoding = 'UTF-8')

input_dir <- normalizePath('07_ta-profit/rscript/tests/data', winslash = '/')
output_dir <- file.path(getwd(), '07_ta-profit/rscript/tests/bungo_review_output')
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = '/')

lookup_path <- normalizePath('data/table/profitability_table_bungo.csv', winslash = '/')
lookup <- read.csv(lookup_path)
carbon_path <- file.path(output_dir, 'bungo_carbon.csv')
write.csv(lookup[, c('ID', 'LC', 'CARBON')], carbon_path, row.names = FALSE)
pu_path <- normalizePath('data/vector/planning_unit_bungo.shp', winslash = '/')
paths <- list(
  pathLULCT1 = file.path(input_dir, 'bungo_landcover_1990r.tif'),
  pathLULCT2 = file.path(input_dir, 'bungo_landcover_2010r.tif'),
  pathPU = pu_path,
  pathLookupPU = NULL,
  pathLookupNPV = lookup_path,
  pathLookupCstock = carbon_path
)
started <- Sys.time()
result <- do.call(preprocess_data, c(paths, list(valueT1 = 1990, valueT2 = 2010,
                                               puIsVector = TRUE)))
pu_names <- unique(result$combinedRasterTable$PU)
pu_outputs <- setNames(lapply(pu_names, function(name) {
  process_pu_data(filter(result$combinedRasterTable, PU == name), name, 'IDR')
}), pu_names)
params <- generate_report_params(
  result, result, paths,
  list(start_time = started, end_time = Sys.time(), valueT1 = 1990,
       valueT2 = 2010, eae_year = 'tidak diketahui'),
  output_dir, pu_outputs, 'IDR'
)
rmarkdown::render('07_ta-profit/report_template/ta-profit-ID.Rmd',
                  output_file = file.path(output_dir, 'bungo_review.html'),
                  params = params, envir = new.env(parent = globalenv()), quiet = FALSE)
cat('\nReport:', file.path(output_dir, 'bungo_review.html'), '\n')
