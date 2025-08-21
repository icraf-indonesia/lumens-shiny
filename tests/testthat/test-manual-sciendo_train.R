source('11_sciendo-train/rscript/function_sciendo_train.R')

install_load(
  "shinyFiles", "shinyvalidate", "shinyjs", "bslib", "sf", "raster",
  "dplyr", "remotes", "rmarkdown", "XML", "splitstackshape", "shinyalert",
  "usdm", "corrplot", "tidyr", "openxlsx", "openxlsx2", "RDCOMClient", "XML", "xml2"
)

tryCatch({
  lc1 <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/raster/bungo_landcover_2005r.tif"
  lc2 <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/raster/bungo_landcover_2010r.tif"
  zone <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/raster/bungo_zone.tif"
  zone_tbl <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/table/zone_table_bungo.csv"
  lc_tbl_file <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/table/landuse_table_bungo.csv"
  lc_tbl <- read.csv(lc_tbl_file)
  factors <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/data/lucmodel/"
  t1 <- 2005
  t2 <- 2010
  dinamica <- "C:/Program Files/Dinamica EGO 7/"
  out <- "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/output/sciendo_train_test/"
})

result <- run_sciendo_train_process(
  lc_t1_path = lc1,
  lc_t2_path = lc2,
  zone_path = zone,
  lc_lookup_table_path = lc_tbl_file,
  lc_lookup_table = lc_tbl,
  z_lookup_table_path = zone_tbl,
  factor_path = factors,
  time_points = list(t1 = t1, t2 = t2),
  dinamica_path = dinamica,
  output_dir = out,
  memory_allocation = 4
)
