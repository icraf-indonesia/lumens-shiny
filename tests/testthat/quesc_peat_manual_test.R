source('04_quesc/rscript/function_ques_c.R')
source('03_preques/rscript/functions_ques_pre.R')

install_load(
  "terra",
  "shiny",
  "shinyFiles",
  "raster",
  "splitstackshape",
  "ggplot2",
  "foreign",
  "reshape2",
  "dplyr",
  "reshape",
  "reshape2",
  "purrr",
  "plotly",
  "sf",
  "shinyvalidate",
  "remotes",
  "shinyjs",
  "rmarkdown",
  "bslib",
  "shinyalert",
  "data.table",
  "magrittr",
  "tidyr",
  "tidyterra",
  "plotly",
  "stringr",
  "readr",
  "readxl",
  "tools",
  "leaflet",
  "stars",
  "mapview",
  "writexl",
  "DT",
  "networkD3"
)

results <- run_quesc_analysis(
  lc_t1_path = "C:/Users/fmahezs/Downloads/WH/KHG_2019_F4_100m.tif",
  lc_t2_path = "C:/Users/fmahezs/Downloads/WH/KHG_2020_HM_100m.tif",
  admin_z_path = "C:/Users/fmahezs/Downloads/WH/PU/PU_KHG_UTM.shp",
  peat_map_path = "C:/Users/fmahezs/Downloads/WH/Area/FEG_KHG_48S.shp",
  c_lookup_path = "C:/Users/fmahezs/Downloads/wh/Tabular/carbon_combined.csv",
  peat_emission_factor_table_path = "C:/Users/fmahezs/Downloads/WH/Tabular/em_peat_combined.csv",
  time_points = list(t1 = 2019, t2 = 2020),
  output_dir = "C:/Users/fmahezs/Downloads/WH/output/quesc2019-2020/",
  peat_decomposition = "Yes"
  # progress_callback = function(value, detail) {
  #   setProgress(value = value, message = detail)
  # }
)

map_c1 <- results$map_c1 
map_c2 <- results$map_c2 
map_e <- results$map_em 
map_s <- results$map_sq 
tbl_quesc <- results$ques_db
lc_t1 <- results$lc_t1
lc_t2 <- results$lc_t2 
zone <- results$zone
df_pu <- results$zone_lookup_input
df_c <- results$df_c
crosstab_peat <- results$crosstab

# Peat decomposition analysis ---------------------------------------------

results_peat <- run_quesc_peat_analysis(
  output_dir = "C:/Users/fmahezs/Downloads/quesc_peat_test/output/",
  lc_t1_path = "C:/Users/fmahezs/Downloads/quesc_peat_test/data/lc2000_aligned.tif",
  lc_t2_path = "C:/Users/fmahezs/Downloads/quesc_peat_test/data/lc2020_aligned.tif",
  admin_z_path = "C:/Users/fmahezs/Downloads/quesc_peat_test/data/Kecamatan OKI.shp",
  peat_map_path = "C:/Users/fmahezs/Downloads/quesc_peat_test/data/peat_oki_f.shp",
  peat_emission_factor_table_path = "C:/Users/fmahezs/Downloads/quesc_peat_test/data/lc_peat_em.xlsx",
  crosstab_peat = crosstab_peat,
  t1 = 2000,
  t2 = 2020
)

# Access peat emission results
tbl_quesc_peat <- results_peat$chg_pdtable
map_e_peat <- results_peat$em_map
peatmap <- results_peat$peatmap
lookup_c.pt <- results_peat$lookup_c.pt

# -------------------------------------------------------------------------

library(dplyr)

# 1. Define your year variables as strings
t1 <- "2000"
t2 <- "2020"
peat_name <- names(peatmap)

# 2. Combine the dataframes using dynamic references
quesc_df <- tbl_quesc %>%
  full_join(
    tbl_quesc_peat,
    by = c("ID_PU", "PU", "ID_LC2", t2, "ID_LC1", t1, "LU_CHG"),
    suffix = c("_nonpeat", "_peat"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    !!peat_name := coalesce(peat_area_peat, peat_area_nonpeat),
    Freq = coalesce(Freq_nonpeat, Freq_peat),
    Ha = coalesce(Ha_nonpeat, Ha_peat),
    EM_Total = EM + Peat_EM
  ) %>%
  select(
    ID_PU, PU, ID_LC2, 
    all_of(t2), C_T2, FE_T2,   
    ID_LC1, 
    all_of(t1), C_T1, FE_T1,  
    Freq, Ha, !!peat_name, EM, SQ, Peat_EM, EM_Total, LU_CHG
  )

quesc_df <- quesc_df %>%
  rename(EM = EM_Total, EM_Mineral = EM, EM_Peat = Peat_EM) %>% 
  relocate(SQ, .after = EM)

