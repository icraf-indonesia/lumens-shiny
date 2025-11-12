library(terra)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)

pathLULCT1 = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/1_Raster/Penutupan Lahan/lc_2010.tif"
pathLULCT2 = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/1_Raster/Penutupan Lahan/lc_2020.tif"
pathLookupLC = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/3_Tabular/tabel_acuan_penutupan_lahan_sulsel.csv"
pathPU = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/1_Raster/Unit Perencanaan/admin_kabupaten.tif"
pathLookupPU = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/3_Tabular/tabel_acuan_administrasi_kabupaten_sulsel.csv"
valueT1 = 2010
valueT2 = 2020

# PREPROCESSING DATA #### 
LULCT1 <- rast(pathLULCT1)
LookupLC <- read_csv(pathLookupLC)

# Get the names of raster_file
name_rast1 <- names(LULCT1)

# Set the levels of raster_file to be lookup_table
levels(LULCT1) <- LookupLC

# Set the names of raster_file
LULCT1 <- setNames(LULCT1, name_rast1)

# Set the year if year is not NULL
if (!is.null(valueT1)) {
  terra::time(LULCT1, tstep="years") <- valueT1
}

LULCT2 <- rast(pathLULCT2)

# Get the names of raster_file
name_rast2 <- names(LULCT2)

# Set the levels of raster_file to be lookup_table
levels(LULCT2) <- LookupLC

# Set the names of raster_file
LULCT2 <- setNames(LULCT2, name_rast2)

# Set the year if year is not NULL
if (!is.null(valueT2)) {
  terra::time(LULCT2, tstep="years") <- valueT2
}

PU <- rast(pathPU)
LookupPU <- read_csv(pathLookupPU)

# Get the names of raster_file
name_PU <- names(PU)
name_PU <- "bungo_zone"

# Set the levels of raster_file to be lookup_table
levels(PU) <- LookupPU

# Set the names of raster_file
PU <- setNames(PU, name_PU)

# PU, LULCT1, LULCT2
# harmonise PU extent according to the LULCT1 geometry
PU <- terra::resample(PU, LULCT1, method="near")

combinedRaster <- c(PU, LULCT1, LULCT2)

# Calculate pixel area in Ha
res_m <- terra::res(LULCT1) # resolution in meters (width, height)
area_ha_per_pixel <- (res_m[1] * res_m[2]) / 10000  # m² to Ha

# Build the frequency and area table
combinedRasterTable <- combinedRaster %>%
  as_tibble() %>%
  tidyr::drop_na() %>%
  setNames(c("PU", "LC1", "LC2")) %>%
  dplyr::filter(LC2 == "Pertanian") %>% 
  group_by(across(everything())) %>%
  mutate(Freq = n()) %>%
  ungroup() %>%
  distinct() %>%
  mutate(Ha = Freq * area_ha_per_pixel)

combinedRasterTable <- combinedRasterTable %>%
  mutate(
    CH4_emission = 1.61 * 240 * Ha * 10^-3, # Units: Ton CH4/tahun
    CH4_emission_CO2 = CH4_emission * 28 * 10^-6 # Units: Juta Ton CO2-eq/tahun
  )

# SUM CH4_emission_CO2 by PU
summary_by_PU <- combinedRasterTable %>%
  group_by(PU) %>%
  summarise(
    "Emission Total (Juta Ton CO2-eq/tahun)" = sum(CH4_emission_CO2, na.rm = TRUE)
  )

p <- ggplot(summary_by_PU,
            aes(
              x = reorder(PU, `Emission Total (Juta Ton CO2-eq/tahun)`),
              y = `Emission Total (Juta Ton CO2-eq/tahun)`,
              text = paste0(
                "PU: ", PU, "<br>",
                "Emisi: ", comma(`Emission Total (Juta Ton CO2-eq/tahun)`), " Juta Ton CO₂-eq/tahun"
              )
            )) +
  geom_col(fill = "#2E86C1") +   
  labs(
    title = paste0("Emisi CH₄ (CO₂-eq) per PU periode ", valueT1, "-", valueT2) ,
    x = "",
    y = "Juta Ton CO₂-eq / tahun"
  ) +
  theme_minimal() +
  coord_flip()

ggplotly(p, tooltip = "text")
