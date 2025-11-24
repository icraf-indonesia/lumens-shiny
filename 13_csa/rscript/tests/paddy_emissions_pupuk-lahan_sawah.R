library(terra)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(tidyr)

pathLULC = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/1_Raster/Penutupan Lahan/lc_2010.tif"
pathLookupLC = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/3_Tabular/tabel_acuan_penutupan_lahan_sulsel.csv"
pathPU = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/1_Raster/Unit Perencanaan/admin_kabupaten.tif"
pathLookupPU = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/3_Tabular/tabel_acuan_administrasi_kabupaten_sulsel.csv"
year = 2010
pathLookupCO2 = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/CO2_conversion_lookup_table.csv"
pathLookupSF = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/scaling_factors_lookup_table.csv"
pathLookupPupuk = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/dosis_pupuk_lookup_table.csv"
pathLookupN2O = "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/data/sulsel/N2O_lookup_table.csv"

# PREPROCESSING DATA #### 
LULC <- rast(pathLULC)
LookupLC <- read_csv(pathLookupLC)
LookupSF <- read_csv(pathLookupSF)
LookupCO2 <- read_csv(pathLookupCO2)
LookupPupuk <- read_csv(pathLookupPupuk)
LookupN2O <- read_csv(pathLookupN2O)

get_lookup_value <- function(tbl, variable_name) {
  tbl %>%
    filter(Variable == variable_name) %>%
    pull(Value)
}

get_rotation_factor <- function(tbl, var_name, rotation_vars) {
  value <- get_lookup_value(tbl, var_name)
  total <- tbl %>%
    filter(Variable %in% rotation_vars) %>%
    pull(Value) %>%
    sum()
  
  value / total
}

# Get the names of raster_file
name_rast <- names(LULC)

# Set the levels of raster_file to be lookup_table
levels(LULC) <- LookupLC

# Set the names of raster_file
LULC <- setNames(LULC, name_rast)

# Set the year if year is not NULL
if (!is.null(year)) {
  terra::time(LULC, tstep="years") <- year
}

PU <- rast(pathPU)
LookupPU <- read_csv(pathLookupPU)

# Get the names of raster_file
name_PU <- names(PU)

# Set the levels of raster_file to be lookup_table
levels(PU) <- LookupPU

# Set the names of raster_file
PU <- setNames(PU, name_PU)

# PU, LULCT1, LULCT2
# harmonise PU extent according to the LULCT1 geometry
PU <- terra::resample(PU, LULC, method="near")

combinedRaster <- c(PU, LULC)

# Calculate pixel area in Ha
res_m <- terra::res(LULC) # resolution in meters (width, height)
area_ha_per_pixel <- (res_m[1] * res_m[2]) / 10000  # m² to Ha

# Build the frequency and area table
combinedRasterTable <- combinedRaster %>%
  as_tibble() %>%
  tidyr::drop_na() %>%
  setNames(c("PU", "LC")) %>%
  dplyr::filter(LC == "Pertanian") %>% 
  group_by(across(everything())) %>%
  mutate(Freq = n()) %>%
  ungroup() %>%
  distinct() %>%
  mutate(Ha = Freq * area_ha_per_pixel)

# --- Prepare Lookup Table ---
Lookup_wide <- LookupSF %>%
  select(variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(
    Total_EF = EF * SFw * SFs * SFr
  )

combinedRasterTable <- combinedRasterTable %>%
  mutate(
    # CH₄ emission (Ton CH₄/tahun)
    CH4_emission = Lookup_wide$Total_EF * Lookup_wide$t * Ha * 1e-3,
    # CH₄ emission converted to CO₂-equivalent (Ton CO₂-eq/tahun)
    CH4_emission_CO2 = CH4_emission * LookupCO2$Value[LookupCO2$Variable == "GWP_CH4"] * 1e-6
  )

#### PERHITUNGAN N2O ####
# Hitung N tunggal (rata-rata Urea)
n_table <- LookupPupuk %>%
  group_by(KABUPATEN) %>%
  summarise(
    `N Urea` = mean(PT_UREA, na.rm = TRUE)
  )

# Mengalikan untuk mendapatkan N2O
n_urea_factor  <- get_lookup_value(LookupN2O, "N Urea")
n_table <- n_table %>%
  mutate(
    `N Tunggal` = `N Urea` * n_urea_factor
  )

# Lookup factor names
rotation_vars <- c(
  "Rotasi padi 1 kali",
  "Rotasi padi 2-3 kali",
  "Tidak ditanami padi"
)

# Extract factors with helper functions
area100_factor <- get_lookup_value(LookupN2O, "100% dosis pupuk")
area50_factor  <- get_lookup_value(LookupN2O, "50% dosis pupuk")

rotation1_factor <- get_rotation_factor(LookupN2O, "Rotasi padi 1 kali", rotation_vars)
rotation2_factor <- get_rotation_factor(LookupN2O, "Rotasi padi 2-3 kali", rotation_vars)

N2O_emission <- combinedRasterTable %>%
  mutate(
    N2O_emission_100_1 = Ha * area100_factor * rotation1_factor,
    N2O_emission_100_2 = Ha * area100_factor * rotation2_factor,
    N2O_emission_50_1  = Ha * area50_factor  * rotation1_factor,
    N2O_emission_50_2  = Ha * area50_factor  * rotation2_factor
  )

# Extract factors with helper functions
EF_N2O <- get_lookup_value(LookupCO2, "EF_N2O")
EF_CO2 <- get_lookup_value(LookupCO2, "EF_CO2")
GWP_N2O  <- get_lookup_value(LookupCO2, "GWP_N2O")

N2O_emission_CO2 <- N2O_emission %>%
  mutate(
    N2O_emission_CO2_100_1 = ((N2O_emission_100_1 * n_table$`N Tunggal` * EF_N2O * GWP_N2O) + (N2O_emission_100_1 * n_table$`N Tunggal` * EF_CO2))/1000,
    N2O_emission_CO2_100_2 = ((N2O_emission_100_2 * n_table$`N Tunggal` * 2.5 * EF_N2O * GWP_N2O) + (N2O_emission_100_2 * n_table$`N Tunggal` * 2.5 * EF_CO2))/1000,
    N2O_emission_CO2_50_1  = ((N2O_emission_50_1 * n_table$`N Tunggal` * 0.5 * EF_N2O * GWP_N2O) + (N2O_emission_50_1 * n_table$`N Tunggal` * 0.5 * EF_CO2))/1000,
    N2O_emission_CO2_50_2  = ((N2O_emission_50_2 * n_table$`N Tunggal` * 2.5 * 0.5 * EF_N2O * GWP_N2O) + (N2O_emission_50_2 * n_table$`N Tunggal` * 2.5 * 0.5 * EF_CO2))/1000,
    # Total N2O emissions across all scenarios (Juta Ton CO2-eq/tahun)
    N2O_emission_CO2_total =
      (N2O_emission_CO2_100_1 +
      N2O_emission_CO2_100_2 +
      N2O_emission_CO2_50_1  +
      N2O_emission_CO2_50_2) * 1e-6
  )

# SUM CH4_emission_CO2 by PU
summary_by_PU <- N2O_emission_CO2 %>%
  group_by(PU) %>%
  select(CH4_emission_CO2, N2O_emission_CO2_total) %>% 
  mutate(
    `Total Emission (Juta Ton CO2-eq/tahun)` =
      sum(CH4_emission_CO2, N2O_emission_CO2_total, na.rm = TRUE)
  )

p <- ggplot(summary_by_PU,
            aes(
              x = reorder(PU, `Emission from CH4 (Juta Ton CO2-eq/tahun)`),
              y = `Emission from CH4 (Juta Ton CO2-eq/tahun)`,
              text = paste0(
                "PU: ", PU, "<br>",
                "Emisi: ", comma(`Emission from CH4 (Juta Ton CO2-eq/tahun)`), " Juta Ton CO₂-eq/tahun"
              )
            )) +
  geom_col(fill = "#2E86C1") +   
  labs(
    title = paste0("Emisi CH₄ (CO₂-eq) per PU pada ", year) ,
    x = "",
    y = "Juta Ton CO₂-eq / tahun"
  ) +
  theme_minimal() +
  coord_flip()

ggplotly(p, tooltip = "text")

# Convert to long format for stacked plotting
summary_long <- summary_by_PU %>%
  select(-`Total Emission (Juta Ton CO2-eq/tahun)`) %>% 
  pivot_longer(
    cols = c(CH4_emission_CO2, N2O_emission_CO2_total),
    names_to = "Gas",
    values_to = "Value"
  )

q <- ggplot(summary_long,
            aes(
              x = reorder(PU, Value),
              y = Value,
              fill = Gas,
              text = paste0(
                "PU: ", PU, "<br>",
                "Gas: ", Gas, "<br>",
                "Emisi: ", 
                format(round(Value, 2), big.mark = ",", scientific = FALSE),
                " Juta Ton CO₂-eq/tahun"
              )
            )) +
  geom_col() +
  labs(
    title = "Emisi CH₄ dan N₂O (CO₂-eq) per PU",
    x = "PU",
    y = "Juta Ton CO₂-eq / tahun",
    fill = "Jenis Gas"
  ) +
  theme_minimal() +
  coord_flip()

ggplotly(q, tooltip = "text")

r <- ggplot(summary_long,
            aes(
              x = reorder(PU, Value),
              y = Value,
              fill = Gas,
              text = paste0(
                "PU: ", PU, "<br>",
                "Gas: ", Gas, "<br>",
                "Emisi: ", 
                format(round(Value, 5), big.mark = ",", scientific = FALSE),
                " Juta Ton CO₂-eq/tahun"
              )
            )) +
  geom_col(position = "fill") +     # <<--- 100% STACKED BAR
  labs(
    title = "Proporsi Emisi CH₄ dan N₂O (CO₂-eq) per PU",
    x = "PU",
    y = "Proporsi (%)",
    fill = "Jenis Gas"
  ) +
  scale_y_continuous(labels = scales::percent) +  # Convert axis to %
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # rotate because of coord_flip
  )

ggplotly(r, tooltip = "text")

#### CONTOH ####
df <- data.frame(
  PU = c(
    "BANTAENG","BARRU","BONE","BULUKUMBA","ENREKANG","GOWA","JENEPONTO",
    "KEPULAUAN SELAYAR","LUWU","LUWU TIMUR","LUWU UTARA","MAROS",
    "PANGKAJENE DAN KEPULAUAN","PINRANG","SIDENRENG RAPPANG","SINJAI",
    "SOPPENG","TAKALAR","TANA TORAJA","TORAJA UTARA","WAJO",
    "KOTA MAKASSAR","KOTA PALOPO","KOTA PARE-PARE"
  ),
  Ha = c(
    6630,9027,90930,17107,3846,26873,23984,5,32798,25095,27486,22785,14115,
    52968,45595,26317,21855,19907,14566,15757,90134,1937,2321,861
  ))

combinedRasterTable_test <- df

# --- Prepare Lookup Table ---
Lookup_wide <- LookupSF %>%
  select(variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(
    Total_EF = EF * SFw * SFs * SFr
  )

combinedRasterTable_test <- combinedRasterTable_test %>%
  mutate(
    # CH₄ emission (Ton CH₄/tahun)
    CH4_emission = Lookup_wide$Total_EF * Lookup_wide$t * Ha * 1e-3,
    # CH₄ emission converted to CO₂-equivalent (Ton CO₂-eq/tahun)
    CH4_emission_CO2 = CH4_emission * LookupCO2$Value[LookupCO2$Variable == "GWP_CH4"] * 1e-6
  )

N2O_emission_test <- combinedRasterTable_test %>%
  mutate(
    N2O_emission_100_1 = Ha * area100_factor * rotation1_factor,
    N2O_emission_100_2 = Ha * area100_factor * rotation2_factor,
    N2O_emission_50_1  = Ha * area50_factor  * rotation1_factor,
    N2O_emission_50_2  = Ha * area50_factor  * rotation2_factor
  )

N2O_emission_CO2_test <- N2O_emission_test %>%
  mutate(
    N2O_emission_CO2_100_1 = ((N2O_emission_100_1 * n_table$`N Tunggal` * EF_N2O * GWP_N2O) + (N2O_emission_100_1 * n_table$`N Tunggal` * EF_CO2))/1000,
    N2O_emission_CO2_100_2 = ((N2O_emission_100_2 * n_table$`N Tunggal` * 2.5 * EF_N2O * GWP_N2O) + (N2O_emission_100_2 * n_table$`N Tunggal` * 2.5 * EF_CO2))/1000,
    N2O_emission_CO2_50_1  = ((N2O_emission_50_1 * n_table$`N Tunggal` * 0.5 * EF_N2O * GWP_N2O) + (N2O_emission_50_1 * n_table$`N Tunggal` * 0.5 * EF_CO2))/1000,
    N2O_emission_CO2_50_2  = ((N2O_emission_50_2 * n_table$`N Tunggal` * 2.5 * 0.5 * EF_N2O * GWP_N2O) + (N2O_emission_50_2 * n_table$`N Tunggal` * 2.5 * 0.5 * EF_CO2))/1000,
    # Total N2O emissions across all scenarios (Juta Ton CO2-eq/tahun)
    N2O_emission_CO2_total =
      (N2O_emission_CO2_100_1 +
         N2O_emission_CO2_100_2 +
         N2O_emission_CO2_50_1  +
         N2O_emission_CO2_50_2) * 1e-6
  )

summary_by_PU_test <- N2O_emission_CO2_test %>%
  group_by(PU) %>%
  select(CH4_emission_CO2, N2O_emission_CO2_total) %>% 
  mutate(
    `Total Emission (Juta Ton CO2-eq/tahun)` =
      sum(CH4_emission_CO2, N2O_emission_CO2_total, na.rm = TRUE)
  )
