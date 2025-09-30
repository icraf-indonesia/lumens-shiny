# Define data directory
data_dir <- "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny"

pathLULCT1 <- file.path(data_dir, "data/raster/bungo_landcover_1990r.tif")
pathLULCT2 <- file.path(data_dir, "data/raster/bungo_landcover_2000r.tif")
valueT1 <- 1990
valueT2 <- 2000
pathPU <- file.path(data_dir, "data/raster/bungo_zone.tif")
pathLookupPU<- file.path(data_dir, "data/table/zone_table_bungo.csv")
pathLookupNPV<- file.path(data_dir, "data/table/profitability_table_bungo.csv")
pathLookupCARBON<- file.path(data_dir, "data/table/carbon_bungo.csv")
period <- valueT2 - valueT1

# PREPROCESSING DATA #### 
LULCT1 <- rast(pathLULCT1)
LookupCARBON<- read_csv(pathLookupCARBON)
LookupCARBON <- LookupCARBON %>% dplyr::select(ID = 1, LC = 2, Carbon = 3)

# Get the names of raster_file
name_rast1 <- names(LULCT1)

# Set the levels of raster_file to be lookup_table
levels(LULCT1) <- LookupCARBON

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
levels(LULCT2) <- LookupCARBON

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
  group_by(across(everything())) %>%
  mutate(Freq = n()) %>%
  ungroup() %>%
  distinct() %>%
  mutate(Ha = Freq * area_ha_per_pixel)

colnames(combinedRasterTable)[1:3] <- c("PU", "LC1", "LC2")

LookupNPV <- read_csv(pathLookupNPV)
LookupNPV <- LookupNPV %>% dplyr::select(ID = 1, LC = 2, NPV = 3)

# Join NPV lookup for LC1 and LC2
combinedRasterTable <- combinedRasterTable %>%
  # Join NPV lookup for LC1 and LC2
  left_join(LookupNPV %>% rename_all(~paste0(., "_LC1")), by = c("LC1" = "LC_LC1")) %>%
  left_join(LookupNPV %>% rename_all(~paste0(., "_LC2")), by = c("LC2" = "LC_LC2")) %>%
  # Join Carbon lookup for LC1 and LC2
  left_join(LookupCARBON %>% rename(C_T1 = Carbon), by = c("LC1" = "LC")) %>%
  left_join(LookupCARBON %>% rename(C_T2 = Carbon), by = c("LC2" = "LC")) %>%
  select(-ID.x, -ID.y) %>%
  mutate(LULCC = paste(LC1, "to", LC2))

# Compute NPV Change
combinedRasterTable$NPV1 <- combinedRasterTable$NPV_LC1*combinedRasterTable$Ha
combinedRasterTable$NPV2 <- combinedRasterTable$NPV_LC2*combinedRasterTable$Ha
combinedRasterTable$deltaNPV <- combinedRasterTable$NPV2 - combinedRasterTable$NPV1
total_area <- sum(combinedRasterTable$Ha, na.rm = TRUE)

build_opcost_table <- function(dt_quesc_npv, period, total_area) {
  data_em_sel <- combinedRasterTable
  
  data_em_sel <- within(data_em_sel, {
    em_rate <- ((C_T1 - C_T2) * (Ha * 3.67)) / (total_area * period)
    em_tot <- (C_T1 - C_T2) * 3.67
    opcost <- (NPV2 - NPV1) / em_tot
  })
  
  opcost_tab <- data.frame(
    luchg = data_em_sel$LULCC,
    zone = data_em_sel$PU,
    opcost = data_em_sel$opcost,
    emrate = data_em_sel$em_rate
  ) %>%
    filter(!is.nan(opcost), !is.na(opcost))
  
  opcost_tab_p <- opcost_tab[opcost_tab$opcost >= 0, ]
  opcost_tab_p <- opcost_tab_p[order(opcost_tab_p$opcost), ]
  opcost_tab_p$cum_emrate <- cumsum(opcost_tab_p$emrate)
  opcost_tab_p$opcost_log <- log10(opcost_tab_p$opcost)
  is.na(opcost_tab_p) <- sapply(opcost_tab_p, is.infinite)
  opcost_tab_p[is.na(opcost_tab_p)] <- 0
  
  opcost_tab_n <- opcost_tab[opcost_tab$opcost < 0, ]
  opcost_tab_n <- opcost_tab_n[order(opcost_tab_n$opcost), ]
  opcost_tab_n$cum_emrate <- cumsum(opcost_tab_n$emrate)
  opcost_tab_n$opcost_log <- log10(-1 * opcost_tab_n$opcost) * -1
  
  opcost_all <- rbind(opcost_tab_n, opcost_tab_p)
  opcost_all$cum_emrate2 <- as.factor(opcost_all$cum_emrate)
  list(opcost_all = opcost_all)
}

# Build the opportunity cost table based on the land use change period and total area
opcost_result <- build_opcost_table(combinedRasterTable, period, total_area)
opcost_table <- opcost_result$opcost_all
opcost_table$order <- c(1:nrow(opcost_table))

df_curve <- data.frame(
  emission_rate = opcost_table$emrate,
  opportunity_cost = opcost_table$opcost,
  log_opportunity_cost = opcost_table$opcost_log,
  land_use_change = opcost_table$luchg
)

# Group data by land use change
df_grouped <- df_curve %>%
  group_by(land_use_change) %>%
  summarise(emission_rate = sum(emission_rate),
            opportunity_cost = sum(opportunity_cost), .groups = "drop")

# Filter and order data
df_all <- df_grouped %>% filter(opportunity_cost != 0)

df <- df_all %>%
  filter(emission_rate > 0) %>%
  mutate(
    opportunity_cost_log = case_when(
      opportunity_cost > 0 ~ log10(opportunity_cost),
      opportunity_cost < 0 ~ -log10(abs(opportunity_cost)),
      opportunity_cost == 0 ~ 0
    )
  ) %>%
  arrange(opportunity_cost_log) %>% 
  mutate(
    xmin = lag(cumsum(emission_rate), default = 0),
    xmax = cumsum(emission_rate),
    hover_text = paste0(
      "Land Use Change: ", land_use_change, "<br>",
      "Opportunity Cost: ", round(opportunity_cost, 2), "<br>",
      "Emission Rate: ", round(emission_rate, 2)
    )
  )

p <- ggplot(df) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost_log,
    fill = land_use_change,
    text = hover_text   # << important for ggplotly tooltip
  ), color = "black") +
  # geom_text(aes(
  #   x = (xmin + xmax) / 2,
  #   y = opportunity_cost + ifelse(opportunity_cost > 0, 5, -5),
  #   label = label_wrapped
  # ), size = 1) +
  labs(
    x = "Emission Rate (ton CO2e/ ha year)",
    y = "Opportunity Cost (currency/ton CO2e)",
    title = "Abatement Cost Curve"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(p, tooltip = "text")
