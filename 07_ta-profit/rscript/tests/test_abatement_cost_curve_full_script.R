library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(purrr)
library(plotly)
# library(htmlwidgets)

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
currency <- "IDR"

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
    emrate = data_em_sel$em_rate,
    area = data_em_sel$Ha
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
  list(opcost_all = opcost_all, data_em_sel = data_em_sel)
}

# Build the opportunity cost table based on the land use change period and total area
opcost_result <- build_opcost_table(combinedRasterTable, period, total_area)
opcost_table <- opcost_result$opcost_all
npv_output_table <- opcost_result$data_em_sel %>% 
  select(-Freq, -ID_LC1, -ID_LC2, -C_T1, -C_T2, -NPV_LC1, -NPV_LC2) %>%
  filter(!is.nan(opcost), !is.na(opcost))

df_curve <- data.frame(
  emission_rate = opcost_table$emrate,
  opportunity_cost = opcost_table$opcost,
  log_opportunity_cost = opcost_table$opcost_log,
  land_use_change = opcost_table$luchg,
  planning_unit = opcost_table$zone,
  area = opcost_table$area
)

# Group data by land use change
df_grouped <- df_curve %>%
  group_by(land_use_change) %>%
  summarise(emission_rate = sum(emission_rate),
            opportunity_cost = sum(opportunity_cost), .groups = "drop")

# Filter and order data
df_all <- df_grouped %>% filter(opportunity_cost != 0)

df_s <- df_all %>%
  mutate(
    opportunity_cost_log = case_when(
      opportunity_cost > 0 ~ log10(opportunity_cost),
      opportunity_cost < 0 ~ -log10(abs(opportunity_cost)),
      opportunity_cost == 0 ~ 0
    )
  ) %>%
  arrange(opportunity_cost_log)

# Split into positive and negative emissions
df_pos <- df_s %>%
  filter(emission_rate >= 0) %>%
  mutate(
    xmin = lag(cumsum(emission_rate), default = 0),
    xmax = cumsum(emission_rate)
  )

df_neg <- df_s %>%
  filter(emission_rate < 0) %>%
  mutate(
    xmax = lag(cumsum(emission_rate), default = 0),
    xmin = cumsum(emission_rate)  # reversed stacking toward negative
  )

df_pu_dominance <- df_curve %>%
  group_by(land_use_change, planning_unit) %>%
  summarise(total_area = sum(area), .groups = "drop") %>%
  group_by(land_use_change) %>%
  mutate(
    land_use_total_area = sum(total_area),
    pct_of_largest_pu = total_area / land_use_total_area
  ) %>%
  slice_max(total_area, n = 1, with_ties = FALSE) %>%  # get largest PU only
  ungroup() %>%
  select(land_use_change, planning_unit, pct_of_largest_pu)

# Combine back
# df_s_fixed <- bind_rows(df_pos, df_neg) %>%
#   mutate(
#     hover_text = paste0(
#       "Land Use Change: ", land_use_change, "<br>",
#       "Opportunity Cost: ", scales::comma(opportunity_cost), "<br>",
#       "Emission Rate: ", scales::comma(emission_rate)
#     )
#   )

df_s_final <- bind_rows(df_pos, df_neg) %>%
  left_join(df_pu_dominance, by = "land_use_change") %>%
  mutate(
    hover_text = paste0(
      "Perubahan Lahan: ", land_use_change, "<br>",
      "Opportunity Cost: ", scales::comma(opportunity_cost), "<br>",
      "Laju Emisi: ", scales::comma(emission_rate), "<br>",
      "Dominasi Unit Perencanaan: ", planning_unit, " (", scales::percent(pct_of_largest_pu, accuracy = 0.1), ")"
    )
  )

# Calculate the x-axis limits from your data
x_limits <- range(c(df_s_final$xmin, df_s_final$xmax), na.rm = TRUE)
max_abs <- max(abs(x_limits))
x_range <- c(x_limits[1], max_abs)

# Calculate y-axis limits
y_limits <- range(c(0, df_s_final$opportunity_cost_log), na.rm = TRUE)

p <- ggplot(df_s_final) +
  # Add background for x < 0 (emissions/negative side)
  geom_rect(
    data = data.frame(xmin = x_range[1], xmax = 0, ymin = y_limits[1], ymax = y_limits[2]),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "#ffe6e6", alpha = 0.8, inherit.aes = FALSE
  ) +
  # Add background for x >= 0 (sequestration/positive side)
  geom_rect(
    data = data.frame(xmin = 0, xmax = x_range[2], ymin = y_limits[1], ymax = y_limits[2]),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "#e6f7e6", alpha = 0.8, inherit.aes = FALSE
  ) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost_log,
    fill = land_use_change,
    text = hover_text 
  ), color = "black") +
  labs(
    x = "Laju Emisi (ton CO<sub>2</sub>-eq/ha.tahun)",
    y = paste0("Opportunity Cost (", currency, "/ton CO<sub>2</sub>-eq)"),
    title = "Kurva Abatement Cost"
  ) +
  scale_x_continuous(
    limits = function(x) {
      max_abs <- max(abs(c(x[1], x[2])))
      c(min(x), max_abs)
    },
    breaks = function(x) pretty(x, n = 6),
    labels = function(x) scales::comma(x, accuracy = 0.01)
  ) +
  scale_y_continuous(
    breaks = function(x) floor(min(x)):ceiling(max(x)),
    labels = function(x) {
      # Handle both positive and negative log values
      values <- ifelse(x >= 0, 10^x, -10^abs(x))
      ifelse(
        values == floor(values),
        scales::comma(values, accuracy = 1),
        scales::comma(values)
      )
    }
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(p, tooltip = "text") %>%
  layout(
    hovermode = "x+y",
    xaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey'),
    yaxis = list(showspikes = TRUE, spikemode = 'across', spikesnap = 'cursor', spikethickness = 0.8, spikecolor = 'grey')
    # hoverlabel = list(bgcolor = "white")
  ) %>%
  config(
    displaylogo = FALSE,
    displayModeBar = TRUE,
    modeBarButtonsToAdd = list(
      list(
        name = "Fullscreen",
        icon = list(
          width = 20,
          height = 20,
          path = "M7 14H5v5h5v-2H7v-3zm-2-4h2V7h3V5H5v5zm12 7h-3v2h5v-5h-2v3zM14 5v2h3v3h2V5h-5z",
          transform = "scale(1)"
        ),
        click = htmlwidgets::JS("
          function(gd) {
            var fullscreenElement = document.fullscreenElement || document.webkitFullscreenElement || document.mozFullScreenElement || document.msFullscreenElement;
            if (!fullscreenElement) {
              var el = gd;
              if (el.requestFullscreen) {
                el.requestFullscreen();
              } else if (el.webkitRequestFullscreen) {
                el.webkitRequestFullscreen();
              } else if (el.mozRequestFullScreen) {
                el.mozRequestFullScreen();
              } else if (el.msRequestFullscreen) {
                el.msRequestFullscreen();
              }
            } else {
              if (document.exitFullscreen) {
                document.exitFullscreen();
              } else if (document.webkitExitFullscreen) {
                document.webkitExitFullscreen();
              } else if (document.mozCancelFullScreen) {
                document.mozCancelFullScreen();
              } else if (document.msExitFullscreen) {
                document.msExitFullscreen();
              }
            }
          }
        ")
      )
    ),
    scrollZoom = TRUE
  ) 

### per Planning Unit
# Prepare data
df_pu <- df_curve %>%
  filter(opportunity_cost != 0) %>%
  mutate(
    opportunity_cost_log = case_when(
      opportunity_cost > 0 ~ log10(opportunity_cost),
      opportunity_cost < 0 ~ -log10(abs(opportunity_cost)),
      opportunity_cost == 0 ~ 0
    )
  )

# Function to assign xmin/xmax per planning unit with directional stacking
process_unit <- function(df) {
  df_pos <- df %>%
    filter(emission_rate >= 0) %>%
    arrange(opportunity_cost_log) %>%
    mutate(
      xmin = lag(cumsum(emission_rate), default = 0),
      xmax = cumsum(emission_rate)
    )
  
  df_neg <- df %>%
    filter(emission_rate < 0) %>%
    arrange(opportunity_cost_log) %>%
    mutate(
      xmax = lag(cumsum(emission_rate), default = 0),
      xmin = cumsum(emission_rate)
    )
  
  bind_rows(df_pos, df_neg) %>%
    mutate(
      hover_text = paste0(
        "Perubahan Lahan: ", land_use_change, "<br>",
        "Opportunity Cost: ", scales::comma(opportunity_cost), "<br>",
        "Laju Emisi: ", scales::comma(emission_rate)
      )
    )
}

# Apply per planning unit
df_pu_processed <- df_pu %>%
  group_split(planning_unit) %>%
  map_df(process_unit)

# Generate plots per planning unit
plots_list <- df_pu_processed %>%
  split(.$planning_unit) %>%
  map(~ {
    p <- ggplot(.x) +
      geom_rect(aes(
        xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost_log,
        fill = land_use_change,
        text = hover_text
      ), color = "black") +
      labs(
        x = "Laju Emisi (ton CO<sub>2</sub>-eq/ha.tahun)",
        y = paste0("Opportunity Cost (", currency, "/ton CO<sub>2</sub>-eq)"),                            
        title = paste("Kurva Abatement Cost -", unique(.x$planning_unit))
      ) +
      scale_y_continuous(
        breaks = function(x) floor(min(x)):ceiling(max(x)),
        labels = function(x) {
          values <- ifelse(x >= 0, 10^x, -10^abs(x))
          ifelse(
            values == floor(values),
            scales::comma(values, accuracy = 1),
            scales::comma(values)
          )
        }
      ) +
      scale_x_continuous(
        limits = function(x) {
          max_abs <- max(abs(c(x[1], x[2])))
          c(min(x), max_abs)
        },
        breaks = function(x) pretty(x, n = 6),
        labels = function(x) scales::comma(x, accuracy = 0.01)
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })

# Show first plot
plots_list[[1]]

