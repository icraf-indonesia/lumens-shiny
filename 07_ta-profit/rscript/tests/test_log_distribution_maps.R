plotly::plot_ly(
  data = main_dissolved_lc1,
  x = ~stringr::str_wrap(LC1, width = 25),
  y = ~Total_NPV1,
  type = "bar",
  hoverinfo = "text",
  hovertext = ~paste(
    "Land Cover Class (LC 1):", LC1, "<br>", 
    "Total NPV:", format(Total_NPV1, big.mark = ",", scientific = FALSE)
  ),
  marker = list(
    color = ~Total_NPV1,
    colorscale = "Viridis",
    showscale = FALSE
  )
) %>%
  plotly::layout(
    title = title,
    xaxis = list(title = "", categoryorder = "total descending", tickangle = -270),
    yaxis = list(title = "Total NPV", type="log"),
    margin = list(b = 150),
    hoverlabel = list(bgcolor = "white", font = list(color = "black"))
  )


npv1_map <- ggplot() +
  tidyterra::geom_spatraster(data = npv1_map) +
  scale_fill_gradientn(
    colours = c("yellow", "darkgreen"),
    trans = "log10",
    breaks = c(1e2, 1e3, 1e4, 1e5),  # customize tick marks
    labels = scales::comma,
    na.value = "white",
    name = "NPV"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.position = "right",
    legend.justification = c(0, 0.5)
  )

library(mapview)

# extract values (ignore NA)
vals <- values(deltaNPV_map)
vals <- vals[!is.na(vals)]

# define log10 breaks, symmetric around 0
max_val <- max(abs(vals))
log_breaks <- unique(c(
  -rev(10^(0:floor(log10(max_val)))),  # negative side
  0,
  10^(0:floor(log10(max_val)))         # positive side
))

# make sure breaks cover range
log_breaks <- log_breaks[log_breaks >= min(vals) & log_breaks <= max(vals)]

# define diverging colors (red → white → green)
pal <- colorRampPalette(c("red", "white", "darkgreen"))

mapview(
  deltaNPV_map,
  maxpixels = ncell(deltaNPV_map),
  col.regions = pal(length(log_breaks) - 1),
  at = log_breaks
) 


#####
vals <- values(npv1_map)
vals <- vals[!is.na(vals)]

# define log10 breaks, symmetric around 0
max_val <- max(abs(vals))
log_breaks <- unique(c(
  0,
  10^(0:floor(log10(max_val)))
))

log_breaks <- c(0, 10^(0:floor(log10(max_val))))
log_breaks <- sort(unique(c(log_breaks, log_breaks * 2)))  # add 5× values
log_breaks <- log_breaks[log_breaks <= max_val]

mapview(
  npv1_map,
  maxpixels = ncell(npv1_map),
  na.color = "transparent",
  layer.name = "NPV",
  col.regions = colorRampPalette(c("white","yellow","orange","darkgreen")),
  at = log_breaks
)


deltaNPV_map_mv <- deltaNPV_map

# inspect actual value range
range(values(deltaNPV_map_mv), na.rm = TRUE)

# get max abs value from raster
val_max <- max(abs(values(deltaNPV_map_mv)), na.rm = TRUE)

# symmetric domain
val_range <- c(-val_max, val_max)

# diverging palette: red → white → green
pal <- leaflet::colorNumeric(
  palette  = colorRampPalette(c("red", "white", "darkgreen"))(256),
  domain   = val_range,
  na.color = "transparent"
)

format_million <- function(x) {
  ifelse(is.na(x), NA, paste0(formatC(x, big.mark = ",", format = "f", digits = 2), " M"))
}

# plot with continuous legend
mapview(
  deltaNPV_map_mv,
  col.regions = pal,
  na.color = "transparent",
  layer.name = "ΔNPV",
  legend = TRUE
)


deltaNPV_map_mv <- deltaNPV_map

# Get value range
vals <- values(deltaNPV_map_mv)
vals <- vals[!is.na(vals)]
min_val <- min(vals)
max_val <- max(vals)

# Create more breakpoints for smoother gradient
n_breaks <- 100
breaks <- seq(min_val, max_val, length.out = n_breaks)

# Create color palette with white at 0
# Find the position where breaks cross 0
zero_pos <- which.min(abs(breaks))
colors <- colorRampPalette(c("red", "white", "blue"))(n_breaks)

mapview(
  deltaNPV_map_mv,
  maxpixels = ncell(deltaNPV_map),
  na.color = "transparent",
  layer.name = "ΔNPV",
  at = breaks,
  col.regions = colors
)

deltaNPV_map_mv <- deltaNPV_map

# Get the range of values to set appropriate breakpoints
vals <- values(deltaNPV_map_mv)
vals <- vals[!is.na(vals)]  # Remove NA values
min_val <- min(vals)
max_val <- max(vals)

# Create breakpoints that include 0 in the middle
breaks <- c(min_val, -1e-10, 1e-10, max_val)  # Using small values around 0

# Define colors with white at 0
colors <- c("red", "lightgrey", "green")  # Negative values: red, 0: white, Positive values: blue

mapview(
  deltaNPV_map_mv,
  maxpixels = ncell(deltaNPV_map),
  na.color = "transparent",
  layer.name = "ΔNPV",
  at = breaks,
  col.regions = colors
)


vals <- values(deltaNPV_map)
vals <- vals[!is.na(vals)]
min_val <- min(vals)
max_val <- max(vals)

# Use pretty breaks but ensure 0 is included and boundaries are exact
main_breaks <- pretty(c(min_val, max_val), n = 8)
breaks <- sort(unique(c(min_val, main_breaks, 0, max_val)))
breaks <- breaks[breaks >= min_val & breaks <= max_val]

mapview(
  npv1_map,
  maxpixels = ncell(npv1_map),
  na.color = "transparent",
  layer.name = "NPV",
  col.regions = colorRampPalette(c("white","yellow","orange","darkgreen")),
  at = breaks
)