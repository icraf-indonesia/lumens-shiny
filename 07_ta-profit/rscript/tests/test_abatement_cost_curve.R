library(ggplot2)
library(dplyr)
strategies <- c("Residential lighting", "Car fuel economy", "New buildings efficiency",
                "Industrial process improvements", "Powerplant efficiency", "Reforestation",
                "Residential insulation", "Wind", "Solar", "Biomass cofiring", "Hybrid cars")

cost <- c(-80, -60, -50, -40, -30, -20, -10, 20, 40, 60, 80)  # $/MTCO2e
reduction <- c(150, 120, 100, 90, 80, 70, 60, 50, 40, 30, 20)  # Million MTCO2e

data <- data.frame(
  strategy = factor(strategies, levels = strategies),
  cost = cost,
  reduction = reduction
)

ggplot(data, aes(x = strategy, y = cost, fill = strategy)) +
  geom_bar(stat = "identity", width = data$reduction / max(data$reduction)) +
  coord_flip() +
  labs(
    title = "Estimate of Cost Effectiveness of Select GHG Emissions Reductions Strategies in the U.S.",
    x = "Strategy",
    y = "Cost per GHG Reduction ($/MTCO₂e)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

######################################################################

# Sample data
df <- data.frame(
  option = c("residential lighting", "car fuel economy", "new buildings efficiency",
             "industrial process improvements", "powerplant efficiency", 
             "residential insulation", "reforestation", "wind", "solar", "biomass cofiring", "hybrid cars"),
  reduction = c(150, 130, 60, 50, 70, 30, 50, 80, 70, 60, 120),
  cost = c(-80, -60, -10, -5, 0, 10, 20, 25, 30, 40, 90)
)

# Calculate cumulative position for stacking horizontally
df <- opcost_curve_table %>%
  mutate(xmin = lag(cumsum(emission_rate), default = 0),
         xmax = cumsum(emission_rate))

# Plot
a <- ggplot(df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost, fill = land_use_change), color = "black") +
  geom_text(aes(x = (xmin + xmax) / 2, y = opportunity_cost + ifelse(opportunity_cost > 0, 5, -5), label = land_use_change), size = 1) +
  labs(
    x = "Emission Rate (ton CO2e/year)",
    y = "Opportunity Cost (currency/ton CO2e)",
    title = "Abatement Cost Curve"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(a)

######################################################################

library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

df <- opcost_curve_table %>%
  mutate(
    xmin = lag(cumsum(emission_rate), default = 0),
    xmax = cumsum(emission_rate),
    label_wrapped = str_wrap(land_use_change, width = 10),  # Wrap long labels
    hover_text = paste0(
      "Land Use Change: ", land_use_change, "<br>",
      "Opportunity Cost: ", round(opportunity_cost, 2), "<br>",
      "Emission Rate: ", round(emission_rate, 2)
    )
  )

p <- ggplot(df) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax, ymin = 0, ymax = opportunity_cost,
    fill = land_use_change,
    text = hover_text   # << important for ggplotly tooltip
  ), color = "black") +
  # geom_text(aes(
  #   x = (xmin + xmax) / 2,
  #   y = opportunity_cost + ifelse(opportunity_cost > 0, 5, -5),
  #   label = label_wrapped
  # ), size = 1) +
  labs(
    x = "Emission Rate (ton CO2e/year)",
    y = "Opportunity Cost (currency/ton CO2e)",
    title = "Abatement Cost Curve"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(p, tooltip = "text")
