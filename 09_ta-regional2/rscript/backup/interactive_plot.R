PRS_graph <- ggplot(Linkages_table, aes(x = DBL, y = DFL, color = CATEGORY)) + 
  geom_point(shape = 19, size = 4) + 
  geom_hline(aes(yintercept = 1), colour = "#BB0000", linetype = "dashed") + 
  geom_vline(aes(xintercept = 1), colour = "#BB0000", linetype = "dashed") +
  geom_text(aes(label = ID), vjust = -1, hjust = 1, size = 2, color = "black") 

# Create the ggplot
PRS_graph <- ggplot(Linkages_table, aes(x = DBL, y = DFL, color = CATEGORY)) + 
  geom_point(shape = 19, size = 4, aes(text = SECTOR)) +  # Add hover text for ID
  geom_hline(aes(yintercept = 1), colour = "#BB0000", linetype = "dashed") + 
  geom_vline(aes(xintercept = 1), colour = "#BB0000", linetype = "dashed") +
  geom_text(aes(label = ID), vjust = -1, hjust = 1, size = 2, color = "black") 

# Convert to an interactive plotly plot with hover information
interactive_PRS_graph <- ggplotly(PRS_graph, tooltip = "text")

#' Plot the GDP by sector
GDP_graph <- ggplot(data = df_GDP_graph, aes(x = CATEGORY, y = GDP, fill = CATEGORY)) +
  geom_bar(colour = "black", stat = "identity", aes(text = SECTOR)) +  # Add hover text for Sector
  coord_flip() +
  xlab("Category") + 
  ylab("GDP")

# Convert to interactive plotly object with hover information
interactive_GDP_graph <- ggplotly(GDP_graph, tooltip = "text")

data <- data.frame(
  SECTOR = "Oil Refining Industry",
  CATEGORY = "Industry",
  Out.multiplier = 1.173,
  Inc.multiplier = 0.872790910,
  Lab.multiplier = 1.319100e-09
)

# Tidy up Lab.multiplier column by rounding it
data <- data %>%
  mutate(
    Out.multiplier = round(Out.multiplier, 3),
    Inc.multiplier = round(Inc.multiplier, 3),
  )
