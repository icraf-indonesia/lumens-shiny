land_req="C:/Users/ykarimah/Downloads/0 Weekend/regeco1-test04/LandRequirement_db.Rdata"
sciendo_db = "data/table/luc_projection.csv"

load(land_req)

# Load land use requirement data
land.requirement.db <- land.requirement_table
# names(landuse_lut) <- as.character(landuse_lut[1,])
# landuse_lut <- landuse_lut[-1,]
lc.list <- subset(landuse_lut, select = c(ID, LC))

# Prepare data for land use change analysis
next_data_luc <- read.csv(sciendo_db)
landuse_area_table <- as.data.frame(na.omit(next_data_luc))
landuse_area_table$LC <- NULL
landuse_area <- landuse_area_table

# Merge land use data for all periods
landuse_bau_table <- merge(lc.list, landuse_area0_table, by = "ID")
landuse_table <- left_join(landuse_bau_table, landuse_area, by = "ID")
landuse_table$LC <- NULL
colnames(landuse_table)[1] <- "LAND_USE"

# Generate new columns for each time period and calculate land use change
n_periods <- ncol(landuse_table) - 1
for (i in 1:n_periods) {
  colnames(landuse_table)[i + 1] <- paste0("T", i, "_HA")
}
# Initialize storage for summary outputs per period if needed
GDP_summary_list <- list()
GDP_overall_list <- list()

for (i in 2:ncol(landuse_area)) {
  # Create a diagonal matrix for the current period's land use area
  landuse_area_diag <- diag(as.numeric(as.matrix(landuse_area[i])))
  landuse_area_diag <- diag(as.numeric(as.matrix(landuse_area[2])))
  
  # MODEL FINAL DEMAND
  land.distribution.scen <- land.distribution.prop %*% landuse_area_diag
  land.requirement.scen <- rowSums(land.distribution.scen)
  fin_dem.rtot <- rowSums(fin_dem)
  int_con.rtot <- rowSums(int_con)
  demand <- fin_dem.rtot + int_con.rtot
  land.requirement.coeff <- land.requirement.db$LRC
  land.productivity.coeff <- land.requirement.db$LPC
  fin_dem.scen <- land.requirement.scen / land.productivity.coeff
  fin_dem.scen[is.infinite(fin_dem.scen)] <- 0
  fin_dem.scen[is.na(fin_dem.scen)] <- 0
  
  # CALCULATE FINAL DEMAND AND GDP FROM SCENARIO OF LAND USE CHANGE
  fin.output.scen <- Leontief %*% fin_dem.scen
  fin.output.scen <- round(fin.output.scen, digits = 1)
  colnames(fin.output.scen)[1] <- "OUTPUT_Scen"
  GDP.prop.from.output <- GDP.val / demand
  GDP.prop.from.output[is.na(GDP.prop.from.output)] <- 0
  GDP.scen <- GDP.prop.from.output * fin.output.scen
  GDP.scen <- round(GDP.scen, digits = 1)
  GDP.scen[is.na(GDP.scen)] <- 0
  colnames(GDP.scen)[1] <- "GDP_scen"
  GDP.diff <- GDP.scen - GDP$GDP
  GDP.diff <- round(GDP.diff, digits = 1)
  colnames(GDP.diff)[1] <- "GDP_diff"
  GDP.rate <- GDP.diff / GDP.val
  GDP.rate[is.na(GDP.rate)] <- 0
  GDP.rate <- round(GDP.rate, digits = 2)
  colnames(GDP.rate)[1] <- "GDP_rate"
  GDP_summary <- cbind(GDP, GDP.scen, fin.output.scen, GDP.diff, GDP.rate)
  GDP_summary$P_OUTPUT <- NULL
  GDP_summary$P_GDP <- NULL
  
  # Calculate total GDP
  GDP_tot_scen <- as.matrix(GDP_summary$GDP_scen)
  GDP_tot_scen <- colSums(GDP_tot_scen)
  GDP_tot_diff <- GDP_tot_scen - GDP_tot
  GDP_tot_rate <- GDP_tot_diff / GDP_tot
  text1 <- "Total GDP"
  text2 <- "Scenario GDP"
  text3 <- "GDP difference"
  text4 <- "Rate of difference"
  GDP_overall1 <- rbind(text1, text2, text3, text4)
  GDP_overall2 <- rbind(GDP_tot, GDP_tot_scen, GDP_tot_diff, GDP_tot_rate)
  GDP_overall <- cbind(GDP_overall1, GDP_overall2)
  
  # Store results for each period if needed
  GDP_summary_list[[i]] <- GDP_summary
  GDP_overall_list[[i]] <- GDP_overall
}

# Extract GDP.scen column from each GDP_summary in GDP_summary_list
GDP_scen_list <- lapply(GDP_summary_list, function(x) x$GDP_scen)
GDP_scen_df <- data.frame(do.call(cbind, GDP_scen_list))
colnames(GDP_scen_df) <- paste0("GDP_scen_Period_", seq_along(GDP_scen_df))

#' Combine results into data frames for plotting and comparison
GDP_scen <- data.frame(
  Sector = GDP$SECTOR, 
  Category = GDP$CATEGORY,
  GDP_bau = GDP$GDP,                 
  GDP_scen_df
)

rv$GDP_scen <- GDP_scen

#' Create bar charts to visualize total GDP
GDP_totals_df <- data.frame(
  Period = paste("Period", seq_len(length(GDP_scen_df))),
  GDP_totals = colSums(GDP_scen_df)
)
GDP_totals_df <- rbind(data.frame(Period = "BAU", GDP_totals = GDP_tot), GDP_totals_df)
GDP_totals_graph <- ggplot(data = GDP_totals_df, aes(x = Period, y = GDP_totals)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(x = "Period", y = "GDP Total") +
  ggtitle(paste("BAU vs Scenario GDP Total")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
rv$GDP_totals_graph <- GDP_totals_graph

#' Remove unwanted columns from GDP_scen_df
db_GDP <- GDP_scen[, !(colnames(GDP_scen) %in% c("Sector", "Category"))]
rownames(db_GDP) <- NULL

#' Create bar charts to visualize total Output, Income, and Labour
db_output_total <- create_totals_df(GDP, db_GDP, 1, "Period BAU")
output_total_graph <- create_bar_plot(db_output_total, "BAU vs Scenario Output Total")

db_income_total <- create_totals_df(GDP, db_GDP, Inc.multiplier$Inc.multiplier, "Period BAU")
income_total_graph <- create_bar_plot(db_income_total, "BAU vs Scenario Income Total")

db_labour_total <- create_totals_df(GDP, db_GDP, Lab.multiplier$Lab.multiplier, "Period BAU")
labour_total_graph <- create_bar_plot(db_labour_total, "BAU vs Scenario Labour Total")
