#Impact of Land Using to Regional Economy Indicator Analysis
land_req="data/LandReq.Rdata"
sciendo_db = "data/table/luc_projection15.csv"

library(reshape2)
library(ggplot2)
library(raster)
library(foreign)
library(magick)

load(land_req)

#READ INPUT FILE
nodata_val<-0
land.requirement.db<-land.requirement_table
names(landuse_lut) <- as.character(landuse_lut[1,])
landuse_lut <- landuse_lut[-1,]
lc.list<-subset(landuse_lut, select=c(ID, LC))

landuse_area0_freq<-freq(landuse_area0)
landuse_area0_table<-as.data.frame(na.omit(landuse_area0_freq))
colnames(landuse_area0_table)<-c("ID", "COUNT")
landuse_area0<-as.matrix(landuse_area0_table$COUNT)

next_data_luc <- read.csv(sciendo_db)
landuse_area_table <- as.data.frame(na.omit(next_data_luc))
landuse_area_table$ID_LC <- NULL
landuse_area_table$LC <- NULL
landuse_area <- landuse_area_table

landuse_table<-merge(lc.list, landuse_area0_table, by="ID")
landuse_table<-cbind(landuse_table, landuse_area)
landuse_table$LC<-NULL
colnames(landuse_table)[1] <- "LAND_USE"
n_periods <- ncol(landuse_table) - 1
sim_periods <- ncol(landuse_table) - 2
for (i in 1:n_periods) {
  colnames(landuse_table)[i + 1] <- paste0("T", i, "_HA")
}
for (i in 2:n_periods) {
  change_col_name <- paste0("CHANGE_T", i-1, "_T", i)
  landuse_table[[change_col_name]] <- landuse_table[[paste0("T", i, "_HA")]] - landuse_table[[paste0("T", i-1, "_HA")]]
}

# Reshape the data for ggplot
landuse_melted <- melt(landuse_table, id.vars = "LAND_USE", 
                       measure.vars = grep("CHANGE_T", colnames(landuse_table), value = TRUE), 
                       variable.name = "Period", value.name = "Change")

LC_graph <- ggplot(data = landuse_melted, aes(x = LAND_USE, y = Change, fill = Period)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge") +
  guides(fill = guide_legend(title = "Period")) +  # Add a legend for periods
  xlab("Land Use") + 
  ylab("Change in Area (ha)") + 
  ggtitle("Land Use Change Over Time") +
  theme(axis.text.x = element_text(angle = 90, size = 6))

# Initialize results storage
GDP_summary_list <- list()
GDP_graph_list <- list()
GDP_overall_list <- list()

# Loop through each period and perform the calculations
for (t in 1:sim_periods) {

  # Create the diagonal matrix for the current period
  landuse_area_diag <- diag(as.numeric(as.matrix(landuse_area[, t])))
  
  # MODEL FINAL DEMAND for each period
  land.distribution.scen <- land.distribution.prop %*% landuse_area_diag
  land.requirement.scen <- rowSums(land.distribution.scen)
  
  fin_dem.rtot <- rowSums(fin_dem[, t, drop = FALSE])
  int_con.rtot <- rowSums(int_con[, t, drop = FALSE])
  
  demand <- fin_dem.rtot + int_con.rtot
  
  land.requirement.coeff <- land.requirement.db$LRC
  land.productivity.coeff <- land.requirement.db$LPC
  
  fin_dem.scen <- land.requirement.scen / land.productivity.coeff
  fin_dem.scen[is.infinite(fin_dem.scen)] <- 0
  fin_dem.scen[is.na(fin_dem.scen)] <- 0
  
  # CALCULATE FINAL DEMAND AND GDP FROM SCENARIO OF LAND USE CHANGE
  fin.output.scen <- Leontief %*% fin_dem.scen
  fin.output.scen <- round(fin.output.scen, digits = 1)
  
  # Check if fin.output.scen is a matrix or dataframe and convert if necessary
  fin.output.scen <- as.data.frame(fin.output.scen)
  fin.output.scen <- sapply(fin.output.scen, as.numeric)
  
  if (is.list(fin.output.scen)) {
    stop("fin.output.scen is still a list. Ensure that it's numeric.")
  }
  
  colnames(fin.output.scen)[1] <- "OUTPUT_Scen"
  
  GDP.prop.from.output <- GDP.val / demand
  GDP.prop.from.output[is.na(GDP.prop.from.output)] <- 0
  
  GDP.scen <- GDP.prop.from.output * as.numeric(fin.output.scen)
  GDP.scen[sapply(GDP.scen, is.infinite)] <- NA
  GDP.scen[is.na(GDP.scen)] <- 0
  GDP.scen <- round(GDP.scen, digits = 1)
  GDP.scen <- as.data.frame(GDP.scen)
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
  
  # CALCULATE TOTAL GDP
  GDP_tot_scen <- colSums(as.matrix(GDP_summary$GDP_scen), na.rm = TRUE)
  GDP_tot_diff <- GDP_tot_scen - GDP_tot
  GDP_tot_rate <- GDP_tot_diff / GDP_tot
  
  GDP_overall <- cbind(
    rbind("Total GDP", "Scenario GDP", "GDP difference", "Rate of difference"),
    rbind(GDP_tot, GDP_tot_scen, GDP_tot_diff, GDP_tot_rate)
  )
  
  # Store the GDP summary and overall results for this period
  GDP_summary_list[[t]] <- GDP_summary
  GDP_overall_list[[t]] <- GDP_overall
  
  # Generate a GDP graph for this period
  order_GDP_scen <- as.data.frame(GDP_summary[order(-GDP_summary$GDP_scen), ])
  order_GDP_scen10 <- head(order_GDP_scen, n = 20)
  
  GDP_summary_melt <- melt(data = order_GDP_scen10, id.vars = c('SECTOR'), measure.vars = c('GDP', 'GDP_scen'))
  GDP_graph <- ggplot(data = GDP_summary_melt, aes(x = SECTOR, y = value, fill = variable)) +
    geom_bar(colour = "black", stat = "identity", position = "dodge") +
    guides(fill = "none") + xlab("Sectors") + ylab("GDP") +
    ggtitle(paste("Comparison of GDP Baseline and Scenario - Period", t)) +
    theme(axis.text.x = element_text(angle = 90, size = 6))
  
    GDP_graph_list[[t]] <- GDP_graph
}

#CALCULATE TOTAL LABOUR
Labour_table<-Lab.multiplier
Labour_table$Lab.multiplier<-as.numeric(format(Labour_table$Lab.multiplier, digits=3, width=5))
Labour_table<-cbind(Labour_table,fin.output.scen)
Labour_table<-cbind(Labour_table,labour)
colnames(Labour_table)[1] <- "SECTOR"
colnames(Labour_table)[2] <- "CATEGORY"
colnames(Labour_table)[4] <- "OUT_scen"
colnames(Labour_table)[5] <- "Lab_base"
test<-Labour_table$Lab.multiplier*Labour_table$OUT_scen*1000000
test<-round(test, digits=0)
Labour_table$Lab_scen<-test
Labour_table$Lab_req<-Labour_table$Lab_scen-Labour_table$Lab_base
test2<-Labour_table$Lab_req
test2<-cbind(sector, test2)
LAB_graph<-ggplot(data=test2, aes(x=SECTOR, y=test2, fill=CATEGORY)) +
  geom_bar(colour="black", stat="identity", position="dodge")+
  guides(fill=FALSE) + xlab("Sector") + ylab("Labour requirement") +ggtitle("Impact of LU Change to Labour")+ theme(axis.text.x  = element_text(angle=90, size=6))