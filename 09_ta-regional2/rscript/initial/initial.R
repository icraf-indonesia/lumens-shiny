#Impact of Land Using to Regional Economy Indicator Analysis
land_req="data/LandReq.Rdata"
projected_land_use="data/raster/tutupan_lahan_Bungo_2010r.tif"
# sciendo_db = "data/table/luc_projection15.csv"

library(reshape2)
library(ggplot2)
library(raster)
library(foreign)
library(magick)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

load(land_req)
# landuse_lut=read_csv(landuse_lut)

#READ INPUT FILE
nodata_val<-0
land.requirement.db<-land.requirement_table
names(landuse_lut) <- as.character(landuse_lut[1,])
landuse_lut <- landuse_lut[-1,]
lc.list<-subset(landuse_lut, select=c(ID, LC))

#Read land use map and calculate area of land use
next_data_luc<-raster(projected_land_use)
next_luc_freq<-freq(next_data_luc)
landuse_area_table<-as.data.frame(na.omit(next_luc_freq))
colnames(landuse_area_table)<-c("ID", "COUNT")
landuse_area<-as.matrix(landuse_area_table$COUNT)

next_data_luc<-read.csv(sciendo_db)
landuse_area_table<-as.data.frame(na.omit(next_data_luc))
landuse_area_table$ID_LC<-NULL
landuse_area_table$LC<-NULL
landuse_area <- landuse_area_table

# landuse_area_map<-raster("C:/Users/ykarimah/Downloads/Result/landuse_area0.tif")
# landuse_area0_freq<-freq(landuse_area_map)
landuse_area0_freq<-freq(landuse_area0)
landuse_area0_table<-as.data.frame(na.omit(landuse_area0_freq))
colnames(landuse_area0_table)<-c("ID", "COUNT")
landuse_area0<-as.matrix(landuse_area0_table$COUNT)

landuse_table<-merge(lc.list, landuse_area0_table, by="ID")
landuse_table<-cbind(landuse_table, landuse_area)
landuse_table$LC<-NULL
# landuse_table<-cbind(lc.list,landuse_area0, landuse_area)
landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area)))
# colnames(landuse_table)[1] <- "LAND_USE"
# colnames(landuse_table)[2] <- "T1_HA"
# colnames(landuse_table)[3] <- "T2_HA"
# # landuse_table<-edit(landuse_table)
# landuse_table$CHANGE<-landuse_table$T2_HA-landuse_table$T1_HA

# Rename the first column to "LAND_USE"
colnames(landuse_table)[1] <- "LAND_USE"

# Loop through the period columns (assuming the periods start from the 2nd column onwards)
n_periods <- ncol(landuse_table) - 1  # Number of periods (subtracting 1 for the "LAND_USE" column)

# Rename each period column dynamically
for (i in 1:n_periods) {
  colnames(landuse_table)[i + 1] <- paste0("T", i, "_HA")
}

# Loop through each period and calculate changes between consecutive periods
for (i in 2:n_periods) {
  change_col_name <- paste0("CHANGE_T", i-1, "_T", i)  # Create dynamic column name for change
  landuse_table[[change_col_name]] <- landuse_table[[paste0("T", i, "_HA")]] - landuse_table[[paste0("T", i-1, "_HA")]]
}

#MODEL FINAL DEMAND
land.distribution.scen<-land.distribution.prop %*% landuse_area_diag
land.requirement.scen<-rowSums(land.distribution.scen)
fin_dem.rtot<-rowSums(fin_dem)
int_con.rtot<-rowSums(int_con)
demand<-fin_dem.rtot+int_con.rtot
land.requirement.coeff<-land.requirement.db$LRC
land.productivity.coeff<-land.requirement.db$LPC
fin_dem.scen<-land.requirement.scen/land.productivity.coeff
fin_dem.scen[is.infinite(fin_dem.scen)]<-0
fin_dem.scen[is.na(fin_dem.scen)]<-0

#CALCULATE FINAL DEMAND AND GDP FROM SCENARIO OF LAND USE CHANGE
fin.output.scen<-Leontief %*% fin_dem.scen
fin.output.scen<-round(fin.output.scen, digits=1)
colnames(fin.output.scen)[1]<-"OUTPUT_Scen"
GDP.prop.from.output<-GDP.val/demand
GDP.prop.from.output[is.na(GDP.prop.from.output)]<-0
GDP.scen<-GDP.prop.from.output*fin.output.scen
GDP.scen<-round(GDP.scen, digits=1)
GDP.scen[is.na(GDP.scen)]<-0
colnames(GDP.scen)[1] <- "GDP_scen"
GDP.diff<-GDP.scen-GDP$GDP
GDP.diff<-round(GDP.diff, digits=1)
colnames(GDP.diff)[1] <- "GDP_diff"
GDP.rate<-GDP.diff/GDP.val
GDP.rate[is.na(GDP.rate)]<-0
GDP.rate<-round(GDP.rate, digits=2)
colnames(GDP.rate)[1] <- "GDP_rate"
GDP_summary<-cbind(GDP,GDP.scen,fin.output.scen,GDP.diff, GDP.rate)
GDP_summary$P_OUTPUT<-NULL
GDP_summary$P_GDP<-NULL

#CALCULATE TOTAL GDP
GDP_tot_scen<-as.matrix(GDP_summary$GDP_scen)
GDP_tot_scen<-colSums(GDP_tot_scen)
GDP_tot_diff<-GDP_tot_scen-GDP_tot
GDP_tot_rate<-GDP_tot_diff/GDP_tot
text1<-"Total GDP"
text2<-"Scenario GDP"
text3<-"GDP difference"
text4<-"Rate of difference"
GDP_overall1<-rbind(text1,text2,text3,text4)
GDP_overall2<-rbind(GDP_tot, GDP_tot_scen,GDP_tot_diff,GDP_tot_rate)
GDP_overall<-cbind(GDP_overall1,GDP_overall2)

order_GDP_scen <- as.data.frame(GDP_summary[order(-GDP_summary$GDP_scen),])
order_GDP_scen10<-head(order_GDP_scen,n=20)
GDP_summary.melt <- melt(data = order_GDP_scen10, id.vars=c('SECTOR'), measure.vars=c('GDP','GDP_scen'))
GDP_graph<-ggplot(data=GDP_summary.melt, aes(x=SECTOR, y=value, fill=variable)) +
  geom_bar(colour="black", stat="identity", position="dodge")+
  guides(fill=FALSE) + xlab("Sectors") + ylab("GDP") +ggtitle("Comparison of GDP Baseline and Scenario")+ theme(axis.text.x  = element_text(angle=90, size=6))

LC_graph<-ggplot(data=landuse_table, aes(x=LAND_USE, y=CHANGE)) +
  geom_bar(colour="black", stat="identity", position="dodge")+
  guides(fill=FALSE) + xlab("Land use") + ylab("Change") +ggtitle("Land Use Change")+ theme(axis.text.x  = element_text(angle=90, size=6))

# Reshape the data for ggplot
landuse_melted <- melt(landuse_table, id.vars = "LAND_USE", 
                       measure.vars = grep("CHANGE_T", colnames(landuse_table), value = TRUE), 
                       variable.name = "Period", value.name = "Change")

# Create the bar chart
LC_graph <- ggplot(data = landuse_melted, aes(x = LAND_USE, y = Change, fill = Period)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge") +
  guides(fill = guide_legend(title = "Period")) +  # Add a legend for periods
  xlab("Land Use") + 
  ylab("Change in Area (ha)") + 
  ggtitle("Land Use Change Over Time") +
  theme(axis.text.x = element_text(angle = 90, size = 6))

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

addParagraph(rtffile, "\\b\\fs20 Table 1. Land use change\\b0\\fs20.")
addTable(rtffile,landuse_table,font.size=8)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,LC_graph)
addParagraph(rtffile, "\\b\\fs20 Table 2. Impact of land use change to GDP\\b0\\fs20.")
addTable(rtffile,GDP_summary,font.size=6)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,GDP_graph)
addParagraph(rtffile, "\\b\\fs20 Table 3. Impact of land use change to labour requirement\\b0\\fs20.")
addTable(rtffile,Labour_table,font.size=6)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,LAB_graph)
