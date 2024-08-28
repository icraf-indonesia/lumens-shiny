#Regional Economy Land Distribution and Requirement Analysis

land_use="data/raster/tutupan_lahan_Bungo_2010r.tif"
land.distribution_file="data/regeco/land_distribution_15.csv"

library(reshape2)
library(ggplot2)
library(raster)
library(foreign)
library(magick)
library(terra)

nodata_val<-0 # still hardcode (!)
land.distribution <- read.csv(land.distribution_file, header = FALSE)
int_con.m<-as.matrix(int_con)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_con.m)

#GDP
GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
GDP.val.m<-as.matrix(GDP.val)
GDP.val.m<-as.numeric(GDP.val.m)
OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
OUTPUT.val.m<-as.matrix(OUTPUT.val)
OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
GDP<-cbind(sector,GDP.val,OUTPUT.val)
colnames(GDP)[1] <- "SECTOR"
colnames(GDP)[2] <- "SECTOR CLASS"
colnames(GDP)[3] <- "GDP"
colnames(GDP)[4] <- "OUTPUT"
GDP$GDP_PROP<-GDP$GDP/GDP$OUTPUT
GDP[is.na(GDP)]<-0
order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
order_GDP10<-head(order_GDP,n=20)
GDP_tot<-as.matrix(GDP$GDP)
GDP_tot<-colSums(GDP_tot)

#LINK LAND DISTRIBUTION FILE WITH LAND USE MAP
#Read land use map and calculate area of land use distribution matrix
lc_map<-raster(land_use)
lc_freq<-freq(lc_map)
colnames(lc_freq)<-c("ID", "COUNT")
landuse_area<-as.data.frame(na.omit(lc_freq))
landuse_area<-as.matrix(landuse_area$COUNT)
land.distribution_t<-as.matrix(land.distribution)
landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area)))
land.distribution.val<-land.distribution_t %*% landuse_area_diag

#CALCULATE LAND DISTRIBUTION COEFFICIENT MATRIX
land.distribution.ctot<-colSums(land.distribution.val)
land.distribution.rtot<-rowSums(land.distribution.val)
land.distribution.prop<-land.distribution.val %*% diag(1/land.distribution.ctot)
land.distribution.prop[is.na(land.distribution.prop)]<-0
land.distribution.prop.r<-t(land.distribution.val) %*% diag(1/land.distribution.rtot)
land.distribution.prop.r[is.na(land.distribution.prop.r)]<-0
land.requirement<-rowSums(land.distribution.val)
fin_dem.rtot<-rowSums(fin_dem)
int_con.rtot<-rowSums(int_con)
demand<-fin_dem.rtot+int_con.rtot
land.requirement.coeff<-land.requirement/demand
land.requirement.coeff[is.infinite(land.requirement.coeff)]<-0
land.productivity.coeff<-land.requirement/fin_dem.rtot
land.productivity.coeff[is.infinite(land.productivity.coeff)]<-0

#PRODUCE OUTPUT
land.requirement_table<-as.data.frame(land.requirement)
land.requirement.tot<-sum(land.requirement)
land.requirement_table_prop<-as.data.frame(land.requirement/land.requirement.tot)
land.requirement_table<-cbind(sector,land.requirement_table,land.requirement_table_prop,demand,fin_dem.rtot,land.requirement.coeff, land.productivity.coeff)
colnames(land.requirement_table)[1] <- "SECTOR"
colnames(land.requirement_table)[2] <- "CATEGORY"
colnames(land.requirement_table)[3] <- "LR"
colnames(land.requirement_table)[4] <- "LR_PROP"
colnames(land.requirement_table)[5] <- "OUTPUT"
colnames(land.requirement_table)[6] <- "DEMAND"
colnames(land.requirement_table)[7] <- "LRC"
colnames(land.requirement_table)[8] <- "LPC"
land.requirement_table$LR<-round(land.requirement_table$LR)
land.requirement_table$LR_PROP<-round(land.requirement_table$LR_PROP, digits=2)
land.requirement_table$OUTPUT<-round(land.requirement_table$OUTPUT)
land.requirement_table$DEMAND<-round(land.requirement_table$DEMAND)
land.requirement_table$LRC<-round(land.requirement_table$LRC, digits=2)
land.requirement_table$LPC<-round(land.requirement_table$LPC, digits=2)

#land.requirement_table$LRC<-round(land.requirement_table$LRC, digits=2)-->catasthropic error !!!!
order_land.requirement <- as.data.frame(land.requirement_table[order(-land.requirement_table$LRC),])
order_land.requirement <-head(order_land.requirement,n=20)
LRC_graph<-ggplot(data=order_land.requirement, aes(x=SECTOR, y=LRC, fill=CATEGORY)) +
  geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Land requirement coefficient") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
         axis.text.x = element_text(size = 8))

#EXPORT OUTPUT
land_distribution_file<-"Land_distribution_matrix.dbf"
# write.dbf(land.distribution.prop, land_distribution_file,  factor2char = TRUE, max_nchar = 254)
land_requirement_file<-"Land_requirement_coefficient.dbf"
# write.dbf(land.requirement_table, land_requirement_file, factor2char = TRUE, max_nchar = 254)