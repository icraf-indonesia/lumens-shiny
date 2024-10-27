int_con <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/02_intermediate demand.csv", header = FALSE)
add_val <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/06_added value.csv", header = FALSE)
fin_dem <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/04_final demand.csv", header = FALSE)
fin_dem_struc <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/03_final demand comp.csv", header = FALSE)
add_val_struc <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/05_added value comp.csv", header = FALSE)
sector <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/01_sector.csv", header = FALSE)
labour <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/07_labour.csv", header = FALSE)

int_con.m<-as.matrix(int_con)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_con.m)

#CALCULATE INVERS LEONTIEF
int_con.ctot<-colSums(int_con.m)
add_val.ctot<-colSums(add_val.m)
fin_con<- 1/(int_con.ctot+add_val.ctot)
fin_con[is.infinite(fin_con)]<-0
t.input.invers<-diag(fin_con)
A<-int_con.m %*% t.input.invers
I<-as.matrix(diag(dim))
I_A<-I-A
Leontief<-solve(I_A)

#DIRECT BACKWARD LINKAGES
DBL<-colSums(Leontief)
DBL<-DBL/(mean(DBL))
DBL<-cbind(sector,DBL)
colnames(DBL)[3] <- "DBL"
order_DBL <- as.data.frame(DBL[order(-DBL$DBL),])
order_DBL10<-head(order_DBL,n=20)
colnames(order_DBL10)[1] <- "SECTOR"
colnames(order_DBL10)[2] <- "CATEGORY"
BPD_graph<-ggplot(data=order_DBL10, aes(x=SECTOR, y=DBL, fill=CATEGORY)) + 
  geom_bar(colour="black", stat="identity")+ coord_flip() +  
  guides(fill=FALSE) + xlab("Sectors") + ylab("Value") 

#DIRECT FORWARD LINKAGES
DFL<-rowSums(Leontief)
DFL<-DFL/(mean(DFL))
DFL<-cbind(sector,DFL)
colnames(DFL)[3] <- "DFL"
order_DFL <- as.data.frame(DFL[order(-DFL$DFL),])
order_DFL10<-head(order_DFL,n=20)
colnames(order_DFL10)[1] <- "SECTOR"
colnames(order_DFL10)[2] <- "CATEGORY"
FPD_graph<-ggplot(data=order_DFL10, aes(x=SECTOR, y=DFL, fill=CATEGORY)) + 
  geom_bar(colour="black", stat="identity")+ coord_flip() +  
  guides(fill=FALSE) + xlab("Sectors") + ylab("Value") 

#CREATE LINKAGES TABLE
DBL_temp<-colSums(Leontief)
BPD_temp<-DBL_temp/(mean(as.matrix(DBL_temp)))
DFL_temp<-rowSums(Leontief)
FPD_temp<-DFL_temp/(mean(as.matrix(DFL_temp)))
DBL_temp<-as.data.frame(round(DBL_temp, digits=2))
BPD_temp<-as.data.frame(round(BPD_temp, digits=2))
DFL_temp<-as.data.frame(round(DFL_temp, digits=2))
FPD_temp<-as.data.frame(round(FPD_temp, digits=2))
Linkages_table<-cbind(sector,DBL_temp,DFL_temp,BPD_temp,FPD_temp)
colnames(Linkages_table)[1] <- "SECTOR"
colnames(Linkages_table)[2] <- "CATEGORY"
colnames(Linkages_table)[3] <- "DBL"
colnames(Linkages_table)[4] <- "DFL"
colnames(Linkages_table)[5] <- "BPD"
colnames(Linkages_table)[6] <- "FPD"
PRS_graph<-ggplot(Linkages_table, aes(x=BPD, y=FPD, color=CATEGORY)) + geom_point(shape=19, size=5) + geom_hline(aes(yintercept=1), colour="#BB0000", linetype="dashed") + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")

#SELECTION OF PRIMARY SECTOR
P.sector<-cbind(DBL,DFL)
colnames (P.sector) [1]<-"Sectors"
P.sector[4]<-NULL
P.sector[4]<-NULL
P.sector.selected <- P.sector[ which(P.sector$DBL >= 1),]
P.sector.selected <- P.sector.selected[ which(P.sector.selected$DFL >= 1),]
colnames(P.sector.selected)[1] <- "SECTOR"
colnames(P.sector.selected)[2] <- "CATEGORY"

#GDP
GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
GDP.val.m<-as.matrix(GDP.val)
GDP.val.m<-as.numeric(GDP.val.m)
OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
OUTPUT.val.m<-as.matrix(OUTPUT.val)
OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
GDP<-cbind(sector,GDP.val,OUTPUT.val)
colnames(GDP)[1] <- "SECTOR"
colnames(GDP)[2] <- "CATEGORY"
colnames(GDP)[3] <- "GDP"
colnames(GDP)[4] <- "OUTPUT"
GDP$GDP_PROP<-GDP$GDP/GDP$OUTPUT
GDP[is.na(GDP)]<-0
colnames(GDP)[5] <- "P_OUTPUT"
GDP_tot<-as.matrix(GDP$GDP)
GDP_tot<-colSums(GDP_tot)
GDP$P_GDP<-round((GDP$GDP/GDP_tot), digits=2)
order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
order_GDP10<-head(order_GDP,n=20)
GDP_graph<-ggplot(data=order_GDP10, aes(x=SECTOR, y=GDP, fill=SECTOR)) + 
  geom_bar(colour="black", stat="identity")+ coord_flip() +  
  guides(fill=FALSE) + xlab("Sectors") + ylab("GDP") 
GDP$GDP<-round(GDP$GDP, digits=1)
GDP$OUTPUT<-round(GDP$OUTPUT, digits=1)
GDP$P_OUTPUT<-round(GDP$P_OUTPUT, digits=2)
GDP$P_GDP<-round(GDP$P_GDP, digits=2)

#OUTPUT MULTIPLIER 
Out.multiplier<-colSums(Leontief)
Out.multiplier<-cbind(sector,Out.multiplier)
order_Out.multiplier <- as.data.frame(Out.multiplier[order(-Out.multiplier$Out.multiplier),])
order_Out.multiplier <-head(order_Out.multiplier,n=20)
OMPL_graph<-ggplot(data=order_Out.multiplier, aes(x=V1, y=Out.multiplier, fill=V2)) + 
  geom_bar(colour="black", stat="identity")+ coord_flip() +  
  guides(fill=FALSE) + xlab("Sectors") + ylab("Output multiplier")

#INCOME MULTIPLIER
V.income<-as.matrix(GDP.val*fin_con)
Inc.multiplier<-Leontief%*%V.income
multiplier<-cbind(Out.multiplier,Inc.multiplier)
Inc.multiplier<-cbind(sector,Inc.multiplier)
colnames(Inc.multiplier)[3]<-"Inc.multiplier"
order_Inc.multiplier <- as.data.frame(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier),])
order_Inc.multiplier <-head(order_Inc.multiplier,n=20)
IMPL_graph<-ggplot(data=order_Inc.multiplier, aes(x=V1, y=Inc.multiplier, fill=V2)) + 
  geom_bar(colour="black", stat="identity")+ coord_flip() +  
  guides(fill=FALSE) + xlab("Sectors") + ylab("Income multiplier") 

#LABOUR MULTIPLIER
labour.m<-as.matrix(labour*fin_con)
labour.m<-labour.m/1000000
Lab.multiplier<-Leontief%*%labour.m
multiplier<-cbind(multiplier,Lab.multiplier)
colnames(multiplier)[1] <- "SECTOR"
colnames(multiplier)[2] <- "CATEGORY"
colnames(multiplier)[5] <- "Lab.multiplier"
multiplier$Out.multiplier<-round(multiplier$Out.multiplier, digits=3)
Lab.multiplier<-cbind(sector,Lab.multiplier)
colnames(Lab.multiplier)[3]<-"Lab.multiplier"
order_Lab.multiplier <- as.data.frame(Lab.multiplier[order(-Lab.multiplier$Lab.multiplier),])
order_Lab.multiplier <-head(order_Lab.multiplier,n=20)
LMPL_graph<-ggplot(data=order_Lab.multiplier, aes(x=V1, y=Lab.multiplier, fill=V2)) + 
  geom_bar(colour="black", stat="identity")+ coord_flip() +  
  guides(fill=FALSE) + xlab("Sectors") + ylab("Labour multiplier")
colnames(multiplier)[4]<-"Inc.multiplier"
multiplier$Inc.multiplier<-round(multiplier$Inc.multiplier, digits=3)

#COMBINE MULTIPLIER
sel.multiplier<-multiplier[ which(multiplier$Out.multiplier > 1),]
sel.multiplier<-sel.multiplier[ which(sel.multiplier$Inc.multiplier > 1),]

int_con.m<-as.matrix(int_con)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_con.m)

#CALCULATE INVERS LEONTIEF
int_con.ctot<-colSums(int_con.m)
add_val.ctot<-colSums(add_val.m)
fin_con<- 1/(int_con.ctot+add_val.ctot)
fin_con[is.infinite(fin_con)]<-0
t.input.invers<-diag(fin_con)
A<-int_con.m %*% t.input.invers
I<-as.matrix(diag(dim))
I_A<-I-A
Leontief<-solve(I_A)

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

### LAND REQUIREMENT Module ####

nodata_val <- 0
landuse_lut <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/table/landuse_table_bungo.csv")
land_use <- rast("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/raster/bungo_landcover_2010r.tif")
land_distribution <- read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/regeco/08_land distribution.csv", header = FALSE)
lc_freq <- freq(land_use)
lc_freq <- as.data.frame(na.omit(lc_freq))
landuse_area<-subset(landuse_lut,ID !=nodata_val)
landuse_area<- as.matrix((lc_freq$count))
land.distribution_t<-as.matrix(land_distribution)
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
#land.requirement_table$LPC<-round(land.requirement_table$LPC, digits=2)-->catasthropic error !!!!
order_land.requirement <- as.data.frame(land.requirement_table[order(-land.requirement_table$LRC),])
order_land.requirement <-head(order_land.requirement,n=20)
LRC_graph<-ggplot(data=order_land.requirement, aes(x=SECTOR, y=LRC, fill=CATEGORY)) +
  geom_bar(stat="identity")+ coord_flip() + xlab("Sectors") + ylab("Land requirement coefficient") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6),
         axis.text.x = element_text(size = 8))

### PROJECTION Module ####
nodata_val<-0
land.requirement.db<-land.requirement_table
lc.list<-subset(landuse_lut, select=c(ID, LC))

int_con.m<-as.matrix(int_con)
add_val.m<-as.matrix(add_val)
dim<-ncol(int_con.m)

#CALCULATE INVERS LEONTIEF
int_con.ctot<-colSums(int_con.m)
add_val.ctot<-colSums(add_val.m)
fin_con<- 1/(int_con.ctot+add_val.ctot)
fin_con[is.infinite(fin_con)]<-0
t.input.invers<-diag(fin_con)
A<-int_con.m %*% t.input.invers
I<-as.matrix(diag(dim))
I_A<-I-A
Leontief<-solve(I_A)

#DIRECT BACKWARD LINKAGES
DBL<-colSums(Leontief)
DBL<-DBL/(mean(DBL))
DBL<-cbind(sector,DBL)
colnames(DBL)[3] <- "DBL"
order_DBL <- as.data.frame(DBL[order(-DBL$DBL),])
order_DBL10<-head(order_DBL,n=20)
colnames(order_DBL10)[1] <- "SECTOR"
colnames(order_DBL10)[2] <- "CATEGORY"
BPD_graph<-ggplot(data=order_DBL10, aes(x=SECTOR, y=DBL, fill=CATEGORY)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Value")

#DIRECT FORWARD LINKAGES
DFL<-rowSums(Leontief)
DFL<-DFL/(mean(DFL))
DFL<-cbind(sector,DFL)
colnames(DFL)[3] <- "DFL"
order_DFL <- as.data.frame(DFL[order(-DFL$DFL),])
order_DFL10<-head(order_DFL,n=20)
colnames(order_DFL10)[1] <- "SECTOR"
colnames(order_DFL10)[2] <- "CATEGORY"
FPD_graph<-ggplot(data=order_DFL10, aes(x=SECTOR, y=DFL, fill=CATEGORY)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Value")


#CREATE LINKAGES TABLE
DBL_temp<-colSums(Leontief)
BPD_temp<-DBL_temp/(mean(as.matrix(DBL_temp)))
DFL_temp<-rowSums(Leontief)
FPD_temp<-DFL_temp/(mean(as.matrix(DFL_temp)))
DBL_temp<-as.data.frame(round(DBL_temp, digits=2))
BPD_temp<-as.data.frame(round(BPD_temp, digits=2))
DFL_temp<-as.data.frame(round(DFL_temp, digits=2))
FPD_temp<-as.data.frame(round(FPD_temp, digits=2))
Linkages_table<-cbind(sector,DBL_temp,DFL_temp,BPD_temp,FPD_temp)
colnames(Linkages_table)[1] <- "SECTOR"
colnames(Linkages_table)[2] <- "CATEGORY"
colnames(Linkages_table)[3] <- "DBL"
colnames(Linkages_table)[4] <- "DFL"
colnames(Linkages_table)[5] <- "BPD"
colnames(Linkages_table)[6] <- "FPD"
PRS_graph<-ggplot(Linkages_table, aes(x=BPD, y=FPD, color=CATEGORY)) + geom_point(shape=19, size=5) + geom_hline(aes(yintercept=1), colour="#BB0000", linetype="dashed") + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")


#SELECTION OF PRIMARY SECTOR
P.sector<-cbind(DBL,DFL)
colnames (P.sector) [1]<-"Sectors"
P.sector[4]<-NULL
P.sector[4]<-NULL
P.sector.selected <- P.sector[ which(P.sector$DBL >= 1),]
P.sector.selected <- P.sector.selected[ which(P.sector.selected$DFL >= 1),]
colnames(P.sector.selected)[1] <- "SECTOR"
colnames(P.sector.selected)[2] <- "CATEGORY"


#GDP
GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
GDP.val.m<-as.matrix(GDP.val)
GDP.val.m<-as.numeric(GDP.val.m)
OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
OUTPUT.val.m<-as.matrix(OUTPUT.val)
OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
GDP<-cbind(sector,GDP.val,OUTPUT.val)
colnames(GDP)[1] <- "SECTOR"
colnames(GDP)[2] <- "CATEGORY"
colnames(GDP)[3] <- "GDP"
colnames(GDP)[4] <- "OUTPUT"
GDP$GDP_PROP<-GDP$GDP/GDP$OUTPUT
GDP[is.na(GDP)]<-0
colnames(GDP)[5] <- "P_OUTPUT"
GDP_tot<-as.matrix(GDP$GDP)
GDP_tot<-colSums(GDP_tot)
GDP$P_GDP<-round((GDP$GDP/GDP_tot), digits=2)
order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
order_GDP10<-head(order_GDP,n=20)
GDP_graph<-ggplot(data=order_GDP10, aes(x=SECTOR, y=GDP, fill=SECTOR)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("GDP")
GDP$GDP<-round(GDP$GDP, digits=1)
GDP$OUTPUT<-round(GDP$OUTPUT, digits=1)
GDP$P_OUTPUT<-round(GDP$P_OUTPUT, digits=2)
GDP$P_GDP<-round(GDP$P_GDP, digits=2)


#OUTPUT MULTIPLIER
Out.multiplier<-colSums(Leontief)
Out.multiplier<-cbind(sector,Out.multiplier)
order_Out.multiplier <- as.data.frame(Out.multiplier[order(-Out.multiplier$Out.multiplier),])
order_Out.multiplier <-head(order_Out.multiplier,n=20)
OMPL_graph<-ggplot(data=order_Out.multiplier, aes(x=V1, y=Out.multiplier, fill=V2)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Output multiplier")

#INCOME MULTIPLIER
V.income<-as.matrix(GDP.val*fin_con)
Inc.multiplier<-Leontief%*%V.income
multiplier<-cbind(Out.multiplier,Inc.multiplier)
Inc.multiplier<-cbind(sector,Inc.multiplier)
colnames(Inc.multiplier)[3]<-"Inc.multiplier"
order_Inc.multiplier <- as.data.frame(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier),])
order_Inc.multiplier <-head(order_Inc.multiplier,n=20)
IMPL_graph<-ggplot(data=order_Inc.multiplier, aes(x=V1, y=Inc.multiplier, fill=V2)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Income multiplier")

#LABOUR MULTIPLIER
labour.m<-as.matrix(labour*fin_con)
labour.m<-labour.m/1000000
Lab.multiplier<-Leontief%*%labour.m
multiplier<-cbind(multiplier,Lab.multiplier)
colnames(multiplier)[1] <- "SECTOR"
colnames(multiplier)[2] <- "CATEGORY"
colnames(multiplier)[5] <- "Lab.multiplier"
multiplier$Out.multiplier<-round(multiplier$Out.multiplier, digits=3)
Lab.multiplier<-cbind(sector,Lab.multiplier)
colnames(Lab.multiplier)[3]<-"Lab.multiplier"
order_Lab.multiplier <- as.data.frame(Lab.multiplier[order(-Lab.multiplier$Lab.multiplier),])
order_Lab.multiplier <-head(order_Lab.multiplier,n=20)
LMPL_graph<-ggplot(data=order_Lab.multiplier, aes(x=V1, y=Lab.multiplier, fill=V2)) +
  geom_bar(colour="black", stat="identity")+ coord_flip() +
  guides(fill=FALSE) + xlab("Sectors") + ylab("Labour multiplier")
colnames(multiplier)[4]<-"Inc.multiplier"
multiplier$Inc.multiplier<-round(multiplier$Inc.multiplier, digits=3)

#Read land use map and calculate area of land use
next_landuse_lut<-read.csv("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/table/luc_projection.csv")

land_use <- rast("D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/data/raster/bungo_landcover_2010r.tif")
lc_freq <- freq(land_use)
lc_freq <- as.data.frame(na.omit(lc_freq))
landuse_area0<-subset(landuse_lut,ID !=nodata_val)
landuse_area0<- as.matrix((lc_freq$count))

# landuse_area0<-as.data.frame(levels(landuse0))
landuse_area<-subset(next_landuse_lut, ID != nodata_val)
# landuse_area<-subset(landuse_area,ID !=15)
landuse_area<-data.frame(landuse_area$ID, landuse_area$LC, landuse_area$T1)
landuse_table<-as.data.frame(cbind(landuse_area$landuse_area.LC,landuse_area0,landuse_area$landuse_area.T1))
landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area$landuse_area.T1)))
colnames(landuse_table)[1] <- "LAND_USE"
colnames(landuse_table)[2] <- "T1_HA"
colnames(landuse_table)[3] <- "T2_HA"
landuse_table<-edit(landuse_table)
landuse_table$CHANGE<-as.numeric(landuse_table$T2_HA)-as.numeric(landuse_table$T1_HA)

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

#calculate total GDP
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
LAB_graph<-ggplot(data=test2, aes(x=V1, y=test2, fill=V2)) +
  geom_bar(colour="black", stat="identity", position="dodge")+
  guides(fill=FALSE) + xlab("Sector") + ylab("Labour requirement") +ggtitle("Impact of LU Change to Labour")+ theme(axis.text.x  = element_text(angle=90, size=6))
