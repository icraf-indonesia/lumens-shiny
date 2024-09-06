#Regional Economy Single I-O Descriptive Analysis
sector_file="data/regeco/01_sektor.csv"
int_con_file="data/regeco/02_intermediate demand.csv"
fin_dem_struc_file="data/regeco/03_final demand comp.csv"
fin_dem_file="data/regeco/04_final demand.csv"
add_val_struc_file="data/regeco/05_added value comp.csv"
add_val_file="data/regeco/06_added value.csv"
labour_file="data/regeco/07_labour.csv"
unit="Million Rupiah"
location="Bungo"
I_O_period= 2010

library(reshape2)
library(ggplot2)
library(foreign)
library(magick)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# #=Set Working Directory
# working_dir<-paste(dirname(proj.file), "/TA/Descriptive_Analysis", idx_TA_regeco, sep="")
# dir.create(working_dir, mode="0777")
# setwd(working_dir)

#READ INPUT FILE
int_con <- read.csv(int_con_file, header = FALSE)
add_val <- read.csv(add_val_file, header = FALSE)
fin_dem <- read.csv(fin_dem_file, header = FALSE)
fin_dem_struc <- read.csv(fin_dem_struc_file)
add_val_struc <- read.csv(add_val_struc_file)
sector <- read.csv(sector_file, header = FALSE)
labour <- read.csv(labour_file, header = FALSE)

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

#EXPORT OUTPUT
Leontief_df<-as.data.frame(Leontief)
PDRB<-"Sectoral_GDP"
PENGGANDA<-"Sectoral_multiplier"

#=Save all params into .ldbase objects
save(int_con,
     add_val,
     fin_dem,
     fin_dem_struc,
     add_val_struc,
     sector,
     labour,
     unit,
     location,
     I_O_period,
     Leontief_df,
     GDP,
     Linkages_table,
     multiplier,
     file=paste0('LandReq', I_O_period, '.Rdata'))
