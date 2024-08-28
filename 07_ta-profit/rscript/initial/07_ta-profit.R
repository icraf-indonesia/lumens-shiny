#=====================================================
workspace=""
NPV="D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/07_ta_profit/data/tabel_acuan_NPV.csv"
ques_c_db="D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/07_ta_profit/data/QUESC_database_2000-2020.csv"
cost_threshold= 2
T1=2020
T2=2024
#=====================================================

#=Load library
library(tiff)
library(foreign)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(grid)
library(ggplot2)
library(spatial.tools)
library(splitstackshape)
library(stringr)
library(magick)
library(readr)
library(hexbin)
library(scales)

#Create Folder and Working Directory
setwd(wd)

#Load Datasets
data<-read_csv(ques_c_db)
lookup_npv<- read_csv(NPV)
t1=T1
t2=T2
period<-t2-t1

#Prepare NPV Lookup Table
lookup_n<-lookup_npv
lookup_n[,2]<-NULL
colnames(lookup_n)[1] ="ID_LC1"
colnames(lookup_n)[2] ="NPV1"
data<-merge(data,lookup_n,by="ID_LC1")
colnames(lookup_n)[1] ="ID_LC2"
colnames(lookup_n)[2] ="NPV2"
data<-merge(data,lookup_n,by="ID_LC2")
tot_area<-sum(data$COUNT)

#Select Data where Emission Happened and Count > 0
data_em_sel <- data[ which(data$ck_em == "TRUE"),]
data_em_sel <- data_em_sel[ which(data_em_sel$em > 0),]
data_em_sel<-within(data_em_sel, {
  em_rate<-((CARBON_t1-CARBON_t2)*(COUNT*3.67))/(tot_area*period)
  em_tot<- (CARBON_t1-CARBON_t2)*3.67
  sq_rate<-((CARBON_t2-CARBON_t1)*(COUNT*3.67))/(tot_area*period)
  sq_tot<- (CARBON_t2-CARBON_t1)*3.67
  opcost<-(NPV1-NPV2)/em_tot
  opcost_sq<-(NPV1-NPV2)/sq_tot
  cumsum_em<-cumsum(em_rate)
  cumsum_sq<-cumsum(sq_rate)
})

#Build Opcost Table
lcc_col<-as.data.frame(data_em_sel$LU_CHG)
zone_col<-as.data.frame(data_em_sel$Z_NAME)
opcost_col<-as.data.frame(data_em_sel$opcost)
em_col<-as.data.frame(data_em_sel$em_rate)
opcost_tab<-cbind(lcc_col,zone_col)
opcost_tab<-cbind(opcost_tab,opcost_col)
opcost_tab<-cbind(opcost_tab,em_col)
names(opcost_tab)[1] <- "luchg"
names(opcost_tab)[2] <- "zone"
names(opcost_tab)[3] <- "opcost"
names(opcost_tab)[4] <- "emrate"

#Build Positive Opcost Table
opcost_tab_p<- opcost_tab[ which(opcost_tab$opcost >= 0),]
opcost_tab_p<- opcost_tab_p[order(opcost_tab_p$opcost),]
opcost_tab_p$cum_emrate<-cumsum(opcost_tab_p$emrate)
TA_opcost_database<-opcost_tab_p
write.dbf(TA_opcost_database,"TA_opcost_database.dbf")
opcost_tab_p$opcost_log<-log10(opcost_tab_p$opcost)
is.na(opcost_tab_p) <- sapply(opcost_tab_p, is.infinite)
opcost_tab_p[is.na(opcost_tab_p)] <- 0

#Build Negative Opcost Table
opcost_tab_n<- opcost_tab[ which(opcost_tab$opcost < 0),]
opcost_tab_n<- opcost_tab_n[order(opcost_tab_n$opcost),]
opcost_tab_n$cum_emrate<-cumsum(opcost_tab_n$emrate)
opcost_tab_n$opcost_log<-opcost_tab_n$opcost*-1
opcost_tab_n$opcost_log<-log10(opcost_tab_n$opcost_log)*-1

#Combine Positive && Negative Opcost
opcost_all<-rbind(opcost_tab_n, opcost_tab_p)
opcost_all$cum_emrate2<-as.factor(opcost_all$cum_emrate)

#Find Cost Threshold
opcost_all2<- opcost_all
opcost_all2$order<-c(1:nrow(opcost_all2))
find_x_val<-subset(opcost_all2, opcost_log>=log10(cost_threshold))
x_val<-find_x_val$order[1]

#====NPV Accounting Process====
rcl.m.npv1<-as.matrix(lookup_npv[,1])
rcl.m.npv2<-as.matrix(lookup_npv[,3])
rcl.m.npv<-cbind(rcl.m.npv1,rcl.m.npv2)
npv1<-reclassify(landuse1, rcl.m.npv)
npv2<-reclassify(landuse2, rcl.m.npv)

npv_chg<-npv2-npv1
opcost<-npv_chg/emission

#export analysis result
carbontiff1<-carbon1
carbontiff2<-carbon2
npvtiff1<-npv1
npvtiff2<-npv2
npvchgtiff<-npv_chg
opcosttiff<-opcost