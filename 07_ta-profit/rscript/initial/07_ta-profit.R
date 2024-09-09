#=====================================================
workspace=""
landuse_1_map="data/raster/tutupan_lahan_Bungo_2005r.tif"
landuse_2_map="data/raster/tutupan_lahan_Bungo_2010r.tif"
NPV_table="07_ta-profit/data/tabel_acuan_NPV.csv"
ques_c_db="data/table/quesc_database.csv"
# lookup_c="data/table/Tabel_karbon_Bungo.csv"
cost_threshold= 2
raster.nodata=0
T1=2005
T2=2010
#=====================================================

#=Load library
# library(tiff)
# library(foreign)
# library(rasterVis)
# library(reshape2)
# library(plyr)
# library(lattice)
# library(latticeExtra)
# library(RColorBrewer)
# library(grid)
# library(ggplot2)
# library(spatial.tools)
# library(splitstackshape)
# library(stringr)
# library(magick)
# library(hexbin)
# library(scales)

library(readr)
library(raster)
library(ggplot2)

#Create Folder and Working Directory
setwd(wd)

#Load Datasets
data<-read_csv(ques_c_db)
lookup_npv<- read_csv(NPV_table)
# lookup_c<-read_csv(lookup_c)
landuse1=raster(landuse_1_map)
landuse2=raster(landuse_2_map)
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
tot_area<-sum(data$Ha)

#Select Data where Emission Happened and Count > 0
# data_em_sel <- data[ which(data$ck_em == "TRUE"),]
data_em_sel <- data
data_em_sel <- data_em_sel[ which(data_em_sel$EM > 0),]
data_em_sel<-within(data_em_sel, {
  em_rate<-((C_T1-C_T2)*(Ha*3.67))/(tot_area*period)
  em_tot<- (C_T1-C_T2)*3.67
  sq_rate<-((C_T1-C_T2)*(Ha*3.67))/(tot_area*period)
  sq_tot<- (C_T1-C_T2)*3.67
  opcost<-(NPV1-NPV2)/em_tot
  opcost_sq<-(NPV1-NPV2)/sq_tot
  cumsum_em<-cumsum(em_rate)
  cumsum_sq<-cumsum(sq_rate)
})

#Build Opcost Table
lcc_col<-as.data.frame(data_em_sel$LU_CHG)
zone_col<-as.data.frame(data_em_sel$PU)
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

# step ambil nilai karbon
lookup_npv<-lookup_npv[which(lookup_npv[1] != 0),]
colnames(lookup_npv)<-c("ID", "LC", "NPV")

rcl.id<-as.matrix(lookup_npv[,1])
rcl.npv<-as.matrix(lookup_npv[,3])
tbl.npv<-as.data.frame(cbind(rcl.id, rcl.npv))
rcl.id.carbon<-as.matrix(data$ID_LC1)
rcl.carbon<-as.matrix(data$C_T1)
tbl.carbon<-as.data.frame(cbind(rcl.id.carbon, rcl.carbon))
colnames(tbl.carbon)<-c('ID', 'Carbon')
lookup_carbon<-unique(merge(tbl.npv,tbl.carbon))
lookup_carbon$NPV<-NULL
lookup_carbon<-rbind(lookup_carbon, c(0, NA))

# kolom ID, LUC dari tabel NPV
# merge ID dengan quescdb untuk kolom carbon

#====Carbon Accounting Process====
NAvalue(landuse1)<-raster.nodata
NAvalue(landuse2)<-raster.nodata
# rcl.m.c1<-as.matrix(lookup_c[,1])
# rcl.m.c2<-as.matrix(lookup_c[,3])
# rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
# rcl.m<-rbind(rcl.m, c(0, NA))
rcl.m<-lookup_carbon
carbon1<-reclassify(landuse1, rcl.m)
carbon2<-reclassify(landuse2, rcl.m)
chk_em<-carbon1>carbon2
emission<-((carbon1-carbon2)*3.67)*chk_em

#====NPV Accounting Process====
rcl.m.npv1<-as.matrix(lookup_npv[,1])
rcl.m.npv2<-as.matrix(lookup_npv[,3])
rcl.m.npv<-cbind(rcl.m.npv1,rcl.m.npv2)
npv1<-reclassify(landuse1, rcl.m.npv)
npv2<-reclassify(landuse2, rcl.m.npv)

npv_chg<-npv2-npv1
opcost<-npv_chg/emission

#Map Result
npvtiff1<-npv1
npvtiff2<-npv2
npvchgtiff<-npv_chg
opcosttiff<-opcost

#====Opportunity Cost Curve====
# Plot the Opportunity Cost Curve
df <- data.frame(
  emission = opcost_all2$emrate,
  opportunity_cost = opcost_all2$opcost,
  land_use_change = opcost_all2$luchg
)

df_grouped <- df %>%
  group_by(land_use_change) %>%
  summarise(emission = sum(emission),
            opportunity_cost = sum(opportunity_cost))

df_all <- df_grouped %>% filter(opportunity_cost != 0)
df_order <- df_all[order(df_all$opportunity_cost),]
df_order$order<-c(1:nrow(df_order))

# Create the plot (source: https://www.r-bloggers.com/2015/07/waterfall-plots-what-and-how/)
opcost_curve <- ggplot(df_order, aes(x=order, y=opportunity_cost)) +
  labs(x = NULL,
       y = "Opportunity Cost ($/ton CO2-eq)",
       title = "Waterfall Plot for Opportunity Cost") +
  theme_classic() %+replace%
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(face="bold",angle=90)) +
  coord_cartesian(ylim = c(-5000,5000))

opcost_curve <- opcost_curve + geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))
