#=====================================================
##TA-Profit Curve
data_dir <- "D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/07_ta-profit/rscript/tests"
ques_c_db <- file.path(data_dir, "data/quesc_database.csv")
NPV <- file.path(data_dir, "data/profitability_table.csv")
cost_threshold= 2
T1=2000
T2=2010
#=====================================================

library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(foreign)
library(scales)
library(readr)

#Create Folder and Working Directory
setwd(data_dir)

#Load Datasets
data<-read_csv(ques_c_db)
lookup_npv<- read_csv(NPV)
t1=T1
t2=T2
period<-t2-t1

#Prepare NPV Lookup Table
lookup_n<-lookup_npv
lookup_n[,2]<-NULL
lookup_n[,3]<-NULL #adjustment
colnames(lookup_n)[1] ="ID_LC1"
colnames(lookup_n)[2] ="NPV1"
data<-merge(data,lookup_n,by="ID_LC1")
colnames(lookup_n)[1] ="ID_LC2"
colnames(lookup_n)[2] ="NPV2"
data<-merge(data,lookup_n,by="ID_LC2")
colnames(data)[colnames(data) == "C_T1"] <- "CARBON_t1" #adjustment
colnames(data)[colnames(data) == "C_T2"] <- "CARBON_t2" #adjustment
colnames(data)[colnames(data) == "Ha"] <- "COUNT" #adjustment
colnames(data)[colnames(data) == "EM"] <- "em" #adjustment
colnames(data)[colnames(data) == "SQ"] <- "sq" #adjustment
colnames(data)[colnames(data) == "PU"] <- "Z_NAME" #adjustment
data$ck_em<-data$CARBON_t1>data$CARBON_t2 #adjustment
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

#=====================================================
##TA-Profit Map
landuse_1<-file.path(data_dir, "data/NTT/NTT_2000V3F.tif")
landuse_2<-file.path(data_dir, "data/NTT/NTT_2010V3F.tif")
planning_unit<-file.path(data_dir, "data/NTT/vector/Admin51s_F_join.shp")
lookup_c<-file.path(data_dir, "data/NTT/carbon_NTT_kp.csv")
lookup_npv<-file.path(data_dir, "data/profitability_table.csv")
# lookup_z_path="data/table/Tabel_zona_Bungo.csv"
raster.nodata=0
# T1=2005
# T2=2010
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

# return the selected data from the list
data_luc1 <- raster(landuse_1)
data_luc2 <- raster(landuse_2)
data_ref <- shapefile(reference)
data_pu <- raster(planning_unit) 
data_lut <- read_csv(lookup_c)
data_npv <- read_csv(lookup_npv)

#=Planning unit
ref <- raster(planning_unit)
ref<-data_ref
count_ref <- freq(ref) |> as.data.frame()|> na.omit()
colnames(count_ref) <- c("IDADM", "COUNT")
ref_table<- read.csv(lookup_z_path)
colnames(ref_table)[colnames(ref_table) == "ID"] = "IDADM"
lookup_z <- merge(count_ref, ref_table, by = "IDADM")

#=or use Zone
zone<-raster(ref)
lookup_z_zone<-read.csv(lookup_z_path)

# landuse first time period
landuse1<-data_luc1  
# landuse second time period
landuse2<-data_luc2
# landcover lookup table
lookup_c <- data_lut 
lookup_npv <- data_npv
# set lookup table
lookup_c<-lookup_c[which(lookup_c[1] != raster.nodata),]
lookup_npv<-lookup_npv[which(lookup_npv[1] != raster.nodata),]
lookup_lc<-lookup_c
lookup_ref<-lookup_z
colnames(lookup_lc)<-c("ID","LC","CARBON")
colnames(lookup_z)<-c("ID", "COUNT_ZONE", "ZONE")
colnames(lookup_npv)<-c("ID", "LC", "NPV")

nLandCoverId<-nrow(lookup_lc)
nPlanningUnitId<-nrow(lookup_z)

#=Projection handling
if (grepl("+units=m",  sp::proj4string(ref))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref)[1]*res(ref)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution")
} else if (grepl("+proj=longlat",  sp::proj4string(ref))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution")
} else{
  statuscode<-0
  statusmessage<-"Raster map projection is unknown"
  statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
  quit()
}

#=START: Create cross-tabulation for zone
dummy1<-data.frame(nPU=lookup_z$ID, divider=nLandCoverId*nLandCoverId)
dummy1<-expandRows(dummy1, 'divider')

dummy2<-data.frame(nT1=lookup_lc$ID, divider=nLandCoverId)
dummy2<-expandRows(dummy2, 'divider')
dummy2<-data.frame(nT1=rep(dummy2$nT1, nPlanningUnitId))

dummy3<-data.frame(nT2=rep(rep(lookup_lc$ID, nLandCoverId), nPlanningUnitId))

landUseChangeMapDummy<-cbind(dummy1, dummy2, dummy3)
colnames(landUseChangeMapDummy)<-c('ZONE', 'ID_LC1', 'ID_LC2')

R2<-(zone*1) + (landuse1*100^1)+ (landuse2*100^2) 
lu.db<-as.data.frame(freq(R2))
lu.db<-na.omit(lu.db)
n<-3
k<-0
lu.db$value_temp<-lu.db$value
while(k < n) {
  eval(parse(text=(paste("lu.db$Var", n-k, "<-lu.db$value_temp %% 100", sep=""))))  
  lu.db$value_temp<-floor(lu.db$value_temp/100)
  k=k+1
}
lu.db$value_temp<-NULL
colnames(lu.db) = c("ID_CHG", "COUNT", "ZONE", "ID_LC1", "ID_LC2")
lu.db<-merge(landUseChangeMapDummy, lu.db, by=c('ZONE', 'ID_LC1', 'ID_LC2'), all=TRUE)
lu.db$ID_CHG<-lu.db$ZONE*1 + lu.db$ID_LC1*100^1 + lu.db$ID_LC2*100^2
lu.db<-replace(lu.db, is.na(lu.db), 0)

idx_lut<-idx_lut+1
eval(parse(text=(paste("in_lut", idx_lut, " <- lu.db", sep=""))))

eval(parse(text=(paste("list_of_data_lut<-data.frame(TBL_DATA='in_lut", idx_lut,"', TBL_NAME='", xtab, "', row.names=NULL)", sep=""))))

setwd(working_directory)
idx_factor<-idx_factor+1
chg_map<-tolower(paste('chgmap_', pu_name, T1, T2, sep=''))
eval(parse(text=(paste("writeRaster(R2, filename='", chg_map, ".tif', format='GTiff', overwrite=TRUE)", sep=""))))
eval(parse(text=(paste("factor", idx_factor, "<-'", chg_map, "'", sep=''))))  
eval(parse(text=(paste("list_of_data_f<-data.frame(RST_DATA='factor", idx_factor,"', RST_NAME='", chg_map, "', row.names=NULL)", sep=""))))  
InFactor_i <- paste("factor", idx_factor, sep="")  
dbWriteTable(DB, "list_of_data_f", list_of_data_f, append=TRUE, row.names=FALSE)
#write to csv
list_of_data_f<-dbReadTable(DB, c("public", "list_of_data_f"))
csv_file<-paste(dirname(proj.file),"/csv_factor_data.csv", sep="")
write.table(list_of_data_f, csv_file, quote=FALSE, row.names=FALSE, sep=",")  
addRasterToPG(project, paste0(chg_map, '.tif'), InFactor_i, srid)
unlink(paste0(chg_map, '.tif'))

#=END: Create cross-tabulation for zone

# rename column
colnames(lookup_c) = c("ID_LC1", "LC_t1", "CARBON_t1")
data_merge <- merge(lu.db,lookup_c,by="ID_LC1")
colnames(lookup_c) = c("ID_LC2", "LC_t2", "CARBON_t2")
data_merge <- as.data.frame(merge(data_merge,lookup_c,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[3]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
#data_merge <- as.data.frame(merge(data_merge,lookup_ref,by="REF"))
data_merge$COUNT<-data_merge$COUNT*Spat_res
data_merge$COUNT_ZONE<-data_merge$COUNT_ZONE*Spat_res

#=Carbon accounting process
NAvalue(landuse1)<-raster.nodata
NAvalue(landuse2)<-raster.nodata
rcl.m.c1<-as.matrix(lookup_lc[,1])
rcl.m.c2<-as.matrix(lookup_lc[,3])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
rcl.m<-rbind(rcl.m, c(0, NA))
carbon1<-reclassify(landuse1, rcl.m)
carbon2<-reclassify(landuse2, rcl.m)
chk_em<-carbon1>carbon2
chk_sq<-carbon1<carbon2
emission<-((carbon1-carbon2)*3.67)*chk_em
sequestration<-((carbon2-carbon1)*3.67)*chk_sq

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
