##TA-Profit Map
landuse_1="data/raster/tutupan_lahan_Bungo_2005r.tif"
landuse_2="data/raster/tutupan_lahan_Bungo_2010r.tif"
planning_unit="data/raster/Zona_Bungo.tif"
lookup_c="data/table/Tabel_karbon_Bungo.csv"
lookup_npv="data/table/Tabel_profitabilitas_Bungo.csv"
# lookup_z_path="data/table/Tabel_zona_Bungo.csv"
raster.nodata=0
T1=2005
T2=2010

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

#=Set Working Directory
wd<-paste(dirname(workspace), "/TA-Profit/", "_OpCost_", T1, "_", T2, sep="")
dir.create(wd)
setwd(wd)

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

#=Set project properties
# title="Kabupaten Bungo"
# tab_title<-as.data.frame(title)
# period1=T1
# period2=T2
# period=period2-period1
# proj_prop<-as.data.frame(title)
# proj_prop$period1<-period1
# proj_prop$period2<-period2
# proj_prop$period <- do.call(paste, c(proj_prop[c("period1", "period2")], sep = " - "))

#=START: Create cross-tabulation for zone
# xtab<-tolower(paste('xtab_', pu_name, T1, T2, sep=''))
# data_xtab<-list_of_data_lut[which(list_of_data_lut$TBL_NAME==xtab),]

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