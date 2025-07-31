#=====================================================
##TA-Profit Map
landuse_1_path<-"data/raster/bungo_landcover_1990r.tif"
landuse_2_path<-"data/raster/bungo_landcover_2010r.tif"
lookup_c_path<-"data/table/carbon_bungo.csv"
lookup_npv_path<-"data/table/profitability_table_bungo.csv"
planning_unit_path <- "data/raster/bungo_zone.tif"
lookup_pu_path<- "data/table/zone_table_bungo.csv"
raster.nodata=0
T1=1990
T2=2010
#=====================================================

#=Load library
library(terra)
library(splitstackshape)

zone<-rast(planning_unit_path)
lookup_z<-read.csv(lookup_pu_path)

# landuse first time period
landuse1<-rast(landuse_1_path)  
# landuse second time period
landuse2<-rast(landuse_2_path)
# landcover lookup table
lookup_c <- read_csv(lookup_c_path)
lookup_c$color_palette<-NULL
lookup_npv <- read_csv(lookup_npv_path)
lookup_npv$CARBON<-NULL
# set lookup table
lookup_c<-lookup_c[which(lookup_c[1] != raster.nodata),]
lookup_npv<-lookup_npv[which(lookup_npv[1] != raster.nodata),]
lookup_lc<-lookup_c
lookup_ref<-lookup_z
colnames(lookup_lc)<-c("ID","LC","CARBON")
colnames(lookup_z)<-c("ID", "ZONE")
colnames(lookup_npv)<-c("ID", "LC", "NPV")

nLandCoverId<-nrow(lookup_lc)
nPlanningUnitId<-nrow(lookup_z)

# Resample
landuse2 <- terra::resample(landuse2, landuse1, method = "near")
zone <- terra::resample(zone, landuse1, method = "near")

#=START: Create cross-tabulation for zone
dummy1<-data.frame(nPU=lookup_z$ID, divider=nLandCoverId*nLandCoverId)
dummy1<-expandRows(dummy1, 'divider')

dummy2<-data.frame(nT1=lookup_lc$ID, divider=nLandCoverId)
dummy2<-expandRows(dummy2, 'divider')
dummy2<-data.frame(nT1=rep(dummy2$nT1, nPlanningUnitId))

dummy3<-data.frame(nT2=rep(rep(lookup_lc$ID, nLandCoverId), nPlanningUnitId))

landUseChangeMapDummy<-cbind(dummy1, dummy2, dummy3)
colnames(landUseChangeMapDummy)<-c('ZONE', 'ID_LC1', 'ID_LC2')

# Combine rasters into a single coded raster
R2 <- (zone * 1) + (landuse1 * 100) + (landuse2 * 10000)

# Get frequency table and remove NA
lu.db <- as.data.frame(freq(R2)) |> na.omit()

# Decode the combined value into its components
lu.db <- lu.db |>
  mutate(
    Var3 = value %% 100,                # zone
    Var2 = (value %/% 100) %% 100,      # landuse1
    Var1 = (value %/% 10000) %% 100     # landuse2
  ) |>
  select(-layer)  # Drop the original coded value

colnames(lu.db) = c("ID_CHG", "COUNT", "ZONE", "ID_LC1", "ID_LC2")
lu.db<-merge(landUseChangeMapDummy, lu.db, by=c('ZONE', 'ID_LC1', 'ID_LC2'), all=TRUE)
lu.db<-replace(lu.db, is.na(lu.db), 0)
#=END: Create cross-tabulation for zone

# rename column
colnames(lookup_c) = c("ID_LC1", "LC_t1", "CARBON_t1")
data_merge <- merge(lu.db,lookup_c,by="ID_LC1")
colnames(lookup_c) = c("ID_LC2", "LC_t2", "CARBON_t2")
data_merge <- as.data.frame(merge(data_merge,lookup_c,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
res_m <- terra::res(landuse1)
spat_res <- (res_m[1] * res_m[2]) / 10000
data_merge$COUNT<-data_merge$COUNT*spat_res
# data_merge$COUNT_ZONE<-data_merge$COUNT_ZONE*spat_res

#=Carbon accounting process
NAflag(landuse1) <- as.numeric(raster.nodata)
NAflag(landuse2) <- as.numeric(raster.nodata)

merged_data <- merge(lookup_npv, lookup_lc, by = "ID")
reclassify_matrix <- as.matrix(merged_data[, c("ID", "CARBON")])

map_carbon1 <- terra::classify(landuse1, reclassify_matrix)
map_carbon2 <- terra::classify(landuse2, reclassify_matrix)

chk_em <- map_carbon1 > map_carbon2
emission_map <- ((map_carbon1 - map_carbon2) * 3.67) * chk_em

#====NPV Accounting Process====
npv_matrix <- as.matrix(lookup_npv[, c("ID", "NPV")])

map_npv1 <- terra::classify(landuse1, npv_matrix)
map_npv2 <- terra::classify(landuse2, npv_matrix)

npv_chg_map <- map_npv2 - map_npv1

npv_chg_map<-map_npv2-map_npv1
opcost_map<-npv_chg_map/emission_map

#result
carbontiff1<-map_carbon1
carbontiff2<-map_carbon2
npvtiff1<-map_npv1
npvtiff2<-map_npv2
npvchgtiff<-npv_chg_map
opcosttiff<-opcost_map
