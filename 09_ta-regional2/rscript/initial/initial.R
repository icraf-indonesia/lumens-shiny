#Impact of Land Using to Regional Economy Indicator Analysis
##TA-PostgreSQL=group
land_req=land.requirement_table
projected_land_use="data/raster/tutupan_lahan_Bungo_2010r.tif"
landuse_lut="data/table/Tabel_landuse_Bungo.csv"

library(reshape2)
library(ggplot2)
library(raster)
library(foreign)
library(magick)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

load(proj.file)
load(land_req)
landuse_lut=read_csv(landuse_lut)

#READ INPUT FILE
nodata_val<-0
land.requirement.db<-land.requirement_table
lc.list<-subset(landuse_lut, select=c(ID, LC))

#Read land use map and calculate area of land use
next_data_luc<-raster(projected_land_use)
next_luc_freq<-freq(next_data_luc)
colnames(lc_freq)<-c("ID", "COUNT")
landuse_area<-as.data.frame(na.omit(lc_freq))
landuse_area<-as.matrix(landuse_area$COUNT)

next_data_luc<-raster(projected_land_use)
next_data_luc
next_landuse_lut<-landuse_lut

# landuse_area0<-as.data.frame(levels(landuse0))
landuse_area<-subset(next_landuse_lut, ID != nodata_val)
landuse_area0<-subset(landuse_lut, ID != nodata_val)
landuse_area<-as.matrix(landuse_area$COUNT)
landuse_area0<-as.matrix(landuse_area0$COUNT)
landuse_table<-cbind(lc.list,landuse_area0, landuse_area)
landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area)))
colnames(landuse_table)[1] <- "LAND_USE"
colnames(landuse_table)[2] <- "T1_HA"
colnames(landuse_table)[3] <- "T2_HA"
landuse_table<-edit(landuse_table)
landuse_table$CHANGE<-landuse_table$T2_HA-landuse_table$T1_HA

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

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Regional economic-Impact of land use change\\b0\\fs20"
test<-as.character(Sys.Date())
date<-paste("Date : ", test, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1<-"\\b\\fs24 Impact of land use change to GDP \\b0\\fs20"

# ==== Report 0. Cover=====
rtffile <- RTF("TA-Landuse_scenario_report.doc", font.size=11, width = 8.267, height = 11.692, omi = c(0,0,0,0))
# INPUT
file.copy(paste0(LUMENS_path, "/ta_cover.png"), work_dir, recursive = FALSE)
img_location<-paste0(work_dir, "/ta_cover.png")
# loading the .png image to be edited
cover <- image_read(img_location)
# to display, only requires to execute the variable name, e.g.: "> cover"
# adding text at the desired location
text_submodule <- paste("Sub-Modul Ekonomi Regional\n\nSimulasi PDRB dari Perubahan Penggunaan Lahan\n", location, ", ", "Tahun ", I_O_period, sep="")
cover_image <- image_annotate(cover, text_submodule, size = 23, gravity = "southwest", color = "white", location = "+46+220", font = "Arial")
cover_image <- image_write(cover_image)
# 'gravity' defines the 'baseline' anchor of annotation. "southwest" defines the text shoul be anchored on bottom left of the image
# 'location' defines the relative location of the text to the anchor defined in 'gravity'
# configure font type
addPng(rtffile, cover_image, width = 8.267, height = 11.692)
addPageBreak(rtffile, width = 8.267, height = 11.692, omi = c(1,1,1,1))

addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 1. Land use change\\b0\\fs20.")
addTable(rtffile,landuse_table,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,LC_graph)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 2. Impact of land use change to GDP\\b0\\fs20.")
addTable(rtffile,GDP_summary,font.size=6)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,GDP_graph)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 3. Impact of land use change to labour requirement\\b0\\fs20.")
addTable(rtffile,Labour_table,font.size=6)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300,LAB_graph)
done(rtffile)

unlink(img_location)
dbDisconnect(DB)

#=Writing final status message (code, message)
statuscode<-1
statusmessage<-"TA regional economy analysis successfully completed!"
statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)