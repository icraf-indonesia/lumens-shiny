library(reshape)

# set initial variable
setwd('D:/ICRAF/Kodingan/icraf-indonesia/lumens-shiny/13_csa/rscript/tests/')
areaOfAgricTable <- read.table('data/A_SCIENDO_Luas_sawah.csv', header=T, sep=',')

# periodOfStock <- 2010
# iteration <- 10

# uniform table's name
colnames(areaOfAgricTable)<-c('TAHUN', 'SAWAH_IRIGASI', 'TADAH_HUJAN', 'LUAS_PANEN')

# TOTAL LUAS SAWAH (ha/yr) 
# A = Luas Sawah Irigasi + Luas Sawah Tadah Hujan
areaOfAgricTable$LUAS_TOTAL <- areaOfAgricTable$SAWAH_IRIGASI + areaOfAgricTable$TADAH_HUJAN

# MASA TANAM DALAM SETAHUN (Index Penanaman Irigasi)
# IP Irigasi (BASELINE) = (Luas Panen - Luas Tadah Hujan) / Luas Sawah Irigasi 
areaOfAgricTable$IP_IRIGASI1 <- (areaOfAgricTable$LUAS_PANEN - areaOfAgricTable$TADAH_HUJAN) / areaOfAgricTable$SAWAH_IRIGASI
# IP Irigasi (PROYEKSI BASELINE) = Luas Panen / Luas Sawah Irigasi 
areaOfAgricTable$IP_IRIGASI2 <- areaOfAgricTable$LUAS_PANEN / areaOfAgricTable$SAWAH_IRIGASI

# SFw = Faktor skala lahan sawah irigasi intermitten (untuk perbedaan rejim air selama pertanaman) 
#       0.49 x Luas Sawah Tadah Hujan + (Luas Panen - Luas Sawah Tadah Hujan) x 1 
# SFW = -------------------------------------------------------------------------
#                                       15
areaOfAgricTable

#Jenis Pupuk 
SF0 = vector
# Varietas
SFr = tabular
# Jenis Air Tergenang
SFw= vector
# Jenis Air Rejim Sebelum Tanam
SFp = tabular
# Jenis Tanah
SFs = tabular
# Faktor Emisi CH4 
EFc = tabular

daily_em_factor <- function(SF0, SFr, SFp, SFw, SFs, EFc){
  FEi <- EFc * SF0 * SFr * SFs * SFp * SFw
  FEi
}

daily_em_factor(10,5,1,1,1,2)
