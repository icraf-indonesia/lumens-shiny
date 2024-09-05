library(terra)
library(dplyr)

syncGeom <- function(input, ref){
  input %>% 
    #rast() %>% 
    crop(ref) %>% 
    resample(ref) %>% 
    `*`(ref)
}

#input data

r_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/RainfallErosivity_lup_EbM.tif" %>% rast()
k_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/ErodibilityUTM_EbM.tif"%>% rast()
ls_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/LS_EbM.tif"%>% rast()
cp_file <- "D:/OneDrive - CIFOR-ICRAF/ICRAF/LUMENS/LUMENS for Brazil ToT/QUES-H/quesh_data/Dataset_clip/cp00_lup_EbM.tif"%>% rast()
zone <- "data/raster/Zona_Bungo.tif"

output_directory <- "data/data_quesh/"


# load a reference map
base_map <- rast(zone) %>% classify(cbind(1:8, 1))

# load factors map
r_map <- syncGeom(input = r_file, ref = base_map)
writeRaster(r_map, paste0(output_directory, "R_factor.tif"))


# load factors map
k_map <- syncGeom(input = k_file, ref = base_map)
writeRaster(k_map, paste0(output_directory, "K_factor.tif"))

# load ls map
ls_map <- syncGeom(input = ls_file, ref = base_map)
writeRaster(ls_map, paste0(output_directory, "LS_factor.tif"))

# load cp map
cp_map <- syncGeom(input = cp_file, ref = base_map)
writeRaster(k_map, paste0(output_directory, "CP_factor.tif"))

# # calculate RUSLE
a <- r_map*k_map*ls_map*cp_map
plot(a)

a_log <- log10(a)
plot(a_log)
