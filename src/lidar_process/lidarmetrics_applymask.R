library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

rasterOptions(maxmemory = 100000000000)

workingdir="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/all_10m/"
setwd(workingdir)

# import lidar metrics (before masking for wetlands)
filelist=list.files(pattern = "*.tif")

lidar=stack(filelist)

#lidar_sel <- subset(lidar, c(1,2,3,4,6,9,12,15,19,20), drop=FALSE)
lidar_sel <- subset(lidar, c(7,8,10,11,13,14), drop=FALSE)
proj4string(lidar_sel) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# import wetland mask (enough to do once)

wetland_mask=stack("D:/Koma/_PhD/Chapter3/Data_Preprocess/input_formask/merged_mask_onlywetland.tif")
proj4string(wetland_mask) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# mask

lidar_masked <- mask(lidar_sel, wetland_mask)
writeRaster(lidar_masked,filename=paste(names(lidar_masked),"_onlywetland",sep=""), bylayer=TRUE,format="GTiff",overwrite=TRUE)
plot(lidar_masked)