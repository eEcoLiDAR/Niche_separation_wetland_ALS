library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

rasterOptions(maxmemory = 100000000000)

workingdir="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/all_10m/"
setwd(workingdir)

# import lidar metrics
filelist=list.files(pattern = "*.tif")

lidar=stack(filelist)

lidar_sel <- subset(lidar, c(1,2,3,4,6,9,12,15,19,20), drop=FALSE)
plot(lidar_sel)
proj4string(lidar_sel) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")



