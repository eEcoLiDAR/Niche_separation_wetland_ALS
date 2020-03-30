library(raster)
library(rgdal)
library(sf)
library(dplyr)

library(spatialEco)
library(landscapemetrics)

library(reshape)

source("D:/Koma/GitHub/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

##

workingdir="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v4/"
setwd(workingdir)

lidarfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/lidar/wh_waterfilt/lidar_metric_perc_95_normalized_height_masked_all.tif"

lidar=stack(lidarfile)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

GrW=read.csv("GrW_territory_intersected.csv")
GrW_shp=CreateShape(GrW)

KK=read.csv("KK_territory_intersected.csv")
KK_shp=CreateShape(KK)

Sn=read.csv("Sn_territory_intersected.csv")
Sn_shp=CreateShape(Sn)

# Reclassify
height_class=reclassify(lidar, c(-Inf,1,1,1,3,2,3,5,3,5,Inf,4))
writeRaster(height_class,"height_classified.tif",overwrite=TRUE)

# calculate landscape metrics

my_metric_npte_GrW = sample_lsm(height_class, GrW_shp,size=100,level = "class", metric = c("np","te"),plot_id=GrW_shp@data$rID,return_raster=TRUE,count_boundary = FALSE,directions = 8)
df_GrW=cast(my_metric_npte_GrW ,plot_id~metric+class)

my_metric_npte_KK = sample_lsm(height_class, KK_shp,size=100,level = "class", metric = c("np","te"),plot_id=KK_shp@data$rID,return_raster=TRUE,count_boundary = FALSE,directions = 8)
df_KK=cast(my_metric_npte_KK ,plot_id~metric+class)

my_metric_npte_Sn = sample_lsm(height_class, Sn_shp,size=100,level = "class", metric = c("np","te"),plot_id=Sn_shp@data$rID,return_raster=TRUE,count_boundary = FALSE,directions = 8)
df_Sn=cast(my_metric_npte_Sn ,plot_id~metric+class)

# add to the intersected data
GrW_wlandsc=merge(GrW,df_GrW, by.x=c('rID'), by.y=c('plot_id'))
write.csv(GrW_wlandsc,"GrW_wlandsc.csv")

KK_wlandsc=merge(KK,df_KK, by.x=c('rID'), by.y=c('plot_id'))
write.csv(KK_wlandsc,"KK_wlandsc.csv")

Sn_wlandsc=merge(Sn,df_Sn, by.x=c('rID'), by.y=c('plot_id'))
write.csv(Sn_wlandsc,"Sn_wlandsc.csv")
