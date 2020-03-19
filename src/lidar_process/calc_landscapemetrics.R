library(raster)
library(rgdal)
library(sf)
library(dplyr)

library(fasterize)

library(spatialEco)
library(landscapemetrics)

library(reshape)

source("C:/Koma/Github/komazsofi/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

##

workingdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
setwd(workingdir)

lidarfile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/lidar/wh_waterfilt/lidar_metric_perc_95_normalized_height_masked_all.tif"

lidar=stack(lidarfile)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

GrW=read.csv("GrW_territory_intersected.csv")
GrW_shp=CreateShape(GrW)

# Reclassify
height_class=reclassify(lidar, c(-Inf,1,1,1,5,2,5,Inf,3))

# calculate landscape metrics

my_metric_npte = sample_lsm(height_class, GrW_shp,size=100,level = "class", metric = c("np","te"),plot_id=GrW_shp@data$rID,return_raster=TRUE,count_boundary = FALSE,directions = 8)
df=cast(my_metric_npte ,plot_id~metric+class)

# add to the intersected data
GrW_wlandsc=merge(GrW,df, by.x=c('rID'), by.y=c('plot_id'))
