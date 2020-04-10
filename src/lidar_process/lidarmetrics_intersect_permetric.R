library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

## function for intersection

IntersectForAnal_single = function(GrW_pres,landcover,lidar,name='GrW') {
  GrW_pres.df <- as(GrW_pres, "data.frame")
  GrW_pres.df$x<- GrW_pres.df$coords.x1
  GrW_pres.df$y<- GrW_pres.df$coords.x2
  
  GrW_pres_sel=subset(GrW_pres.df, select=c("coords.x1","coords.x2","species","occrrnc","x","y"))
  names(GrW_pres_sel)<- c("coords.x1","coords.x2","species","occurrence","x","y")
  
  coordinates(GrW_pres_sel)=~coords.x1+coords.x2
  proj4string(GrW_pres_sel)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  # add lgn8 info
  GrW_ter_lgn8=raster::extract(landcover,GrW_pres_sel)
  GrW_pres_sel$lgn8=GrW_ter_lgn8[,1]
  
  # intersect
  d <- sdmData(occurrence~.,train=GrW_pres_sel,predictors = lidar)
  write.sdm(d,name,overwrite=TRUE) 
  data=d@features
  
  data$occurrence <- 0
  data$occurrence[1:length(d@species[["occurrence"]]@presence)] <- 1
  
  return(data)
  
}

rasterOptions(maxmemory = 100000000000)

workingdir="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v6/"
setwd(workingdir)

# Work with the masked files

lidarlist=list.files(path="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/lidar/wh_waterfilt/",pattern = "*.tif",full.names = TRUE)
landcoverfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"

lidar=stack(lidarlist)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

## GrW

GrW_pres=readOGR(dsn="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v5/GrW_territory.shp")

data_GrW=IntersectForAnal_single(GrW_pres,landcover,lidar,name='GrW')

write.csv(data_GrW,"GrW_territory_intersected.csv")

## Sn

Sn_pres=readOGR(dsn="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v5/Sn_territory.shp")

data_Sn=IntersectForAnal_single(Sn_pres,landcover,lidar,name='Sn')

write.csv(data_Sn,"Sn_territory_intersected.csv")

## KK

KK_pres=readOGR(dsn="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v5/KK_territory.shp")

data_KK=IntersectForAnal_single(KK_pres,landcover,lidar,name='KK')

write.csv(data_KK,"KK_territory_intersected.csv")

## Background
Bgr=readOGR(dsn="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v5/Bgr.shp")
names(Bgr)<- c("coords.x1","coords.x2","species","occrrnc")

data_Bgr=IntersectForAnal_single(Bgr,landcover,lidar,name='Background')

write.csv(data_Bgr,"Bgr_territory_intersected.csv")

Bgr_lgn8 <- subset(data_Bgr, lgn8 %in% c(16,17,30,322,332,41,42,43))
