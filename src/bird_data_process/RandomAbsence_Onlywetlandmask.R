"
@author: Zsofia Koma, UvA
Aim: Place absence into survey plots
"

library(rgdal)
library(raster)
library(sp)
library(sf)
library(maptools)

library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)

library(spThin)
library(spatialEco)

library(snow)
library(sdm)

source("D:/Koma/GitHub/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

### Set global parameters

# Set working dirctory
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v6/"
setwd(workingdirectory)

lgn8_wetland_mask=stack("C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/merged_mask_onlywetland_genrand.tif")
proj4string(lgn8_wetland_mask) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

birds_pres=stack("C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v4/Presonly_200mbuffer.tif")
proj4string(birds_pres) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Place random points in raster
background=sampleRandom(lgn8_wetland_mask, size=150000, cells=FALSE,xy=TRUE,sp=TRUE,na.rm=TRUE)

raster::shapefile(background, "Background_whfilt.shp",overwrite=TRUE)
