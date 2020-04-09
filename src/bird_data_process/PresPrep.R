"
@author: Zsofia Koma, UvA
Aim: Pre-process presence bird data (territory)
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

source("D:/Koma/GitHub/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

### Set global parameters

# Set working dirctory
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v5/"
setwd(workingdirectory)

birdfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/avimap_observations_reedland_birds.csv" # using the one which contains more observation also outside of NL

landcoverfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"

# Import

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

### Process territory mapping data (pres-only)

birds=read.csv(birdfile,sep=";")

# Drop data if it was measured before 2013
birds$occurrence<-1
bird=birds[birds$year>"2013",]
bird=bird[bird$species!="Roerdomp",]
bird=bird[bird$species!="Baardman",]

# Create shp
bird_shp=CreateShape(bird)

# LGN8 add landcover classes

bird_lgn8=raster::extract(landcover,bird_shp)
bird_shp$lgn8=bird_lgn8[,1]

# Export per species

GrW <- subset(bird_shp, species %in% c('Grote Karekiet'))
KK <- subset(bird_shp, species %in% c('Kleine Karekiet'))
Sn <- subset(bird_shp, species %in% c('Snor'))

raster::shapefile(GrW, "GrW_territory.shp",overwrite=TRUE)
raster::shapefile(KK, "KK_territory.shp",overwrite=TRUE)
raster::shapefile(Sn, "Sn_territory.shp",overwrite=TRUE)

# Spatial thinning

GrW_subs_20 <- subsample.distance(GrW, size = length(GrW)-1, d = 20,replacement=FALSE) 
KK_subs_20 <- subsample.distance(KK, size= length(KK)-1, d = 20,replacement=FALSE) 
Sn_subs_20 <- subsample.distance(Sn, size = length(Sn)-1, d = 20,replacement=FALSE) 

raster::shapefile(GrW_subs_20, "GrW_territory_20.shp",overwrite=TRUE)
raster::shapefile(KK_subs_20, "KK_territory_20.shp",overwrite=TRUE)
raster::shapefile(Sn_subs_20, "Sn_territory_20.shp",overwrite=TRUE)