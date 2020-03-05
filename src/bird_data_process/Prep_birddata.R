"
@author: Zsofia Koma, UvA
Aim: Pre-process bird data ( add LGN8 landcover + convert to shapefile)
"

library(rgdal)
library(raster)
library(sp)

library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)

library(spThin)
library(spatialEco)

source("D:/GitHub/eEcoLiDAR/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

# Set working dirctory
workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata/"
setwd(workingdirectory)

birdfile="avimap_observations_reedland_birds.csv" # using the one which contains more observation also outside of NL

ahn3_actimefile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/lidar_acquision/ahn3_measuretime.shp"
landcoverfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"

### Process territory mapping data

#Import
birds=read.csv(birdfile,sep=";")

ahn3_actime = readOGR(dsn=ahn3_actimefile)

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Drop data if it was measured before 2013
birds$occurrence<-1
bird=birds[birds$year>"2013",]
bird=bird[bird$species!="Roerdomp",]

# Create shp
bird_shp=CreateShape(bird)

# Intersect with year of acquision

bird_ahn3ac=raster::intersect(bird_shp,ahn3_actime)

# LGN7 add landcover classes

bird_ahn3ac_lgn8=raster::extract(landcover,bird_ahn3ac)
bird_ahn3ac$landcover_lgn8=bird_ahn3ac_lgn8[,1]

#Export
raster::shapefile(bird_ahn3ac, "Birds_territory_wextra.shp",overwrite=TRUE)

# Export per species

GrW <- subset(bird_ahn3ac, species %in% c('Grote Karekiet'))
KK <- subset(bird_ahn3ac, species %in% c('Kleine Karekiet'))
Sn <- subset(bird_ahn3ac, species %in% c('Snor'))

raster::shapefile(GrW, "GrW_territory_wextra.shp",overwrite=TRUE)
raster::shapefile(KK, "KK_territory_wextra.shp",overwrite=TRUE)
raster::shapefile(Sn, "Sn_territory_wextra.shp",overwrite=TRUE)
