"
@author: Zsofia Koma, UvA
Aim: Pre-process bird data ( add LGN8 landcover + convert to shapefile)
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

source("C:/Koma/Github/komazsofi/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

### Set global parameters

# Set working dirctory
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v4/"
setwd(workingdirectory)

# Import

ahn3_actimefile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/lidar_acquision/ahn3_measuretime.shp"
landcoverfile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"

lidarfile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/lidar/wh_waterfilt/lidar_metric_perc_95_normalized_height_masked_all.tif"

ahn3_actime = readOGR(dsn=ahn3_actimefile)

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

birdfile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/avimap_observations_reedland_birds.csv"

# Pres-only
birds=read.csv(birdfile,sep=";")

birds$occurrence<-1
bird=birds[birds$year>"2013",]
bird=bird[bird$species!="Roerdomp",]
bird=bird[bird$species!="Baardman",]
bird=bird[bird$species!="Kleine Karekiet",]
bird=bird[bird$species!="Snor",]

bird_shp=CreateShape(bird)

# Gen. only needed landcover + remove Groningen

lgn8_wetland_mask <- setValues(raster(landcover), NA)
lgn8_wetland_mask[landcover==16 | landcover==17 | landcover==30 | landcover==322 | landcover==323 | landcover==332 | landcover==333 | landcover==41 | landcover==42 | landcover==43 
                | landcover==45 | landcover==46 | landcover==47] <- 1

# Generate random background ((#randomsample_lidar=sampleRandom(lidar, size=100,xy=TRUE)))

d <- sdmData(occurrence~layer,train=bird_shp,predictors=landcover,bg=list(n=600,method='gRandom',remove=TRUE))
data=d@features

background=subset(data, rID %in% d@species[["occurrence"]]@background)
background_wcoord=merge(background,d@info@coords, by.x=c('rID'), by.y=c('rID'))
names(background_wcoord)<-c("rID","lgn8","x","y")

background_shp=CreateShape(background_wcoord)
raster::shapefile(background_shp, "background.shp",overwrite=TRUE)

# spatial thinning

background_shp_20 <- subsample.distance(background_shp, n = length(background_shp)-1, d = 20,replacement=FALSE) # size = length(background_shp)-1
raster::shapefile(background_shp_20, "background_20.shp",overwrite=TRUE)
