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

source("D:/Koma/GitHub/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

### Set global parameters

# Set working dirctory
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v4/"
setwd(workingdirectory)

# Import

birdfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/avimap_observations_reedland_birds.csv"

# Pres-only
birds=read.csv(birdfile,sep=";")

birds$occurrence<-1
bird=birds[birds$year>"2013",]
bird=bird[bird$species!="Roerdomp",]
bird=bird[bird$species!="Baardman",]

bird_shp=CreateShape(bird)

lgn8_wetland_mask=stack("D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/onlywetland_mask/merged_mask_onlywetland.tif")

# Generate random background ((#randomsample_lidar=sampleRandom(lidar, size=100,xy=TRUE)))

d <- sdmData(occurrence~layer,train=bird_shp,predictors=lgn8_wetland_mask,bg=list(n=20000,method='gRandom',remove=TRUE))
data=d@features

background=subset(data, rID %in% d@species[["occurrence"]]@background)
background_wcoord=merge(background,d@info@coords, by.x=c('rID'), by.y=c('rID'))
names(background_wcoord)<-c("rID","lgn8","x","y")
#write.csv(background_wcoord,"background_wcoord.csv")

background_shp=CreateShape(background_wcoord)
raster::shapefile(background_shp, "background.shp",overwrite=TRUE)

# spatial thinning

background_shp_20 <- subsample.distance(background_shp, n = length(background_shp)-1, d = 20,replacement=FALSE) # size = length(background_shp)-1
raster::shapefile(background_shp_20, "background_20.shp",overwrite=TRUE)
