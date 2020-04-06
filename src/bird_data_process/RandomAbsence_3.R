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

lgn8_wetland_mask=stack("D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/merged_mask_onlywetland_genrand.tif")
proj4string(lgn8_wetland_mask) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

ahn3_actimefile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/lidar_acquision/ahn3_measuretime.shp"
ahn3_actime = readOGR(dsn=ahn3_actimefile)

# km squares
kmsquaresfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/kmsquare_aslist.csv"

kmsquares_poly.df=read.csv(file=kmsquaresfile,header=TRUE,sep=",")
colnames(kmsquares_poly.df)[colnames(kmsquares_poly.df)=="sum"] <- "nofkmsquare"
names(kmsquares_poly.df) <- c("kmsquare","x","y","nofkmsquare")

kmsquares_shp=CreateShape(kmsquares_poly.df)

# Bird (pres.-only)

#birdfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/avimap_observations_reedland_birds.csv"
#birds=read.csv(birdfile,sep=";")

#birds$occurrence<-1
#bird=birds[birds$year>"2013",]
#bird=bird[bird$species!="Roerdomp",]
#bird=bird[bird$species!="Baardman",]

#write.csv(bird,"bird.csv") # create  araster in cloud compare

birds_pres=stack("D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v4/raster.tif")
proj4string(birds_pres) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Filters (landcover and presence points loc with 500m radius)

birds_abs_shp_lgn8=raster::extract(lgn8_wetland_mask,kmsquares_shp)
kmsquares_shp$mask=birds_abs_shp_lgn8[,1]
kmsquares_shp2=kmsquares_shp[which(kmsquares_shp$mask==1),]

pres=raster::extract(birds_pres,kmsquares_shp2)
kmsquares_shp2$mask_pres=pres[,1]
kmsquares_shp_filt=kmsquares_shp2[which(is.na(kmsquares_shp2$mask_pres)),]

# Generate random points

Gen_absence2(kmsquares_shp_filt,spname='Background',outname="Bgr",nofsamp=15000)
