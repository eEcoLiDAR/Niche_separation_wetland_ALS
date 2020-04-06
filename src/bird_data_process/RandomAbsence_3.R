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

# Bird (pres.-only)

birdfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/avimap_observations_reedland_birds.csv"
birds=read.csv(birdfile,sep=";")

birds$occurrence<-1
bird=birds[birds$year>"2013",]
bird=bird[bird$species!="Roerdomp",]
bird=bird[bird$species!="Baardman",]

bird_shp=CreateShape(bird)

Presonly_b=st_buffer(st_as_sf(bird_shp), 25)
Presonly_b_sp <- sf:::as_Spatial(Presonly_b)

Presonly_b_sp_union <- unionSpatialPolygons(Presonly_b_sp,rep(1, length(Presonly_b_sp)))
Presonly_b_sp_union$terrytory <- 1

# km squares
kmsquaresfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/kmsquare_aslist.csv"

kmsquares_poly.df=read.csv(file=kmsquaresfile,header=TRUE,sep=",")
colnames(kmsquares_poly.df)[colnames(kmsquares_poly.df)=="sum"] <- "nofkmsquare"
names(kmsquares_poly.df) <- c("kmsquare","x","y","nofkmsquare")

kmsquares_shp=CreateShape(kmsquares_poly.df)

# apply that only within lidar and relevant landcover
birds_abs_shp_lgn8=raster::extract(lgn8_wetland_mask,kmsquares_shp)
kmsquares_shp$mask=birds_abs_shp_lgn8[,1]

kmsquares_shp_filt <- subset(kmsquares_shp, mask %in% c(1))

kmsquares_shp_filt_presonly=raster::intersect(kmsquares_shp_filt,Presonly_b_sp_union) # only reamin where we have presence???

# Generate random points

Gen_absence2(kmsquares_shp_filt_presonly,spname='Background',outname="Bgr",nofsamp=15000)
