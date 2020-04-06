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
birds=read.csv(birdfile,sep=";")

birds$occurrence<-1
bird=birds[birds$year>"2013",]
bird=bird[bird$species!="Roerdomp",]
bird=bird[bird$species!="Baardman",]

bird_shp=CreateShape(bird)

lgn8_wetland_mask=stack("D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/merged_mask_onlywetland_genrand.tif")

# km squares
kmsquaresfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/kmsquare_aslist.csv"
km_obs_file="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/Breeding_bird_atlas_aggregated_data_kmsquares.csv"

kmsquares_poly.df=read.csv(file=kmsquaresfile,header=TRUE,sep=",")
colnames(kmsquares_poly.df)[colnames(kmsquares_poly.df)=="sum"] <- "nofkmsquare"

km_obs=read.csv(file=km_obs_file,header=TRUE,sep=";")
km_obs=km_obs[ which(km_obs$year>2013 & km_obs$species!="Roerdomp" & km_obs$species!="Baardman"),]

birds_abs=km_obs[ which(km_obs$present==0),]

birds_abs_kmcoord=merge(birds_abs,kmsquares_poly.df,by="kmsquare",all.x = TRUE)
names(birds_abs_kmcoord)[4]<-"x_5km"
names(birds_abs_kmcoord)[5]<-"y_5km"
names(birds_abs_kmcoord)[14]<-"occurrence"

names(birds_abs_kmcoord)[15]<-"x"
names(birds_abs_kmcoord)[16]<-"y"

birds_abs_shp=CreateShape(birds_abs_kmcoord)

# apply that only within lidar and relevant landcover
birds_abs_shp_lgn8=raster::extract(lgn8_wetland_mask,birds_abs_shp)
birds_abs_shp$landcover_lgn8=birds_abs_shp_lgn8[,1]

birds_abs_shp_filt <- subset(birds_abs_shp, landcover_lgn8 %in% c(1))

Gen_absence2(birds_abs_shp_filt,spname='Background',outname="Background_genabs",nofsamp=15000)
