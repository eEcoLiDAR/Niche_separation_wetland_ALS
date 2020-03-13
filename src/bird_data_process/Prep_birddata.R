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

source("D:/Koma/GitHub/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

### Set global parameters

# Set working dirctory
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v2/"
setwd(workingdirectory)

birdfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/avimap_observations_reedland_birds.csv" # using the one which contains more observation also outside of NL
atl_pres="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/Breeding_bird_atlas_individual_observations.csv" # pres. observation in kmsquares

ahn3_actimefile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/lidar_acquision/ahn3_measuretime.shp"
landcoverfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"

kmsquaresfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/kmsquare_aslist.csv"
km_obs_file="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/Input/Breeding_bird_atlas_aggregated_data_kmsquares.csv"

lidarfile="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/all_10m/onlywetland/lidar_metric_perc_95_normalized_height_masked_all_onlywetland.tif"

# Import

ahn3_actime = readOGR(dsn=ahn3_actimefile)

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

### Process territory mapping data (pres-only)

birds=read.csv(birdfile,sep=";")

# Drop data if it was measured before 2013
birds$occurrence<-1
bird=birds[birds$year>"2013",]
bird=bird[bird$species!="Roerdomp",]

# Create shp
bird_shp=CreateShape(bird)

# Intersect with year of acquision

bird_ahn3ac=raster::intersect(bird_shp,ahn3_actime)

# LGN8 add landcover classes

bird_ahn3ac_lgn8=raster::extract(landcover,bird_ahn3ac)
bird_ahn3ac$landcover_lgn8=bird_ahn3ac_lgn8[,1]

# lidar
bird_ahn3ac_lidar=raster::extract(lidar,bird_ahn3ac)
bird_ahn3ac$lidar=bird_ahn3ac_lidar[,1]

#Apply filters

bird_ahn3ac_filt <- subset(bird_ahn3ac, landcover_lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))

#bird_ahn3ac_filt=bird_ahn3ac_filt[is.na(bird_ahn3ac_filt$lidar),]
bird_ahn3ac_filt=bird_ahn3ac_filt[bird_ahn3ac_filt$Jaar!=2019,]

#Export
#raster::shapefile(bird_ahn3ac, "Birds_territory_wextra.shp",overwrite=TRUE)

# Export per species

GrW <- subset(bird_ahn3ac_filt, species %in% c('Grote Karekiet'))
KK <- subset(bird_ahn3ac_filt, species %in% c('Kleine Karekiet'))
Sn <- subset(bird_ahn3ac_filt, species %in% c('Snor'))

#raster::shapefile(GrW, "GrW_territory.shp",overwrite=TRUE)
#raster::shapefile(KK, "KK_territory.shp",overwrite=TRUE)
#raster::shapefile(Sn, "Sn_territory.shp",overwrite=TRUE)

# Spatial thinning

GrW_subs_20 <- subsample.distance(GrW, size = length(GrW)-1, d = 20,replacement=FALSE) 
KK_subs_20 <- subsample.distance(KK, size= length(KK)-1, d = 20,replacement=FALSE) 
Sn_subs_20 <- subsample.distance(Sn, size = length(Sn)-1, d = 20,replacement=FALSE) 

raster::shapefile(GrW_subs_20, "GrW_territory_20.shp",overwrite=TRUE)
raster::shapefile(KK_subs_20, "KK_territory_20.shp",overwrite=TRUE)
raster::shapefile(Sn_subs_20, "Sn_territory_20.shp",overwrite=TRUE)

### Process presence in atlas mapping data 

presatl_birds=read.csv(atl_pres,sep=";")

names(presatl_birds)[14]<-"x"
names(presatl_birds)[15]<-"y"
names(presatl_birds)[17]<-"occurrence"

# Drop data if it was measured before 2013

presatl_birds<-presatl_birds[!(presatl_birds$species=="Roerdomp"),]

birds_wfilt_presatl=presatl_birds[presatl_birds$year>"2013",]

birds_wfilt_presatl_shp=CreateShape(birds_wfilt_presatl)

# Add lgn8 attribute

birds_wfilt_presatl_shp_lgn8=raster::extract(landcover,birds_wfilt_presatl_shp)
birds_wfilt_presatl_shp$landcover_lgn8=birds_wfilt_presatl_shp_lgn8[,1]

#Export

raster::shapefile(birds_wfilt_presatl_shp, "birds_presatl.shp",overwrite=TRUE)

# Export per species
GrW_atl <- subset(birds_wfilt_presatl_shp, species %in% c('Grote Karekiet'))
KK_atl <- subset(birds_wfilt_presatl_shp, species %in% c('Kleine Karekiet'))
Sn_atl <- subset(birds_wfilt_presatl_shp, species %in% c('Snor'))
Se_atl <- subset(birds_wfilt_presatl_shp, species %in% c('Rietzanger'))

raster::shapefile(GrW_atl, "GrW_presatl_wextra.shp",overwrite=TRUE)
raster::shapefile(KK_atl, "KK_presatl_wextra.shp",overwrite=TRUE)
raster::shapefile(Sn_atl, "Sn_presatl_wextra.shp",overwrite=TRUE)
raster::shapefile(Se_atl, "Se_presatl_wextra.shp",overwrite=TRUE)

### Process absence atlas data
kmsquares_poly.df=read.csv(file=kmsquaresfile,header=TRUE,sep=",")
colnames(kmsquares_poly.df)[colnames(kmsquares_poly.df)=="sum"] <- "nofkmsquare"

km_obs=read.csv(file=km_obs_file,header=TRUE,sep=";")
km_obs=km_obs[ which(km_obs$year>2013 & km_obs$species!="Roerdomp"),]

birds_abs=km_obs[ which(km_obs$present==0),]

birds_abs_kmcoord=merge(birds_abs,kmsquares_poly.df,by="kmsquare",all.x = TRUE)
names(birds_abs_kmcoord)[4]<-"x_5km"
names(birds_abs_kmcoord)[5]<-"y_5km"
names(birds_abs_kmcoord)[14]<-"occurrence"

names(birds_abs_kmcoord)[15]<-"x"
names(birds_abs_kmcoord)[16]<-"y"

birds_abs_shp=CreateShape(birds_abs_kmcoord)

# Add lgn8 and lidar availability attribute

birds_abs_shp_lgn8=raster::extract(landcover,birds_abs_shp)
birds_abs_shp$landcover_lgn8=birds_abs_shp_lgn8[,1]

birds_abs_shp_lidar=raster::extract(lidar,birds_abs_shp)
birds_abs_shp$lidar=birds_abs_shp_lidar[,1]

birds_abs_shp_wlidar=raster::intersect(birds_abs_shp,ahn3_actime)

#Export

#raster::shapefile(birds_abs_shp, "birds_absatl.shp",overwrite=TRUE)

# Export per species
GrW_atl_abs <- subset(birds_abs_shp_wlidar, species %in% c('Grote Karekiet'))
KK_atl_abs <- subset(birds_abs_shp_wlidar, species %in% c('Kleine Karekiet'))
Sn_atl_abs <- subset(birds_abs_shp_wlidar, species %in% c('Snor'))
Se_atl_abs <- subset(birds_abs_shp_wlidar, species %in% c('Rietzanger'))

#raster::shapefile(GrW_atl_abs, "GrW_absatl_wextra.shp",overwrite=TRUE)
#raster::shapefile(KK_atl_abs, "KK_absatl_wextra.shp",overwrite=TRUE)
#raster::shapefile(Sn_atl_abs, "Sn_absatl_wextra.shp",overwrite=TRUE)
#raster::shapefile(Se_atl_abs, "Se_absatl_wextra.shp",overwrite=TRUE)

### Create absences based on atlas data (for territory mapping data)

Gen_absence(GrW_atl_abs,spname='Grote Karekiet',outname="GrW_genabs",nofsamp=2*157)
Gen_absence(KK_atl_abs,spname='Kleine Karekiet',outname="KK_genabs",nofsamp=2*14297)
Gen_absence(Sn_atl_abs,spname='Snor',outname="Sn_genabs",nofsamp=2*1161)

