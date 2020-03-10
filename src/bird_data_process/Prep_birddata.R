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

source("D:/GitHub/eEcoLiDAR/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

### Set global parameters

# Set working dirctory
workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata/"
setwd(workingdirectory)

birdfile="avimap_observations_reedland_birds.csv" # using the one which contains more observation also outside of NL
atl_pres="Breeding_bird_atlas_individual_observations.csv" # pres. observation in kmsquares

ahn3_actimefile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/lidar_acquision/ahn3_measuretime.shp"
landcoverfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"

kmsquaresfile="kmsquare_aslist.csv"
km_obs_file="Breeding_bird_atlas_aggregated_data_kmsquares.csv"

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

# LGN7 add landcover classes

bird_ahn3ac_lgn8=raster::extract(landcover,bird_ahn3ac)
bird_ahn3ac$landcover_lgn8=bird_ahn3ac_lgn8[,1]

# Year match
bird_ahn3ac$acq_sync <- bird_ahn3ac$year == bird_ahn3ac$Jaar
bird_terr_timematch_shp=bird_ahn3ac[bird_ahn3ac$acq_sync==TRUE,]

#Export
raster::shapefile(bird_ahn3ac, "Birds_territory_wextra.shp",overwrite=TRUE)

# Export per species

GrW <- subset(bird_ahn3ac, species %in% c('Grote Karekiet'))
KK <- subset(bird_ahn3ac, species %in% c('Kleine Karekiet'))
Sn <- subset(bird_ahn3ac, species %in% c('Snor'))

raster::shapefile(GrW, "GrW_territory.shp",overwrite=TRUE)
raster::shapefile(KK, "KK_territory.shp",overwrite=TRUE)
raster::shapefile(Sn, "Sn_territory.shp",overwrite=TRUE)

# Export per species with time match

GrW_t <- subset(bird_terr_timematch_shp, species %in% c('Grote Karekiet'))
KK_t <- subset(bird_terr_timematch_shp, species %in% c('Kleine Karekiet'))
Sn_t <- subset(bird_terr_timematch_shp, species %in% c('Snor'))

raster::shapefile(GrW_t, "GrW_territory_timematch.shp",overwrite=TRUE)
raster::shapefile(KK_t, "KK_territory_timematch.shp",overwrite=TRUE)
raster::shapefile(Sn_t, "Sn_territory_timematch.shp",overwrite=TRUE)

# Spatial thinning

GrW_subs_20 <- subsample.distance(GrW, n = 545, d = 20,replacement=FALSE) 
KK_subs_20 <- subsample.distance(KK, n = 5000, d = 20,replacement=FALSE) # total would be 105982
Sn_subs_20 <- subsample.distance(Sn, n = 5000, d = 20,replacement=FALSE) # total would be 11765

raster::shapefile(GrW_subs_20, "GrW_territory_subsall_20.shp",overwrite=TRUE)
raster::shapefile(KK_subs_20, "KK_territory_subsall_20.shp",overwrite=TRUE)
raster::shapefile(Sn_subs_20, "Sn_territory_subsall_20.shp",overwrite=TRUE)

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

# Add lgn8 attribute

birds_abs_shp_lgn8=raster::extract(landcover,birds_abs_shp)
birds_abs_shp$landcover_lgn8=birds_abs_shp_lgn8[,1]

#Export

raster::shapefile(birds_abs_shp, "birds_absatl.shp",overwrite=TRUE)

# Export per species
GrW_atl_abs <- subset(birds_abs_shp, species %in% c('Grote Karekiet'))
KK_atl_abs <- subset(birds_abs_shp, species %in% c('Kleine Karekiet'))
Sn_atl_abs <- subset(birds_abs_shp, species %in% c('Snor'))
Se_atl_abs <- subset(birds_abs_shp, species %in% c('Rietzanger'))

raster::shapefile(GrW_atl_abs, "GrW_absatl_wextra.shp",overwrite=TRUE)
raster::shapefile(KK_atl_abs, "KK_absatl_wextra.shp",overwrite=TRUE)
raster::shapefile(Sn_atl_abs, "Sn_absatl_wextra.shp",overwrite=TRUE)
raster::shapefile(Se_atl_abs, "Se_absatl_wextra.shp",overwrite=TRUE)

### Create absences based on atlas data (for territory mapping data)
# GrW
factor(GrW_atl_abs$landcover_lgn8)
GrW_atl_abs_lgn8 <- subset(GrW_atl_abs, landcover_lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))

GrW_atl_abs_b=st_buffer(st_as_sf(GrW_atl_abs_lgn8), 1000)
GrW_atl_abs_b_sp <- sf:::as_Spatial(GrW_atl_abs_b)

GrW_atl_abs_b_sp_union <- unionSpatialPolygons(GrW_atl_abs_b_sp,rep(1, length(GrW_atl_abs_b_sp)))

raster::shapefile(GrW_atl_abs_b_sp_union, "GrW_atl_abs_lg8_b1km_union.shp",overwrite=TRUE)

GrW_genabs=spsample(GrW_atl_abs_b_sp_union,n=5000,"random")
GrW_genabs.df=as.data.frame(GrW_genabs)

GrW_genabs.df$species <- 'Grote Karekiet'
GrW_genabs.df$occurrence <- 0

GrW_genabs.df_shp=CreateShape(GrW_genabs.df)
raster::shapefile(GrW_genabs.df_shp, "GrW_genabs_5000.shp",overwrite=TRUE)

# KK
factor(KK_atl_abs$landcover_lgn8)
KK_atl_abs_lgn8 <- subset(KK_atl_abs, landcover_lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))

KK_atl_abs_b=st_buffer(st_as_sf(KK_atl_abs_lgn8), 1000)
KK_atl_abs_b_sp <- sf:::as_Spatial(KK_atl_abs_b)

KK_atl_abs_b_sp_union <- unionSpatialPolygons(KK_atl_abs_b_sp,rep(1, length(KK_atl_abs_b_sp)))

raster::shapefile(KK_atl_abs_b_sp_union, "KK_atl_abs_lg8_b1km_union.shp",overwrite=TRUE)

KK_genabs=spsample(KK_atl_abs_b_sp_union,n=5000,"random")
KK_genabs.df=as.data.frame(KK_genabs)

KK_genabs.df$species <- 'Kleine Karekiet'
KK_genabs.df$occurrence <- 0

KK_genabs.df_shp=CreateShape(KK_genabs.df)
raster::shapefile(KK_genabs.df_shp, "KK_genabs_5000.shp",overwrite=TRUE)

# Sn
factor(Sn_atl_abs$landcover_lgn8)
Sn_atl_abs_lgn8 <- subset(Sn_atl_abs, landcover_lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))

Sn_atl_abs_b=st_buffer(st_as_sf(Sn_atl_abs_lgn8), 1000)
Sn_atl_abs_b_sp <- sf:::as_Spatial(Sn_atl_abs_b)

Sn_atl_abs_b_sp_union <- unionSpatialPolygons(Sn_atl_abs_b_sp,rep(1, length(Sn_atl_abs_b_sp)))

raster::shapefile(Sn_atl_abs_b_sp_union, "Sn_atl_abs_lg8_b1km_union.shp",overwrite=TRUE)

Sn_genabs=spsample(Sn_atl_abs_b_sp_union,n=5000,"random")
Sn_genabs.df=as.data.frame(Sn_genabs)

Sn_genabs.df$species <- 'Snor'
Sn_genabs.df$occurrence <- 0

Sn_genabs.df_shp=CreateShape(Sn_genabs.df)
raster::shapefile(Sn_genabs.df_shp, "Sn_genabs_5000.shp",overwrite=TRUE)

