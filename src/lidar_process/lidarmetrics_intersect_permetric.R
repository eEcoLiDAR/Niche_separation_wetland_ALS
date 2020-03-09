library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

rasterOptions(maxmemory = 100000000000)

workingdir="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/all_10m/onlywetland/"
setwd(workingdir)

# Work with the masked files

lidarlist=list.files(pattern = "*_onlywetland.tif")

lidar=stack(lidarlist)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

## import GrW and prepare data for intersection

GrW_abs=readOGR(".","GrW_genabs")
GrW_pres=readOGR(".","GrW_territory_subs_20")

GrW_pres.df <- as(GrW_pres, "data.frame")
GrW_pres.df$id<- seq(1,length(GrW_pres.df$occrrn))

GrW_pres_sel=subset(GrW_pres.df, select=c("coords.x1","coords.x2","species","occrrnc","id"))
names(GrW_pres_sel)<- c("coords.x1","coords.x2","species","occurrence","id")

GrW_abs.df <- as(GrW_abs, "data.frame")
GrW_abs_sel=subset(GrW_abs.df, select=c("coords.x1","coords.x2","species","occurrence"))
GrW_abs_sel$id <- 0

GrW_ter=rbind(GrW_pres_sel,GrW_abs_sel)

coordinates(GrW_ter)=~coords.x1+coords.x2
proj4string(GrW_ter)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# intersect
d <- sdmData(occurrence~.,train=GrW_ter,predictors = lidar)
data=d@features

data$occurrence <- 0
data$occurrence[1:length(d@species[["occurrence"]]@presence)] <- 1

write.csv(data,"GrW_territory_intersected.csv")

## import Sn and prepare data for intersection

Sn_abs=readOGR(".","Sn_genabs")
Sn_pres=readOGR(".","Sn_territory_subs_20")

Sn_pres.df <- as(Sn_pres, "data.frame")
Sn_pres.df$id<- seq(1,length(Sn_pres.df$occrrn))

Sn_pres_sel=subset(Sn_pres.df, select=c("coords.x1","coords.x2","species","occrrnc","id"))
names(Sn_pres_sel)<- c("coords.x1","coords.x2","species","occurrence","id")

Sn_abs.df <- as(Sn_abs, "data.frame")
Sn_abs_sel=subset(Sn_abs.df, select=c("coords.x1","coords.x2","species","occurrence"))
Sn_abs_sel$id <- 0

Sn_ter=rbind(Sn_pres_sel,Sn_abs_sel)

coordinates(Sn_ter)=~coords.x1+coords.x2
proj4string(Sn_ter)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# intersect
d <- sdmData(occurrence~.,train=Sn_ter,predictors = lidar)
data=d@features

data$occurrence <- 0
data$occurrence[1:length(d@species[["occurrence"]]@presence)] <- 1

write.csv(data,"Sn_territory_intersected.csv")

## import KK and prepare data for intersection

KK_abs=readOGR(".","KK_genabs")
KK_pres=readOGR(".","KK_territory_subs_20")

KK_pres.df <- as(KK_pres, "data.frame")
KK_pres.df$id<- seq(1,length(KK_pres.df$occrrn))

KK_pres_sel=subset(KK_pres.df, select=c("coords.x1","coords.x2","species","occrrnc","id"))
names(KK_pres_sel)<- c("coords.x1","coords.x2","species","occurrence","id")

KK_abs.df <- as(KK_abs, "data.frame")
KK_abs_sel=subset(KK_abs.df, select=c("coords.x1","coords.x2","species","occurrence"))
KK_abs_sel$id <- 0

KK_ter=rbind(KK_pres_sel,KK_abs_sel)

coordinates(KK_ter)=~coords.x1+coords.x2
proj4string(KK_ter)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# intersect
d <- sdmData(occurrence~.,train=KK_ter,predictors = lidar)
data=d@features

data$occurrence <- 0
data$occurrence[1:length(d@species[["occurrence"]]@presence)] <- 1

write.csv(data,"KK_territory_intersected.csv")
