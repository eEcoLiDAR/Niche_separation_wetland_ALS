library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

rasterOptions(maxmemory = 100000000000)

workingdir="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/all_10m/"
setwd(workingdir)

# Work with the masked files

lidarlist=list.files(pattern = "*_onlywetland.tif")

lidar=stack(lidarlist)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# import bird and prepare data for intersection

GrW_abs=readOGR(".","GrW_genabs")
GrW_pres=readOGR(".","GrW_territory_wextra")

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

d <- sdmData(occurrence~.,train=GrW_ter,predictors = lidar)
data=d@features

write.csv(data,"birds_swet_presatl_intersected.csv")