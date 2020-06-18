"
@author: Zsofia Koma, UvA
Aim: Place absence into survey plots
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
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v6/"
setwd(workingdirectory)

territoryidfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMP_number_of_territories_per_plot_per_year.csv"
plot1file="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMPplots_12380.shp"
plot2file="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMPplots_12510.shp"
plot3file="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMPplots_12530.shp"

lgn8_wetland_mask=stack("D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/merged_mask_onlywetland_genrand.tif")
proj4string(lgn8_wetland_mask) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

birds_pres=stack("D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v4/Presonly_200mbuffer.tif")
proj4string(birds_pres) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Import

territoryid=read.csv(territoryidfile,sep=";")

territoryid=territoryid[territoryid$jaar>2013 & territoryid$jaar<2018,]
territoryid=territoryid[territoryid$naam!="Roerdomp",]
territoryid=territoryid[territoryid$naam!="Baardman",]

# Which survey plots are applicable
plots=unique(territoryid$euring)

# GrW
GrW=territoryid[territoryid$naam=="Grote Karekiet",]
#unique(GrW$euring)

# Import relevant survey plots

surveyplot1 = readOGR(dsn=plot3file)
#unique(surveyplot1@data$plotid)

surveyplot1_GrW=subset(surveyplot1, plotid %in% unique(GrW$plotid))
#plot(surveyplot1_GrW)
raster::shapefile(surveyplot1_GrW, "surveyplot1_GrW.shp",overwrite=TRUE)

# KK
KK=territoryid[territoryid$naam=="Kleine Karekiet",]
#unique(KK$euring)

# Import relevant survey plots

surveyplot2 = readOGR(dsn=plot2file)

surveyplot2_KK=subset(surveyplot2, plotid %in% unique(KK$plotid))
#plot(surveyplot2_KK)

# Sn
Sn=territoryid[territoryid$naam=="Snor",]
#unique(Sn$euring)

# Import relevant survey plots

surveyplot3 = readOGR(dsn=plot1file)

surveyplot3_Sn=subset(surveyplot3, plotid %in% unique(Sn$plotid))
#plot(surveyplot3_Sn)

#Union

survey_union <- bind(surveyplot1_GrW, surveyplot2_KK,surveyplot3_Sn)
proj4string(survey_union) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

survey_union=unionSpatialPolygons(survey_union,rep(1, length(survey_union)))

raster::shapefile(survey_union, "survey_union.shp",overwrite=TRUE)

# place points randomly
survey_union_genabs=spsample(survey_union,n=150000,"random",iter=10)
survey_union_genabs.df=as.data.frame(survey_union_genabs)

survey_union_genabs.df$species <- "Background"
survey_union_genabs.df$occurrence <- 0

survey_union_genabs_shp=CreateShape(survey_union_genabs.df)

#delete the ones which are too close to presence

birds_abs_shp=raster::extract(birds_pres,survey_union_genabs_shp)
survey_union_genabs_shp$mask=birds_abs_shp[,1]

raster::shapefile(survey_union_genabs_shp, "Bgr_prefilt.shp",overwrite=TRUE)

birds_abs_shp2=survey_union_genabs_shp[which(is.na(survey_union_genabs_shp$mask)),]

birds_abs_shp2_lgn8=raster::extract(lgn8_wetland_mask,birds_abs_shp2)
birds_abs_shp2$lgn8=birds_abs_shp2_lgn8[,1]

raster::shapefile(birds_abs_shp2, "Bgr_200m.shp",overwrite=TRUE)
