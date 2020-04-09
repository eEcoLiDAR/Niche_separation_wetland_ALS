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

source("C:/Koma/Github/komazsofi/PhDPaper2_wetlandniche/src/bird_data_process/Func_ProcessOcc.R")

### Set global parameters

# Set working dirctory
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Process_birddata_v5/"
setwd(workingdirectory)

territoryidfile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMP_number_of_territories_per_plot_per_year.csv"
plot1file="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMPplots_12380.shp"
plot2file="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMPplots_12510.shp"
plot3file="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/bird_data/BMP_plots_reedland_birds/BMPplots_12530.shp"

# Import

territoryid=read.csv(territoryidfile,sep=";")

territoryid=territoryid[territoryid$jaar>2013 & territoryid$jaar<2018,]
territoryid=territoryid[territoryid$naam!="Roerdomp",]
territoryid=territoryid[territoryid$naam!="Baardman",]

# Which survey plots are applicable
plots=unique(territoryid$euring)

# GrW
GrW=territoryid[territoryid$naam=="Grote Karekiet",]
unique(GrW$euring)

# Import relevant survey plots

surveyplot1 = readOGR(dsn=plot3file)
unique(surveyplot1@data$plotid)

surveyplot1_GrW=subset(surveyplot1, plotid %in% unique(GrW$plotid))
plot(surveyplot1_GrW)

# KK
KK=territoryid[territoryid$naam=="Kleine Karekiet",]
unique(KK$euring)

# Import relevant survey plots

surveyplot2 = readOGR(dsn=plot2file)

surveyplot2_KK=subset(surveyplot2, plotid %in% unique(KK$plotid))
plot(surveyplot2_KK)

# Sn
Sn=territoryid[territoryid$naam=="Snor",]
unique(Sn$euring)

# Import relevant survey plots

surveyplot3 = readOGR(dsn=plot1file)

surveyplot3_Sn=subset(surveyplot3, plotid %in% unique(Sn$plotid))
plot(surveyplot3_Sn)

#Union

survey_union <- bind(surveyplot1_GrW, surveyplot2_KK,surveyplot3_Sn)
