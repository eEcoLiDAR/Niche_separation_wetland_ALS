"
@author: Zsofia Koma, UvA
Aim: G-space
"

library(ENMTools)

library(dismo)
library(rgeos)
library(rgdal)

library(ggplot2)

library(usdm)
library(sdm)

###  Set global variables
full_path="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/50m/"
setwd(full_path)

lidarfile="Merged_lidar_50m.tif"
grotekarakietfile="grotekarakiet_atlas.shp"
kleinekarakietfile="kleinekarakiet_atlas.shp"

### Import data

grotekarakiet <- readOGR(dsn=grotekarakietfile)

latlong_proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
grotekarakiet_wgs84 <- spTransform(grotekarakiet, latlong_proj)

grotekarakiet_wgs84.df <- as(grotekarakiet_wgs84, "data.frame")
names(grotekarakiet_wgs84.df ) <- c("species","occurrence","Longitude","Latitude")

kleinekarakiet <- readOGR(dsn=kleinekarakietfile)
kleinekarakiet_wgs84 <- spTransform(kleinekarakiet, latlong_proj)

kleinekarakiet_wgs84.df <- as(kleinekarakiet_wgs84, "data.frame")
names(kleinekarakiet_wgs84.df ) <- c("species","occurrence","Longitude","Latitude")

lidarmetrics=stack(lidarfile)

crs(lidarmetrics) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
lidarmetrics_wgs84 <-projectRaster(from = lidarmetrics, crs=crs("+proj=longlat +ellps=WGS84 +datum=WGS84"))
writeRaster(lidarmetrics_wgs84,"lidarmetrics_wgs84.tif",overwrite=TRUE)

###  Correlation analysis

# Prepare SDM with true pres-abs
grotekarakiet <- enmtools.species()

grotekarakiet$species.name <- "Acrocephalus arundinaceus"
grotekarakiet$presence.points <-grotekarakiet_wgs84.df[ which(grotekarakiet_wgs84.df$occurrence==1),3:4]
grotekarakiet$background.points <- grotekarakiet_wgs84.df[ which(grotekarakiet_wgs84.df$occurrence==0),3:4]

grotekarakiet.maxe <- enmtools.maxent(species = grotekarakiet, env = lidarmetrics_wgs84, test.prop = 0.3,rts.reps=5)


# Prepare an other SDM with true pres-abs
kleinekarakiet <- enmtools.species()

kleinekarakiet$species.name <- "Acrocephalus scirpaceus"
kleinekarakiet$presence.points <- kleinekarakiet_wgs84.df[ which(kleinekarakiet_wgs84.df$occurrence==1),3:4]
kleinekarakiet$background.points <- kleinekarakiet_wgs84.df[ which(kleinekarakiet_wgs84.df$occurrence==0),3:4]

kleinekarakiet.maxe <- enmtools.maxent(species = kleinekarakiet, env = lidarmetrics_wgs84, test.prop = 0.3,rts.reps=5)

### Hypho testing

#Niche overlap
over=raster.overlap(grotekarakiet.maxe,kleinekarakiet.maxe)

# niche identity testing
options(java.parameters = "-Xmx100g")

id.maxe <- identity.test(grotekarakiet,kleinekarakiet,env=lidarmetrics_wgs84,nreps = 1, type= "mx")
id.maxe

# background test
bg.gam <- background.test(baardman,snor,env=lidarmetrics_wgs84,nreps = 5, type= "glm",f=pres~kurto_z_all+pulse_pen_ratio_all+var_z_nonground)
bg.gam

