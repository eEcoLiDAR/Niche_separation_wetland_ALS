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

grotekarakiet.rf <- enmtools.rf(species = grotekarakiet, env = lidarmetrics_wgs84, test.prop = 0.3,rts.reps=5)

visualize.enm(grotekarakiet.rf, lidarmetrics_wgs84, layers = c("Merged_lidar_50m.1", "Merged_lidar_50m.6"), plot.test.data = TRUE)

# Prepare an other SDM with true pres-abs
kleinekarakiet <- enmtools.species()

kleinekarakiet$species.name <- "Acrocephalus scirpaceus"
kleinekarakiet$presence.points <- kleinekarakiet_wgs84.df[ which(kleinekarakiet_wgs84.df$occurrence==1),3:4]
kleinekarakiet$background.points <- kleinekarakiet_wgs84.df[ which(kleinekarakiet_wgs84.df$occurrence==0),3:4]

kleinekarakiet.rf <- enmtools.rf(species = kleinekarakiet, env = lidarmetrics_wgs84, test.prop = 0.3,rts.reps=5)

### Hypho testing
#Breath
raster.breadth(grotekarakiet.rf)

#Niche overlap
over=raster.overlap(grotekarakiet.rf,kleinekarakiet.rf)

# niche identity testing
id.gam <- identity.test(baardman,snor,env=lidarmetrics_wgs84,nreps = 5, type= "glm",f=pres~kurto_z_all+pulse_pen_ratio_all+var_z_nonground)
id.gam

# background test
bg.gam <- background.test(baardman,snor,env=lidarmetrics_wgs84,nreps = 5, type= "glm",f=pres~kurto_z_all+pulse_pen_ratio_all+var_z_nonground)
bg.gam

### Ensemble with sdm
data_forsdm <- sdmData(formula=occ~., train=grotekarakiet, predictors=lidarmetrics)
data_forsdm

model1 <- sdm(occ~.,data=data_forsdm,methods=c('glm','gam','brt','rf','svm','mars'),replication=c('boot'),n=2)
model1

rcurve(model1,id = 5,mean=F,confidence = T)
a <- getResponseCurve(model1,id=5)

feaimp_1=getVarImp(model1,id = 5)
plot(feaimp_1,'auc')
plot(feaimp_1,'cor')

ens1 <- ensemble(model1, newdata=lidarmetrics, filename="",setting=list(method='weighted',stat='AUC',opt=2))

niche(x=lidarmetrics,h=ens1,n=c("pulse_pen_ratio_all","max_z__nonground"))