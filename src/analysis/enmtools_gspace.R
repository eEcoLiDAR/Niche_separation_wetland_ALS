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
full_path="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v1/"
#full_path="C:/Koma/Sync/_Amsterdam/00_PhD/Teaching/SDM/"

birdfile_baardman="Baardman_bird_data_forSDM.shp"
birdfile_snor="Snor_bird_data_forSDM.shp"
lidarfile="lidarmetrics_forSDM.grd"

setwd(full_path)

### Import data

bird_baardman <- readOGR(dsn=birdfile_baardman)

latlong_proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
bird_baardman_wgs84 <- spTransform(bird_baardman, latlong_proj)

bird_baardman_wgs84.df <- as(bird_baardman_wgs84, "data.frame")
names(bird_baardman_wgs84.df ) <- c("occurrence","Longitude","Latitude")

bird_snor <- readOGR(dsn=birdfile_snor)
bird_snor_wgs84 <- spTransform(bird_snor, latlong_proj)

bird_snor_wgs84.df <- as(bird_snor_wgs84, "data.frame")
names(bird_snor_wgs84.df ) <- c("occurrence","Longitude","Latitude")

lidarmetrics=stack(lidarfile)

crs(lidarmetrics) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
lidarmetrics_wgs84 <-projectRaster(from = lidarmetrics, crs=crs("+proj=longlat +ellps=WGS84 +datum=WGS84"))

###  Correlation analysis
#raster.cor.matrix(lidarmetrics)
raster.cor.plot(lidarmetrics)

v <- vifstep(lidarmetrics,th=10)
v
lidarmetrics2 <- exclude(lidarmetrics,v)

###  SDM with ENMtools

baardman_onlypres <- enmtools.species()
baardman_onlypres

baardman_onlypres$species.name <- "Panurus biarmicus"
baardman_onlypres$presence.points <- bird_baardman_wgs84.df[ which(bird_baardman_wgs84.df$occurrence==1),2:3]

interactive.plot.enmtools.species(baardman_onlypres)

baardman_onlypres$range <- background.raster.buffer(baardman_onlypres$presence.points,5000,mask=lidarmetrics_wgs84)
baardman.glm <- enmtools.glm(species = baardman_onlypres, env = lidarmetrics_wgs84, test.prop = 0.3)

baardman.glm$test.evaluation

# Prepare SDM with true pres-abs
baardman <- enmtools.species()

baardman$species.name <- "Panurus biarmicus"
baardman$presence.points <- bird_baardman_wgs84.df[ which(bird_baardman_wgs84.df$occurrence==1),2:3]
baardman$background.points <- bird_baardman_wgs84.df[ which(bird_baardman_wgs84.df$occurrence==0),2:3]

baardman.glm <- enmtools.glm(species = baardman, env = lidarmetrics_wgs84, test.prop = 0.3,rts.reps=5)

baardman.glm$response.plots$pulse_pen_ratio_all
baardman.glm$response.plots$roughness.1
baardman.glm$response.plots$max_z__nonground

visualize.enm(baardman.glm, lidarmetrics_wgs84, layers = c("roughness.1", "pulse_pen_ratio_all"), plot.test.data = TRUE)

# Prepare an other SDM with true pres-abs
snor <- enmtools.species()

snor$species.name <- "Locustella luscinioides"
snor$presence.points <- bird_snor_wgs84.df[ which(bird_snor_wgs84.df$occurrence==1),2:3]
snor$background.points <- bird_snor_wgs84.df[ which(bird_snor_wgs84.df$occurrence==0),2:3]

snor.glm <- enmtools.glm(species = snor, env = lidarmetrics_wgs84, test.prop = 0.3,rts.reps=5)

### Hypho testing
#Breath
raster.breadth(baardman.glm)

#Niche overlap
over=raster.overlap(baardman.glm,snor.glm)

# niche identity testing
id.gam <- identity.test(baardman,snor,env=lidarmetrics_wgs84,nreps = 5, type= "glm",f=pres~kurto_z_all+pulse_pen_ratio_all+var_z_nonground)
id.gam

# background test
bg.gam <- background.test(baardman,snor,env=lidarmetrics_wgs84,nreps = 5, type= "glm",f=pres~kurto_z_all+pulse_pen_ratio_all+var_z_nonground)
bg.gam

### Ensemble with sdm
data_forsdm <- sdmData(formula=occurrence~., train=bird_baardman, predictors=lidarmetrics)
data_forsdm

model1 <- sdm(occurrence~.,data=data_forsdm,methods=c('glm','gam','brt','rf','svm','mars'),replication=c('boot'),n=2)
model1

rcurve(model1,id = 1,mean=F,confidence = T)
a <- getResponseCurve(model1,id=5)

feaimp_1=getVarImp(model1,id = 1)
plot(feaimp_1,'auc')
plot(feaimp_1,'cor')

ens1 <- ensemble(model1, newdata=lidarmetrics, filename="",setting=list(method='weighted',stat='AUC',opt=2))

niche(x=lidarmetrics,h=ens1,n=c("pulse_pen_ratio_all","max_z__nonground"))