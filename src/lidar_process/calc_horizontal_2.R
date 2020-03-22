library(raster)
library(snow)
library(rgdal)
library(stringr)

library(sf)
library(dplyr)

library(spatialEco)

#### function

mosaicList <- function(rasList){
  
  #Internal function to make a list of raster objects from list of files.
  ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster::raster(grd_name)
    }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
  
  #convert every raster path to a raster object and create list of the results
  raster.list <-sapply(rasList, FUN = ListRasters)
  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  #####This function deals with overlapping areas
  raster.list$fun <- mean
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(raster::mosaic, raster.list)
  
  #set crs of output
  crs(mos) <- crs(x = raster(rasList[1]))
  return(mos)
}


##

#workingdir="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/masked/"
workingdir="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked2/"
setwd(workingdir)

filelist=list.files(pattern = "*95_normalized_height_masked.tif")
radii=21

for (i in filelist) {
  print(i)
  name=str_sub(i,1,-5)
  
  dsm=raster(i)
  proj4string(dsm) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  #create height classes
  
  height_class_b1=reclassify(dsm, c(c(-Inf,1,1,1,5,0,5,Inf,0)))
  prop_lowveg=focal(height_class_b1,w=matrix(1,radii,radii), fun=sum, pad=TRUE,na.rm = TRUE)
  
  height_class_med=reclassify(dsm, c(c(-Inf,1,0,1,5,1,5,Inf,0)))
  prop_medveg=focal(height_class_med,w=matrix(1,radii,radii), fun=sum, pad=TRUE,na.rm = TRUE)

  
  writeRaster(prop_lowveg,paste(name,"_prop_lowvegb1_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  writeRaster(prop_medveg,paste(name,"_prop_medveg_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  
}

feaname=c("_perc_95_normalized_height_masked_prop_lowvegb1_21p_masked","_perc_95_normalized_height_masked_prop_medveg_21p_masked")

for (i in feaname) {
  print(i)
  
  files_permetric=list.files(pattern = paste("*",i,".tif",sep=""))
  metric=mosaicList(files_permetric)
  
  writeRaster(metric,paste("lidar_metric",i,"_all.tif",sep=""),overwrite=TRUE)
  
}