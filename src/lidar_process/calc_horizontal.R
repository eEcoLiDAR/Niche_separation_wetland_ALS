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
radii=41 #200m

for (i in filelist) {
  print(i)
  
  dsm=raster(i)
  proj4string(dsm) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  #variability per height classes metrics
  
  reedveg=dsm
  reedveg[(reedveg<1 | reedveg>3)] <- NA
  
  bushveg=dsm
  bushveg[(bushveg<3 | bushveg>5)] <- NA
  
  beginCluster(15)
  
  sd_dsm_21=clusterR(dsm, focal, args=list(w=matrix(1,radii,radii), fun=sd, pad=TRUE,na.rm = TRUE))
  sd_reedveg_21=clusterR(reedveg, focal, args=list(w=matrix(1,radii,radii), fun=sd, pad=TRUE,na.rm = TRUE))
  sd_bushveg_21=clusterR(bushveg, focal, args=list(w=matrix(1,radii,radii), fun=sd, pad=TRUE,na.rm = TRUE))

  endCluster()
  
  # export
  
  name=str_sub(i,1,-5)
  
  writeRaster(sd_dsm_21,paste(name,"_dsm_sd_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  writeRaster(sd_reedveg_21,paste(name,"_reedveg_sd_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  writeRaster(sd_bushveg_21,paste(name,"_bushveg_sd_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  
  #proportion per height classes metrics
  
  height_class_b1=reclassify(dsm, c(c(-Inf,1,1,1,3,0,3,5,0,5,Inf,0)))
  prop_helophyteveg=focal(height_class_b1,w=matrix(1,radii,radii), fun=sum, pad=TRUE,na.rm = TRUE)
  
  height_class_reed=reclassify(dsm, c(c(-Inf,1,0,1,3,1,3,5,0,5,Inf,0)))
  prop_reedveg=focal(height_class_reed,w=matrix(1,radii,radii), fun=sum, pad=TRUE,na.rm = TRUE)
  
  height_class_bush=reclassify(dsm, c(c(-Inf,1,0,1,3,0,3,5,1,5,Inf,0)))
  prop_bushveg=focal(height_class_bush,w=matrix(1,radii,radii), fun=sum, pad=TRUE,na.rm = TRUE)
  
  height_class_trees=reclassify(dsm, c(c(-Inf,1,0,1,3,0,3,5,0,5,Inf,1)))
  prop_treesveg=focal(height_class_trees,w=matrix(1,radii,radii), fun=sum, pad=TRUE,na.rm = TRUE)
  
  
  writeRaster(prop_helophyteveg,paste(name,"_prop_helophyteveg_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  writeRaster(prop_reedveg,paste(name,"_prop_reedveg_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  writeRaster(prop_bushveg,paste(name,"_prop_bushveg_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  writeRaster(prop_treesveg,paste(name,"_prop_treesveg_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  
}

feaname=c("_perc_95_normalized_height_masked_dsm_sd_41p_masked","_perc_95_normalized_height_masked_reedveg_sd_41p_masked",
          "_perc_95_normalized_height_masked_bushveg_sd_41p_masked","_perc_95_normalized_height_masked_prop_helophyteveg_41p_masked",
          "_perc_95_normalized_height_masked_prop_reedveg_41p_masked","_perc_95_normalized_height_masked_prop_bushveg_41p_masked",
          "_perc_95_normalized_height_masked_prop_treesveg_41p_masked")

for (i in feaname) {
  print(i)
  
  files_permetric=list.files(pattern = paste("*",i,".tif",sep=""))
  metric=mosaicList(files_permetric)
  
  writeRaster(metric,paste("lidar_metric",i,"_all.tif",sep=""),overwrite=TRUE)
  
}
