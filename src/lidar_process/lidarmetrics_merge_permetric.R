library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

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

####

workingdir="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked2/"
setwd(workingdir)

filelist=list.files(pattern = "*.tif") # need to rename the files: same start all-> veg, ahn3cj intead of ud, two numer for tile identification
# in windows powerline: get-childitem *.mp3 | foreach { rename-item $_ $_.Name.Replace("all", "veg") }

# import landcover

landcoverfile="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/input_formask/LGN2018.tif"
landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# organize filenames

id=str_sub(filelist,1,31)
id=unique(id)
id=str_remove(id, "\\_$")

list_forfea=list.files(pattern=id[1])
feaname=str_remove(list_forfea, id[1])
feaname=str_remove(feaname, ".tif")

feaname=unique(feaname)

# Merge files per metric

for (i in feaname) {
  print(i)
  
  files_permetric=list.files(pattern = paste("*",i,".tif",sep=""))
  metric=mosaicList(files_permetric)
  
  writeRaster(metric,paste("lidar_metric",i,"_all.tif",sep=""),overwrite=TRUE)
  
}