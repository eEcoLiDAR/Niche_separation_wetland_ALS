"
@author: Zsofia Koma, UvA
Aim: Create wetland mask
"
library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
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

# Initialize

workingdirectory="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/"
setwd(workingdirectory)

dir.create("onlywetland")

landcoverfile="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"

#Import

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# loop over all the tif files one by one

filelist=list.files(pattern = "*95_normalized_height_masked.tif")

for (i in filelist) {
  print(i)
  
  getfilename=str_sub(i,1,-5)
  
  lidar=stack(i)
  proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  landcover_sel=crop(landcover,extent(lidar))
  
  lgn8_water_mask <- setValues(raster(landcover_sel), NA)
  lgn8_water_mask[landcover_sel==30 | landcover_sel==322 | landcover_sel==332 | landcover_sel==41 | landcover_sel==42 | landcover_sel==43] <- 1
  
  lgn8_water_mask_resampled=resample(lgn8_water_mask,lidar)
  
  writeRaster(lgn8_water_mask_resampled,paste(workingdirectory,"/onlywetland/",getfilename,"_onlywetland_mask.tif",sep=""),overwrite=TRUE)
  
  # apply
  lidar_masked <- mask(lidar, lgn8_water_mask_resampled)
  
  writeRaster(lidar_masked,paste(workingdirectory,"/onlywetland/",getfilename,"_onlywetland.tif",sep=""),overwrite=TRUE)
 
}

setwd(paste(workingdirectory,"/onlywetland/",sep=""))
mask_tifs <- list.files(pattern = "*_onlywetland_mask.tif")

# Merge wetland mask

mask_merged=mosaicList(mask_tifs)
writeRaster(mask_merged,"merged_mask_onlywetland_genrand.tif",overwrite=TRUE)
  