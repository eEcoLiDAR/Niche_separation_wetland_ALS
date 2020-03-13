"
@author: Zsofia Koma, UvA
Aim: Apply cadastre and landcover based filters prior of data aggregation
"
library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
library(stringr)

# Initialize

#workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/"
workingdirectory="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/"
setwd(workingdirectory)

dir.create("masked2")

landcoverfile="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/input_formask/LGN2018.tif"
humanobjectfile="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/input_formask/powerlines_buff20.shp"

#Import

humanobject = readOGR(dsn=humanobjectfile)

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# loop over all the tif files one by one

filelist=list.files(pattern = "*.tif")

for (i in filelist) {
  print(i)
  
  lidar=stack(i)
  proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  landcover_sel=crop(landcover,extent(lidar))
  
  lgn8_water_mask <- setValues(raster(landcover_sel), 1)
  lgn8_water_mask[landcover_sel==18 | landcover_sel==19 | landcover_sel==20 | landcover_sel==22 | landcover_sel==23 | landcover_sel==24 | landcover_sel==25 | landcover_sel==26 
                  | landcover_sel==27 | landcover_sel==28] <- NA
  
  lgn8_water_mask_resampled=resample(lgn8_water_mask,lidar)
  
  # apply
  lidar_masked <- mask(lidar, lgn8_water_mask_resampled)
  
  # Create powerline mask
  humanobj_sel=crop(humanobject,extent(lidar))
  
  if (is.null(humanobj_sel)) {
    getfilename=str_sub(i,1,-5)
    writeRaster(lidar_masked,paste(workingdirectory,"/masked2/",getfilename,"_masked.tif",sep=""),overwrite=TRUE)
  } else {
    humanobj_rast <- rasterize(humanobj_sel, lidar,field="hoogtenive")
    humanobj_rast_resampled=resample(humanobj_rast,lidar)
    
    #apply
    lidar_masked_2 <- mask(lidar_masked, humanobj_rast_resampled,maskvalue=0)
    
    getfilename=str_sub(i,1,-5)
    
    writeRaster(lidar_masked_2,paste(workingdirectory,"/masked2/",getfilename,"_masked.tif",sep=""),overwrite=TRUE)
  }
  
}


