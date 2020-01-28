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

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/"
setwd(workingdirectory)

landcoverfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"
humanobjectfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/human_objects/powerlines_buff20.shp"

#Import

humanobject = readOGR(dsn=humanobjectfile)

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# loop over all the tif files one by one

filelist=list.files(pattern = "*.tif")

for (i in filelist[1:3]) {
  print(i)
  
  lidar=stack(i)
  proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  landcover_sel=crop(landcover,extent(lidar))
  
  lgn8_water_mask <- setValues(raster(landcover_sel), 1)
  lgn8_water_mask[landcover_sel==16 | landcover_sel==17 | landcover_sel==18 | landcover_sel==19 | landcover_sel==20 | landcover_sel==22 | landcover_sel==23 | landcover_sel==24 | landcover_sel==25 | landcover_sel==26 
                  | landcover_sel==27 | landcover_sel==28] <- NA
  
  lgn8_water_mask_resampled=resample(lgn8_water_mask,lidar)
  
  # apply
  H90perc_masked <- mask(lidar, lgn8_water_mask_resampled)
  
  # Create powerline mask
  humanobj_sel=crop(humanobject,extent(lidar))
  humanobj_rast <- rasterize(humanobj_sel, lidar,field="hoogtenive")
  humanobj_rast_resampled=resample(humanobj_rast,lidar)
  
  #apply
  H90perc_masked_2 <- mask(H90perc_masked, humanobj_rast_resampled,maskvalue=0)
  
  getfilename=str_sub(i,1,-5)
  
  writeRaster(H90perc_masked_2,paste(getfilename,"_masked.tif"),overwrite=TRUE)
  
}


