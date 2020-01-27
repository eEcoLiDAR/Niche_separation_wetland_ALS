"
@author: Zsofia Koma, UvA
Aim: Apply cadastre and landcover based filters prior of data aggregation
"
library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)

# Initialize

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/"
setwd(workingdirectory)

filelist=list.files(pattern = "*95_normalized_height.tif")

landcoverfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"
humanobjectfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/human_objects/powerlines_buff20.shp"

#Import

lidar=stack(filelist)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

humanobject = readOGR(dsn=humanobjectfile)

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# Create water mask

landcover_sel=crop(landcover,extent(lidar))

lgn8_water_mask <- setValues(raster(landcover_sel), 1)
lgn8_water_mask[landcover_sel==16 | landcover_sel==17] <- NA

lgn8_water_mask_resampled=resample(lgn8_water_mask,lidar)

# apply
H90perc_masked <- mask(lidar, lgn8_water_mask_resampled)

# Create powerline mask
humanobj_sel=crop(humanobject,extent(lidar))
humanobj_rast <- rasterize(humanobj_sel, lidar,field="hoogtenive")
humanobj_rast_resampled=resample(humanobj_rast,lidar)

#apply
H90perc_masked_2 <- mask(H90perc_masked, humanobj_rast_resampled,maskvalue=0)

writeRaster(H90perc_masked_2,"H90perc_masked_2.tif",overwrite=TRUE)
