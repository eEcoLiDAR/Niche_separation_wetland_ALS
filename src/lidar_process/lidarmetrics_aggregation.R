library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

workingdir="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/"
setwd(workingdir)

filelist=list.files(pattern = "*.tif") # need to rename the files: same start all-> veg, ahn3cj intead of ud, two numer for tile identification
# in windows powerline: get-childitem *.mp3 | foreach { rename-item $_ $_.Name.Replace("all", "veg") }

agr_fact=5
# import landcover

landcoverfile="D:/Koma/_PhD/Chapter3/Data_Preprocess/input_formask/LGN2018.tif"
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

# aggregate per tiles

for (i in id) {
  print(i)
  
  rastlist=list.files(pattern=paste(i,"_",sep=""))
  feaname=str_remove(rastlist, i)
  feaname=str_remove(feaname, ".tif")
  
  rasters=stack(rastlist)
  
  crs(rasters) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
  names(rasters) <- feaname
  
  rasters_50m=aggregate(rasters,fact=agr_fact,fun=median)
  writeRaster(rasters_50m,paste(i,"merged",agr_fact,"0m.grd",sep=""),overwrite=TRUE)
  
  # landcover filter indicate only wetlands
  
  landcover_sel=crop(landcover,extent(rasters_50m))
  
  lgn8_water_mask <- setValues(raster(landcover_sel), NA)
  lgn8_water_mask[landcover_sel==16 | landcover_sel==17 | landcover_sel==30 | landcover_sel==322 | landcover_sel==323 | landcover_sel==332 | landcover_sel==333 | landcover_sel==41 | landcover_sel==42 | landcover_sel==43 
                  | landcover_sel==45 | landcover_sel==46 | landcover_sel==47] <- 1
  
  lgn8_water_mask_resampled=resample(lgn8_water_mask,rasters_50m)
  
  # apply
  lidar_masked <- mask(rasters_50m, lgn8_water_mask_resampled)
  
  writeRaster(lidar_masked,paste(i,"merged_",agr_fact,"0m_onlywetland.grd",sep=""),overwrite=TRUE)
  
}

