library(raster)
library(snow)
library(rgdal)
library(stringr)

library(sf)
library(dplyr)

library(spatialEco)

library(fasterize)
library(landscapemetrics)

##

workingdir="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/masked/"
setwd(workingdir)

filelist=list.files(pattern = "*95_normalized_height_masked.tif")

for (i in filelist) {
  print(i)
  
  dsm=raster(i)
  proj4string(dsm) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  #create height classes
  
  height_class=reclassify(dsm, c(-Inf,5,1, 5,Inf,0))
  prop_lowveg=focal(height_class,w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE)
  
  #calc. hor variability
  
  lowveg=dsm
  lowveg[lowveg>5] <- NA
  
  beginCluster(2)
  
  sd_dsm_11=clusterR(dsm, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
  sd_lowveg_11=clusterR(lowveg, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
  
  endCluster()
  
  # landscape str
  
  height_class2=reclassify(dsm, c(-Inf,5,1, 5,Inf,NA))
  
  my_grid_geom = st_make_grid(height_class2, cellsize = 50)
  my_grid = st_sf(geom = my_grid_geom)
  
  low_ed = sample_lsm(height_class2, my_grid,level = "class", metric = "ed",count_boundary = TRUE,directions = 8)
  low_np = sample_lsm(height_class2, my_grid,level = "class", metric = "np",count_boundary = TRUE,directions = 8)
  
  low_ed_p= bind_cols(my_grid, my_metric_low_ed)
  low_np_p= bind_cols(my_grid, my_metric_low_np)
  
  r_ed <- raster(low_ed_p, res = 50)
  my_metric_ed_r <- fasterize(low_ed_p, r_ed, field = "value", fun="max")
  
  r_np <- raster(low_np_p, res = 50)
  my_metric_np_r <- fasterize(low_np_p, r_np, field = "value", fun="max")
  
  # export
  
  #name=str_sub(i,1,-5)
  
  #writeRaster(sd_dsm_11,paste(name,"_dsm_sd_50m.tif",sep=""),overwrite=TRUE)
  #writeRaster(sd_lowveg_11,paste(name,"_lowveg_sd_50m.tif",sep=""),overwrite=TRUE)
  
}
