library(raster)
library(snow)
library(rgdal)
library(stringr)

library(sf)
library(dplyr)

library(spatialEco)


##

#workingdir="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/masked/"
workingdir="D:/Koma/_PhD/Offline/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked2/"
setwd(workingdir)

filelist=list.files(pattern = "*95_normalized_height_masked.tif")
radii=21

for (i in filelist) {
  print(i)
  
  dsm=raster(i)
  proj4string(dsm) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  #create height classes
  
  height_class=reclassify(dsm, c(-Inf,5,1, 5,Inf,0))
  prop_lowveg=focal(height_class,w=matrix(1,radii,radii), fun=sum, pad=TRUE,na.rm = TRUE)
  
  #dsm_allin=dsm
  #dsm_allin[is.na(dsm_allin)] <- 0
  
  #height_class2=reclassify(dsm_allin, c(-Inf,0,1, 0,5,2, 5,Inf,3))
  #proj4string(height_class2) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  #calc. hor variability
  
  lowveg=dsm
  lowveg[lowveg>5] <- NA
  
  beginCluster(15)
  
  sd_dsm_11=clusterR(dsm, focal, args=list(w=matrix(1,radii,radii), fun=sd, pad=TRUE,na.rm = TRUE))
  sd_lowveg_11=clusterR(lowveg, focal, args=list(w=matrix(1,radii,radii), fun=sd, pad=TRUE,na.rm = TRUE))
  #landsc_m_mv_np=clusterR(height_class2,focal.lmetrics,args=list(w=radii, metric = "n.patches"))
  #landsc_m_mv_ed=clusterR(height_class2,focal.lmetrics,args=list(w=radii, land.value = 2, metric = "edge.density"))

  endCluster()
  
  # export
  
  name=str_sub(i,1,-5)
  
  writeRaster(sd_dsm_11,paste(name,"_dsm_sd_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  writeRaster(sd_lowveg_11,paste(name,"_lowveg_sd_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  
  writeRaster(prop_lowveg,paste(name,"_prop_lowveg_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  
  #writeRaster(landsc_m_mv_np,paste(name,"_landscape_npatch_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  #writeRaster(landsc_m_mv_ed,paste(name,"_landscape_edgeden_",radii,"p_masked.tif",sep=""),overwrite=TRUE)
  
}
