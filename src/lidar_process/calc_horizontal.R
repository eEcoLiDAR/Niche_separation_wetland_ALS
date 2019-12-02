library(raster)
library(snow)
library(rgdal)
library(stringr)

##

workingdir="D:/Koma/Paper2/Paper2_2019Nov/ahn3_2019_10_15_geotiffs/features_veg_10m_1m/"
setwd(workingdir)

filelist=list.files(pattern = "*95_normalized_height.tif")

for (i in filelist) {
  print(i)
  
  dsm=raster(i)
  proj4string(dsm) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  rough_dsm=terrain(dsm,opt="roughness",neighbors=4)
  
  height_class=reclassify(dsm, c(-Inf,4,0, 4,Inf,1))
  highveg=focal(height_class,w=matrix(1,11,11), fun=sum, pad=TRUE,na.rm = TRUE)
  
  lowveg=dsm
  lowveg[lowveg>4] <- NA
  
  beginCluster(18)
  
  sd_dsm_11=clusterR(dsm, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
  med_dsm_11=clusterR(dsm, focal, args=list(w=matrix(1,11,11), fun=median, pad=TRUE,na.rm = TRUE))
  
  sd_dsm_21=clusterR(dsm, focal, args=list(w=matrix(1,21,21), fun=sd, pad=TRUE,na.rm = TRUE))
  med_dsm_21=clusterR(dsm, focal, args=list(w=matrix(1,21,21), fun=median, pad=TRUE,na.rm = TRUE))
  
  sd_lowveg_5=clusterR(lowveg, focal, args=list(w=matrix(1,5,5), fun=sd, pad=TRUE,na.rm = TRUE))
  med_lowveg_5=clusterR(lowveg, focal, args=list(w=matrix(1,5,5), fun=median, pad=TRUE,na.rm = TRUE))
  range_lowveg_5=clusterR(lowveg, focal, args=list(w=matrix(1,5,5), fun=range, pad=TRUE,na.rm = TRUE))
  
  sd_lowveg_11=clusterR(lowveg, focal, args=list(w=matrix(1,11,11), fun=sd, pad=TRUE,na.rm = TRUE))
  med_lowveg_11=clusterR(lowveg, focal, args=list(w=matrix(1,11,11), fun=median, pad=TRUE,na.rm = TRUE))
  rangemed_lowveg_11=clusterR(range_lowveg_5, focal, args=list(w=matrix(1,11,11), fun=median, pad=TRUE,na.rm = TRUE))
  
  endCluster()
  
  name=str_sub(i,1,25)
  
  writeRaster(rough_dsm,paste(name,"_dsm_roughness_10m.tif",sep=""),overwrite=TRUE)
  writeRaster(sd_dsm_11,paste(name,"_dsm_sd_50m.tif",sep=""),overwrite=TRUE)
  writeRaster(med_dsm_11,paste(name,"_dsm_med_50m.tif",sep=""),overwrite=TRUE)
  writeRaster(sd_dsm_21,paste(name,"_dsm_sd_100m.tif",sep=""),overwrite=TRUE)
  writeRaster(med_dsm_21,paste(name,"_dsm_med_100m.tif",sep=""),overwrite=TRUE)
  writeRaster(highveg,paste(name,"_highveg_count_50m.tif",sep=""),overwrite=TRUE)
  
  writeRaster(sd_lowveg_5,paste(name,"_lowveg_med_20m.tif",sep=""),overwrite=TRUE)
  writeRaster(med_lowveg_5,paste(name,"_lowveg_sd_20m.tif",sep=""),overwrite=TRUE)
  writeRaster(range_lowveg_5,paste(name,"_lowveg_range_20m.tif",sep=""),overwrite=TRUE)
  writeRaster(sd_lowveg_11,paste(name,"_lowveg_med_50m.tif",sep=""),overwrite=TRUE)
  writeRaster(med_lowveg_11,paste(name,"_lowveg_sd_50m.tif",sep=""),overwrite=TRUE)
  writeRaster(rangemed_lowveg_11,paste(name,"_lowveg_medrange_50m.tif",sep=""),overwrite=TRUE)
  
}
