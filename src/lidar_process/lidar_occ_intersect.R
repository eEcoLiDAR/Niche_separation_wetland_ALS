library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

workingdir="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/masked/"
setwd(workingdir)

# import bird observation data - need to be set all trough the script the right input shp and the shp should be located in the working directory together with the grd file

shp=readOGR(".","birds_swet_presatl")
#shp=readOGR(".","birds_swet_absatl")
#shp=readOGR(".","GreedW_avi_wacq")
#shp=readOGR(".","ReedW_avi_wacq")
#shp=readOGR(".","SaviW_avi_wacq")
#shp=readOGR(".","BReed_avi_wacq")

shp.df <- as(shp, "data.frame")
shp.df$id<- seq(1,length(shp.df$occrrn))

shp_sel=subset(shp.df, select=c("coords.x1","coords.x2","species","occrrnc","id"))

coordinates(shp_sel)=~coords.x1+coords.x2
proj4string(shp_sel)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

grdlist=list.files(pattern = "*_onlywetland.grd")

for (j in grdlist) {
  print(j)
  
  raster=stack(j)
  
  feaname_exp=str_remove(names(raster), "^X_")
  names(raster)<-feaname_exp
  
  possibleError=tryCatch(sdmData(occrrnc~.,train=shp_sel,predictors = raster), error = function(e) e)
  
  if(inherits(possibleError, "error")) next
  
  d <- sdmData(occrrnc~.,train=shp_sel,predictors = raster)
  data=d@features
  
  intersect_data=merge(x = data, y = shp.df, by = c("id"), all.x = TRUE)
    
  name=sub('\\..*', "", j)
  
  write.csv(intersect_data,paste(name,"_intersected.csv",sep=""))
  #write.csv(intersect_data,paste(name,"_intersected_abs.csv",sep=""))
  #write.csv(intersect_data,paste(name,"_intersected_GreedW.csv",sep=""))
  #write.csv(intersect_data,paste(name,"_intersected_ReedW.csv",sep=""))
  #write.csv(intersect_data,paste(name,"_intersected_SaviW.csv",sep=""))
  #write.csv(intersect_data,paste(name,"_intersected_BReed.csv",sep=""))

}

files <- list.files(pattern = "*_intersected.csv")
#files <- list.files(pattern = "*_intersected_abs.csv")
#files <- list.files(pattern = "*_intersected_GreedW.csv")
#files <- list.files(pattern = "*_intersected_ReedW.csv")
#files <- list.files(pattern = "*_intersected_SaviW.csv")
#files <- list.files(pattern = "*_intersected_BReed.csv")

allcsv <- lapply(files,function(g){
  read.csv(g, header=TRUE)
})

allcsv_df <- do.call(rbind.data.frame, allcsv)

write.csv(allcsv_df,"veg_metrics_50m_pres.csv")
#write.csv(allcsv_df,"veg_metrics_10m_abs.csv")
#write.csv(allcsv_df,"veg_metrics_10m_GreedW.csv")
#write.csv(allcsv_df,"veg_metrics_10m_ReedW.csv")
#write.csv(allcsv_df,"veg_metrics_10m_SaviW.csv")
#write.csv(allcsv_df,"veg_metrics_10m_BReed.csv")
