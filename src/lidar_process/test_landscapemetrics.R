library(raster)
library(rgdal)

library(landscapemetrics)

##

workingdir="D:/Sync/_Amsterdam/00_PhD/Teaching/2019/MSc/MiniProject/lidarmetrics/"
setwd(workingdir)

lidarfile="ahn3cj_feat_veg_10m_1m_18merged_50m.grd"
  
lidar=stack(lidarfile)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

dsm=lidar$X_perc_95_normalized_height
height_class=reclassify(dsm, c(-Inf,4,0, 4,Inf,1))

# landscape metrics

check_landscape(height_class)

lsm_p_perim=lsm_p_perim(height_class)

show_patches(height_class, class = "all", labels = FALSE)
