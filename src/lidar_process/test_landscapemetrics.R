library(raster)
library(rgdal)
library(sf)
library(dplyr)

library(spatialEco)
library(landscapemetrics)

##

workingdir="D:/Sync/_Amsterdam/00_PhD/Teaching/2019/MSc/MiniProject/lidarmetrics/"
setwd(workingdir)

lidarfile="ahn3cj_feat_veg_10m_1m_18merged_50m.grd"
landcoverfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"
  
lidar=stack(lidarfile)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# set up input files
dsm=lidar$X_perc_95_normalized_height
height_class=reclassify(dsm, c(-Inf,4,0, 4,Inf,1))

ext=extent(160000,170000,500000,510000)
dsm_sel=crop(dsm,ext)
height_class_sel=crop(height_class,ext)
landcover_sel=crop(landcover,ext)

# landcover filter
wetland_mask <- setValues(raster(landcover_sel), NA)
wetland_mask[landcover_sel==30 | landcover_sel==41 | landcover_sel==42 | landcover_sel==43 | landcover_sel==45] <- 1

plot(wetland_mask, col="dark green", legend = FALSE)
wetland_mask_resampled=resample(wetland_mask,height_class_sel)

height_class_masked <- mask(height_class_sel, wetland_mask_resampled)

# landscape in moving windows - landscape level between height classes
landsc_m_mv_np <- focal.lmetrics(height_class_masked, w=3, land.value = 1, metric = "n.patches")
landsc_m_mv_ed <- focal.lmetrics(height_class_masked, w=3, land.value = 1, metric = "edge.density")
landsc_m_mv_propl <- focal.lmetrics(height_class_masked, w=3, land.value = 1, metric = "prop.landscape")
landsc_m_mv_tarea <- focal.lmetrics(height_class_masked, w=3, land.value = 1,metric = "total.area")

landsc_m_mv_np_agr <- aggregate(landsc_m_mv_np, fact=3, fun=mean)
landsc_m_mv_ed_agr <- aggregate(landsc_m_mv_ed, fact=3, fun=mean)
landsc_m_mv_propl_agr <- aggregate(landsc_m_mv_propl, fact=3, fun=mean)

