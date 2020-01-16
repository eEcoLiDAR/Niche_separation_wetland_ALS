library(raster)
library(rgdal)

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

# landscape metrics

check_landscape(height_class_masked)

moving_window <- matrix(1, nrow = 3, ncol = 3)
result <- window_lsm(height_class_masked , window = moving_window, what = c("lsm_l_te"))
plot(result[[1]][["lsm_l_te"]])


show_patches(height_class_masked, class = "all", labels = TRUE)

# Patch metrics
patch_metrics <- dplyr::bind_rows(
  lsm_p_area(height_class_masked),
  lsm_p_enn(height_class_masked)
)

show_correlation(patch_metrics, method = "pearson")
show_lsm(height_class_masked, what = "lsm_p_area", class = "global", label_lsm = FALSE)

patches_r=get_patches(height_class_masked)
