library(raster)
library(rgdal)
library(sf)
library(dplyr)

library(fasterize)

library(spatialEco)
library(landscapemetrics)

##

workingdir="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/masked/"
#workingdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/masked/"
setwd(workingdir)

lidarfile="ahn3cj_feat_veg_10m_1m_0_perc_95_normalized_height_masked.tif"
landcoverfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"
#landcoverfile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"
  
lidar=stack(lidarfile)
proj4string(lidar) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

landcover=stack(landcoverfile)
proj4string(landcover) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")

# set up input files
height_class=reclassify(lidar, c(-Inf,5,0, 5,Inf,1))

ext=extent(56000,58000,360000,362000)
dsm_sel=crop(lidar,ext)
height_class_sel=crop(height_class,ext)
landcover_sel=crop(landcover,ext)

# landscape in moving windows - landscape level between height classes
landsc_m_mv_np <- focal.lmetrics(height_class_sel, w=11, land.value = 0, metric = "n.patches")
landsc_m_mv_ed <- focal.lmetrics(height_class_sel, w=11, land.value = 0, metric = "edge.density")
landsc_m_mv_propl <- focal.lmetrics(height_class_sel, w=11, land.value = 0, metric = "prop.landscape")

landsc_m_mv_np_agr <- aggregate(landsc_m_mv_np, fact=11, fun=mean)
landsc_m_mv_ed_agr <- aggregate(landsc_m_mv_ed, fact=11, fun=mean)
landsc_m_mv_propl_agr <- aggregate(landsc_m_mv_propl, fact=11, fun=mean)

# landcscape metrics without moving window
my_grid_geom = st_make_grid(height_class_sel, cellsize = 50)
my_grid = st_sf(geom = my_grid_geom)

plot(height_class_sel)
plot(my_grid, add = TRUE)

my_metric_ed = sample_lsm(height_class_sel, my_grid,level = "landscape", metric = "ed")
my_metric_np = sample_lsm(height_class_sel, my_grid,level = "landscape", metric = "np")
my_metric_ta = sample_lsm(height_class_sel, my_grid,level = "landscape", metric = "ta")

my_grid_ed = bind_cols(my_grid, my_metric_ed)
my_grid_np = bind_cols(my_grid, my_metric_np)
my_grid_ta = bind_cols(my_grid, my_metric_ta)

plot(height_class_sel, main="Classified")
plot(my_grid_ed["value"],main="Edge density")
plot(my_grid_np["value"],main="Number of patches")
plot(my_grid_ta["value"],main="area density")

r_ed <- raster(my_grid_ed, res = 50)
my_metric_ed_r <- fasterize(my_grid_ed, r_ed, field = "value", fun="max")

r_np <- raster(my_grid_np, res = 50)
my_metric_np_r <- fasterize(my_grid_np, r_np, field = "value", fun="max")

r_ta <- raster(my_grid_ta, res = 50)
my_metric_ta_r <- fasterize(my_grid_ta, r_ta, field = "value", fun="max")

# for the article
height_class2=reclassify(lidar, c(-Inf,5,1, 5,Inf,NA))
height_class_sel2=crop(height_class2,ext)

my_metric_low_ed = sample_lsm(height_class_sel2, my_grid,level = "class", metric = "ed",count_boundary = TRUE,directions = 8)
my_metric_low_np = sample_lsm(height_class_sel2, my_grid,level = "class", metric = "np",count_boundary = TRUE,directions = 8)

my_grid_low_ed= bind_cols(my_grid, my_metric_low_ed)
my_grid_low_np= bind_cols(my_grid, my_metric_low_np)

plot(my_grid_low_ed["value"],main="Edge density")
plot(my_grid_low_np["value"],main="Number of patches")
