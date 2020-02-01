library(raster)
library(rgdal)
library(sf)
library(dplyr)

library(fasterize)

library(spatialEco)
library(landscapemetrics)

##

#workingdir="D:/Sync/_Amsterdam/00_PhD/Teaching/2019/MSc/MiniProject/lidarmetrics/"
workingdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Trial/masked/"
setwd(workingdir)

lidarfile="ahn3cj_feat_veg_10m_1m_0_perc_95_normalized_height_masked.tif"
#landcoverfile="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"
landcoverfile="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/filters/landcover/UvA_LGN2018/LGN2018.tif"
  
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
landsc_m_mv_np <- focal.lmetrics(height_class, w=11, land.value = 0, metric = "n.patches")
landsc_m_mv_ed <- focal.lmetrics(height_class, w=11, land.value = 0, metric = "edge.density")
landsc_m_mv_propl <- focal.lmetrics(height_class, w=11, land.value = 0, metric = "prop.landscape")

landsc_m_mv_np_agr <- aggregate(landsc_m_mv_np, fact=3, fun=mean)
landsc_m_mv_ed_agr <- aggregate(landsc_m_mv_ed, fact=3, fun=mean)
landsc_m_mv_propl_agr <- aggregate(landsc_m_mv_propl, fact=3, fun=mean)

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

plot(my_grid_ed["value"])
plot(my_grid_np["value"])
plot(my_grid_ta["value"])

r <- raster(my_grid, res = 50)
my_metric_ed_r <- fasterize(my_grid, r, field = "value2", fun="max")
