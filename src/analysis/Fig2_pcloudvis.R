library(lidR)
library(plot3D)
library(rgdal)
library(sp)

workdirectory=setwd("C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/PointCloud_Vis/")

# Cut example files

catalog=catalog(workdirectory)

clipped_1=lasclipCircle(catalog,196431,517740,100)
clipped_2=lasclipCircle(catalog,197661,517499,100)
clipped_3=lasclipCircle(catalog,198263,516790,100)

writeLAS(clipped_1,"grw_1.laz")
writeLAS(clipped_2,"sn_1.laz")
writeLAS(clipped_3,"kk_1.laz")

clipped_4=lasclipCircle(catalog,184877,511157,100)
clipped_5=lasclipCircle(catalog,184606,510973,100)
clipped_6=lasclipCircle(catalog,182338,508226,100)

writeLAS(clipped_4,"grw_2.laz")
writeLAS(clipped_5,"sn_2.laz")
writeLAS(clipped_6,"kk_2.laz")

plot(clipped_1, color = "Classification")
plot(clipped_2, color = "Classification")
plot(clipped_3, color = "Classification")

plot(clipped_4, color = "Classification")
plot(clipped_5, color = "Classification")
plot(clipped_6, color = "Classification")

# Visualization with Plot3D

clipped_4=readLAS("grw_2.laz")
clipped_5=readLAS("sn_2.laz")
clipped_6=readLAS("kk_2.laz")

# normalize point cloud

clipped_4_nonveg = lasfilter(clipped_4, Classification != 1)

dtm = grid_terrain(clipped_4_nonveg, res = 1, algorithm = knnidw(k = 25L))
lasnormalize(clipped_4, dtm)

hperc09 = grid_metrics(clipped_4, quantile(Z, 0.90), res=1)
plot(hperc09)
crs(hperc09) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"

slope <- terrain(hperc09, opt='slope')
aspect <- terrain(hperc09, opt='aspect')
dsm_shd <- hillShade(slope, aspect, 40, 270)

coords = matrix(c(184877, 511157), 
                ncol = 2, byrow = TRUE)


birdpoint = SpatialPoints(coords, proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))

coords = matrix(c(184877, 511157-10,
                  184877, 511157+10), 
                ncol = 2, byrow = TRUE)


P1 = Polygon(coords)
line_cr = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))

coords2 = matrix(c(184877-10, 511157,
                   184877+10, 511157), 
                 ncol = 2, byrow = TRUE)


P12 = Polygon(coords2)
line_cr2 = SpatialPolygons(list(Polygons(list(P12), ID = "a")), proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))


par(mfrow=c(1,1)) 
plot(dsm_shd, col=grey(0:100/100), legend=FALSE, main="Great reed warbler")
plot(hperc09, col=rainbow(25, alpha=0.35), add=TRUE,legend.args=list(text='Height [m]', side=4, font=2, line=2.5, cex=1.5))
plot(birdpoint,pch=1,cex=3,lwd = 3,add=TRUE)
plot(line_cr, lwd=3,add=TRUE)
plot(line_cr2, lwd=3,add=TRUE)

# 2D raster
