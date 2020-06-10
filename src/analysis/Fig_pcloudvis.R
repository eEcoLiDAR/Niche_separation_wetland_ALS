library(lidR)
library(plot3D)
library(rgdal)
library(sp)

workdirectory=setwd("C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/PointCloud_Vis/Kampen/")

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

clipped_7=lasclipCircle(catalog,188843,514837,100)
clipped_8=lasclipCircle(catalog,188658,514598,100)
clipped_9=lasclipCircle(catalog,188930,514575,100)

writeLAS(clipped_7,"grw_3.laz")
writeLAS(clipped_8,"sn_3.laz")
writeLAS(clipped_9,"kk_3.laz")

plot(clipped_7, color = "Classification")
plot(clipped_8, color = "Classification")
plot(clipped_9, color = "Classification")

clipped_10=lasclipCircle(catalog,188951,514459,100)
writeLAS(clipped_10,"kk_4.laz")
plot(clipped_10, color = "Classification")

clipped_7=lasclipCircle(catalog,188843,514837,200)
clipped_8=lasclipCircle(catalog,188658,514598,200)
clipped_9=lasclipCircle(catalog,188951,514459,200)

writeLAS(clipped_7,"grw_200m.laz")
writeLAS(clipped_8,"sn_200m.laz")
writeLAS(clipped_9,"kk_200m.laz")

# Visualization with Plot3D

clipped_1=readLAS("grw_1.laz")
clipped_2=readLAS("sn_1.laz")
clipped_3=readLAS("kk_1.laz")

clipped_4=readLAS("grw_2.laz")
clipped_5=readLAS("sn_2.laz")
clipped_6=readLAS("kk_2.laz")

clipped_7=readLAS("grw_200m.laz")
clipped_8=readLAS("sn_200m.laz")
clipped_9=readLAS("kk_200m.laz")

#rasterplot
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

defcolpal=palette(c("gray48","gold","lawngreen","chartreuse4"))
defcolpal=addalpha(defcolpal, 0.45)

rasterplot<-function(clipped_4,x=184877,y=511157,bird="Great reed warbler"){
  
  # remove water related plot and calc if do not have water
  
  clipped_4_nonveg = lasfilter(clipped_4, Classification != 1)
  
  dtm = grid_terrain(clipped_4_nonveg, res = 1, algorithm = knnidw(k = 25L))
  lasnormalize(clipped_4, dtm)
  
  hperc09all = grid_metrics(clipped_4, quantile(Z, 0.90), res=1)
  crs(hperc09all) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
  
  slope <- terrain(hperc09all, opt='slope')
  aspect <- terrain(hperc09all, opt='aspect')
  dsm_shd <- hillShade(slope, aspect, 40, 270)
  
  clipped_4_onlyveg=lasfilter(clipped_4, Classification == 1)
  
  hperc09 = grid_metrics(clipped_4_onlyveg, quantile(Z, 0.90), res=1)
  plot(hperc09)
  crs(hperc09) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
  height_class=reclassify(hperc09, c(-Inf,1,1,1,3,2,3,5,3,5,Inf,4))
  
  clipped_4_onlywater=lasfilter(clipped_4, Classification == 9)
  
  water = grid_metrics(clipped_4_onlywater, quantile(Z, 0.90), res=1)
  plot(hperc09)
  crs(water) <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
  water_class=reclassify(water, c(-Inf,Inf,0))
  
  coords = matrix(c(x, y), 
                  ncol = 2, byrow = TRUE)
  
  
  birdpoint = SpatialPoints(coords, proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))
  
  coords2 = matrix(c(x, y-10,
                    x, y+10), 
                  ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords2)
  line_cr = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))
  
  coords3 = matrix(c(x-10, y,
                     x+10, y), 
                   ncol = 2, byrow = TRUE)
  
  
  P12 = Polygon(coords3)
  line_cr2 = SpatialPolygons(list(Polygons(list(P12), ID = "a")), proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))
  
  defcolpal=palette(c("gray48","gold","lawngreen","green4"))
  defcolpal=addalpha(defcolpal, 0.45)
  
  par(mfrow=c(1,1)) 
  par(cex.lab=2, cex.axis=2.5, cex.main=2)
  plot(dsm_shd, col=grey(0:100/100), legend=FALSE, main=bird)
  legend("topright",legend=c("Ground","Water"),xpd=TRUE,pch=19,col = c("grey","blue"),cex=1.5)
  plot(birdpoint,pch=1,lwd = 3,col="black",add=TRUE)
  plot(line_cr, lwd=3,col="black",add=TRUE)
  plot(line_cr2, lwd=3,col="black",add=TRUE)
  plot(height_class, col=defcolpal,breaks=c(0,1,2,3,4), add=TRUE,
       lab.breaks = c("0","1","3","5","20"),
       legend.args=list(text='Height [m]', side=4, font=2, line=2.5,cex=2))
  plot(water_class, col="blue", legend=FALSE, add=TRUE)
  
}

rasterplot(clipped_7,x=188843,y=514837,bird="Great reed warbler")
rasterplot(clipped_8,x=188658,y=514598,bird="Savi's warbler")
rasterplot(clipped_9,x=188951,y=514459,bird="Reed warbler")

#crossplot

crossplot<-function(clipped_4,x=184877,y=511157){
  las_cross_ver=lasclipRectangle(clipped_4,x-80,y-1,x+80,y+1)
  las_cross_ver@data$cross=(las_cross_ver@data$X-x+100)-40
  
  par(cex.lab=2, cex.axis=2.5, cex.main=2)
  plot(x = las_cross_ver@data$cross, 
       y = las_cross_ver@data$Z, col = c("forestgreen", "grey", "blue","blue","blue","blue","blue","blue","blue","blue")[las_cross_ver@data$Classification],
       frame = FALSE, 
       xlab = "Distance[m]", ylab = "Height[m]",pch=19,ylim=c(0,20),
       main="Observation point",cex.lab=2)
  abline(v=60,lty=3)
  legend("topright",legend=c("Ground","Vegetation","Water"),xpd=TRUE,pch=19,col = c("grey", "forestgreen","blue"),cex=2.5)
  
}

crossplot_vert<-function(clipped_4,x=184877,y=511157){
  las_cross_ver=lasclipRectangle(clipped_4,x-1,y-80,x+1,y+80)
  las_cross_ver@data$cross=(las_cross_ver@data$Y-y+100)-40
  
  par(cex.lab=2, cex.axis=2.5, cex.main=2)
  plot(x = las_cross_ver@data$cross, 
       y = las_cross_ver@data$Z, col = c("forestgreen", "grey", "blue","blue","blue","blue","blue","blue","blue","blue")[las_cross_ver@data$Classification],
       frame = FALSE, 
       xlab = "Distance[m]", ylab = "Height[m]",pch=19,ylim=c(0,20),
       main="Observation point",cex.lab=2)
  abline(v=60,lty=3)
  legend("topright",legend=c("Ground","Vegetation","Water"),xpd=TRUE,pch=19,col = c("grey", "forestgreen","blue"),cex=2.5)
  
}

crossplot_vert(clipped_7,x=188843,y=514837)
crossplot_vert(clipped_8,x=188658,y=514598)
crossplot_vert(clipped_9,x=188951,y=514459)

# 3D pcloudpot

plot(clipped_4)
