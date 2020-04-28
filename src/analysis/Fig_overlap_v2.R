library(dplyr)
library(stringr)
library(tidyr)

library(ecospat)

library(raster)
library(scales)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v11/"
setwd(workingdirectory)

# Import data

grw_pca12 <- readRDS("grw_kdens_r.rds")
kk_pca12 <- readRDS("kk_kdens_r.rds")
sn_pca12 <- readRDS("sn_kdens_r.rds")

# plot
pal <- colorRampPalette(c("grey95", "goldenrod4"))
pal50grw<- pal(50)

pal2 <- colorRampPalette(c("grey95", "green3"))
pal50kk<- pal2(50)

pal3 <- colorRampPalette(c("grey95", "deeppink"))
pal50sn<- pal3(50)

plot.2niche <- function(kk_pca12,grw_pca12,pal50kk,pal50grw,col1,col2) {
  image(x=kk_pca12$x,y=kk_pca12$y,z=t(as.matrix(kk_pca12$z.uncor))[,nrow(as.matrix(kk_pca12$z.uncor)):1], col = alpha(pal50kk,1), zlim = c(1e-06, cellStats(kk_pca12$z.cor,"max")), 
        xlab = "PCA 1", ylab = "PCA 1")
  image(x=grw_pca12$x,y=grw_pca12$y,z=t(as.matrix(grw_pca12$z.uncor))[,nrow(as.matrix(grw_pca12$z.uncor)):1], col = alpha(pal50grw,0.6), zlim = c(1e-06, cellStats(grw_pca12$z.cor,"max")), 
        xlab = "PCA 1", ylab = "PCA 1", add=TRUE)
  
  grw_pca12$uncor.norm<-t(as.matrix(grw_pca12$z.uncor))[,nrow(as.matrix(grw_pca12$z.uncor)):1]
  contour(x=grw_pca12$x,y=grw_pca12$y,grw_pca12$uncor.norm, levels = quantile(grw_pca12$uncor.norm[grw_pca12$uncor.norm > 0], c(0, 0.5)), drawlabels = FALSE,
          lty = c(1, 2),col=col1,lwd=2,add=TRUE)
  kk_pca12$uncor.norm<-t(as.matrix(kk_pca12$z.uncor))[,nrow(as.matrix(kk_pca12$z.uncor)):1]
  contour(x=kk_pca12$x,y=kk_pca12$y,kk_pca12$uncor.norm, add = TRUE, levels = quantile(kk_pca12$uncor.norm[kk_pca12$uncor.norm > 0], c(0, 0.5)), drawlabels = FALSE,
          lty = c(1, 2),col=col2,lwd=2)
  
}

plot.2niche(kk_pca12,grw_pca12,pal50kk,pal50grw,"goldenrod4","green3")
plot.2niche(kk_pca12,sn_pca12,pal50kk,pal50sn,"deeppink","green3")
plot.2niche(sn_pca12,grw_pca12,pal50sn,pal50grw,"goldenrod4","deeppink")