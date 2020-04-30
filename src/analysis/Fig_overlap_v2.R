library(dplyr)
library(stringr)
library(tidyr)

library(ecospat)

library(raster)
library(scales)


# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v12/"
setwd(workingdirectory)

# Import data

grw_pca12 <- readRDS("grw_kdens.rds")
kk_pca12 <- readRDS("kk_kdens.rds")
sn_pca12 <- readRDS("sn_kdens.rds")

eq_grw_kk <- readRDS("eq.test_gr_k.rds")
sim.test_gr_k<- readRDS("sim.test_gr_k.rds")
sim.test_k_gr<- readRDS("sim.test_k_gr.rds")

eq_grw_s <- readRDS("eq.test_gr_s.rds")
eq_k_s <- readRDS("eq.test_k_s.rds")


# create colorbar
pal <- colorRampPalette(c("grey95", "goldenrod4"))
pal50grw<- pal(50)

pal2 <- colorRampPalette(c("grey95", "green3"))
pal50kk<- pal2(50)

pal3 <- colorRampPalette(c("grey95", "deeppink"))
pal50sn<- pal3(50)

paldiff <- colorRampPalette(c("blue","white","red"))
pal50diff<- paldiff(50)

# two niche overlap
plot.2niche <- function(kk_pca12,grw_pca12,pal50kk,pal50grw,col1,col2) {
  image(x=kk_pca12$x,y=kk_pca12$y,z=t(as.matrix(kk_pca12$z.uncor))[,nrow(as.matrix(kk_pca12$z.uncor)):1], col = alpha(pal50kk,1), zlim = c(1e-06, cellStats(kk_pca12$z.cor,"max")), 
        xlab = "PCA 1", ylab = "PCA 2")
  image(x=grw_pca12$x,y=grw_pca12$y,z=t(as.matrix(grw_pca12$z.uncor))[,nrow(as.matrix(grw_pca12$z.uncor)):1], col = alpha(pal50grw,0.6), zlim = c(1e-06, cellStats(grw_pca12$z.cor,"max")), 
        xlab = "PCA 1", ylab = "PCA 2", add=TRUE)
  
  grw_pca12$uncor.norm<-t(as.matrix(grw_pca12$z.uncor))[,nrow(as.matrix(grw_pca12$z.uncor)):1]
  contour(x=grw_pca12$x,y=grw_pca12$y,grw_pca12$uncor.norm, levels = quantile(grw_pca12$uncor.norm[grw_pca12$uncor.norm > 0], c(0, 0.5,0.75,0.9)), drawlabels = FALSE,
          lty = c(1,2,2,2),col=col1,lwd=2,add=TRUE)
  kk_pca12$uncor.norm<-t(as.matrix(kk_pca12$z.uncor))[,nrow(as.matrix(kk_pca12$z.uncor)):1]
  contour(x=kk_pca12$x,y=kk_pca12$y,kk_pca12$uncor.norm, add = TRUE, levels = quantile(kk_pca12$uncor.norm[kk_pca12$uncor.norm > 0], c(0, 0.5,0.75,0.9)), drawlabels = FALSE,
          lty = c(1,2,2,2),col=col2,lwd=2)
  kk_pca12$Z<-t(as.matrix(kk_pca12$Z))[,nrow(as.matrix(kk_pca12$Z)):1]
  contour(x=kk_pca12$x,y=kk_pca12$y,kk_pca12$Z, add = TRUE, levels = quantile(kk_pca12$Z[kk_pca12$Z > 0], c(0)), drawlabels = FALSE,
          lty = c(1),lwd=2)
  
}

plot.2niche(kk_pca12,grw_pca12,pal50kk,pal50grw,"goldenrod4","green3")
title("a) Great reed warbler compare to reed warbler [D=0.61]", adj = 0)
legend("topright", legend=c("RW","GrW", "Background"),
       col=c("green3","goldenrod4","black"), lty=c(1,1,1),lwd=2,cex=0.8)

plot.2niche(kk_pca12,sn_pca12,pal50kk,pal50sn,"deeppink","green3")
title("b) Savi's warbler compare to reed warbler [D=0.56]", adj = 0)
legend("topright", legend=c("RW","SW", "Background"),
       col=c("green3","deeppink","black"), lty=c(1,1,1),lwd=2,cex=0.8)

plot.2niche(sn_pca12,grw_pca12,pal50sn,pal50grw,"goldenrod4","deeppink")
title("c) Great reed warbler compare to Savi's warbler [D=0.46]", adj = 0)
legend("topright", legend=c("GrW","SW", "Background"),
       col=c("goldenrod4","deeppink","black"), lty=c(1,1,1),lwd=2,cex=0.8)

#plot niche difference
plot.2niche_diff <- function(kk_pca12,grw_pca12,pal50kk,pal50grw,col1,col2) {
  diff=grw_pca12$z.uncor-kk_pca12$z.uncor
  
  image(x=kk_pca12$x,y=kk_pca12$y,z=t(as.matrix(diff))[,nrow(as.matrix(diff)):1], col = alpha(pal50diff,1), zlim = c(-1,1), 
        xlab = "PCA 1", ylab = "PCA 2")
  
  grw_pca12$uncor.norm<-t(as.matrix(grw_pca12$z.uncor))[,nrow(as.matrix(grw_pca12$z.uncor)):1]
  contour(x=grw_pca12$x,y=grw_pca12$y,grw_pca12$uncor.norm, levels = quantile(grw_pca12$uncor.norm[grw_pca12$uncor.norm > 0], c(0, 0.5)), drawlabels = FALSE,
          lty = c(1, 2),col=col1,lwd=2,add=TRUE)
  kk_pca12$uncor.norm<-t(as.matrix(kk_pca12$z.uncor))[,nrow(as.matrix(kk_pca12$z.uncor)):1]
  contour(x=kk_pca12$x,y=kk_pca12$y,kk_pca12$uncor.norm, add = TRUE, levels = quantile(kk_pca12$uncor.norm[kk_pca12$uncor.norm > 0], c(0, 0.5)), drawlabels = FALSE,
          lty = c(1, 2),col=col2,lwd=2)
  kk_pca12$Z<-t(as.matrix(kk_pca12$Z))[,nrow(as.matrix(kk_pca12$Z)):1]
  contour(x=kk_pca12$x,y=kk_pca12$y,kk_pca12$Z, add = TRUE, levels = quantile(kk_pca12$Z[kk_pca12$Z > 0], c(0)), drawlabels = FALSE,
          lty = c(1),lwd=2)
  
}

plot.2niche_diff(kk_pca12,grw_pca12,pal50kk,pal50grw,"goldenrod4","green3")
#title("b) niche difference", adj = 0)
legend("topright", legend=c("RW 100%","RW 50%","GrW 100%", "GrW 50%","Background 100%"),
       col=c("green3","green3","goldenrod4","goldenrod4","black"), lty=c(1,2,1,2,1),lwd=2,cex=0.8)

plot.2niche_diff(kk_pca12,sn_pca12,pal50kk,pal50sn,"deeppink","green3")
#title("b) niche difference", adj = 0)
legend("topright", legend=c("RW 100%","RW 50%","SW 100%", "SW 50%","Background 100%"),
       col=c("green3","green3","deeppink","deeppink","black"), lty=c(1,2,1,2,1),lwd=2,cex=0.8)

plot.2niche_diff(sn_pca12,grw_pca12,pal50sn,pal50grw,"deeppink","goldenrod4")
#title("b) niche difference", adj = 0)
legend("topright", legend=c("GrW 100%","GrW 50%","SW 100%", "SW 50%","Background 100%"),
       col=c("goldenrod4","goldenrod4","deeppink","deeppink","black"), lty=c(1,2,1,2,1),lwd=2,cex=0.8)

# plot color bar
color.bar <- function(lut, min, max, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

color.bar(paldiff(50), -1,1)
color.bar(pal(50), 0,1)
color.bar(pal2(50), 0,1)
color.bar(pal3(50), 0,1)

# plot tests

ecospat.plot.overlap.test2 <- function(x, type, title,pos) {
  if (type == "D") {
    obs <- x$obs$D
    sim <- x$sim$D
    p <- x$p.D
  }
  if (type == "I") {
    obs <- x$obs$I
    sim <- x$sim$I
    p <- x$p.I
  }
  r0 <- c(sim, obs)
  l0 <- max(sim) - min(sim)
  w0 <- l0/(log(length(sim), base = 2) + 1)
  xlim0 <- range(r0) + c(-w0, w0)
  h0 <- hist(sim, plot = FALSE, nclass = 10)
  y0 <- max(h0$counts)
  d=density(sim)
  plot(d,xlim = xlim0, col = "blue", main = title, xlab = "D")
  legend(pos, legend=c(paste("mean sim.D=",round(mean(sim),2)),paste("sd sim.D=",round(sd(sim),2)),
                             paste("obs.D=",round(obs,2))),paste("p.value = ", round(p, 5)),cex=0.8)
  polygon(d, col="lightblue", border="blue")
  lines(c(obs, obs), c(y0/2, 0), col = "red")
  points(obs, y0/2, pch = 18, cex = 2, col = "red")
  invisible()
}

ecospat.plot.overlap.test2(eq_grw_kk,"D","Equivalency **","topleft")
ecospat.plot.overlap.test2(eq_k_s,"D","Equivalency **","topleft")
ecospat.plot.overlap.test2(eq_grw_s,"D","Equivalency **","topleft")

ecospat.plot.overlap.test2(sim.test_gr_k,"D","Similiraty RW->GrW","topright")
ecospat.plot.overlap.test2(sim.test_k_gr,"D","Similiraty GrW->RW","topright")