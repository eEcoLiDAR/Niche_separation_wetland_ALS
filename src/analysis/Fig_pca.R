library(dplyr)
library(stringr)
library(tidyr)

library(FactoMineR)
library(factoextra)
library(corrplot)

library(ecospat)

library(ggplot2)
library(gridExtra)
library(GGally)

library(egg)
library(ecospat)
library(raster)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v8/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
KK=read.csv("KK_wlandsc.csv")
Sn=read.csv("Sn_wlandsc.csv")
Bgr=read.csv("Bgr_wlandsc.csv")

grw_pca12 <- readRDS("grw_kdens_r.rds")
kk_pca12 <- readRDS("kk_kdens_r.rds")
sn_pca12 <- readRDS("sn_kdens_r.rds")

# PCA plot
data_merged=rbind(GrW,KK,Sn,Bgr)

noffea=10

# 200 m only reed
data_merged=subset(data_merged,select=c(11,10,9,7,8,12,14,13,18,22,15,16,4,5,2))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "species","occurrence","x","y","id")

data_merged=data_merged[(data_merged$VV_p95<30),]
data_merged[is.na(data_merged)==TRUE] <- 0

# PCA -- Fig.2. a PCA 1,2

pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.env$co=pca.env$co*-1

p1=fviz_pca_var(pca.env,axes = c(1, 2), col.var = "black",repel = TRUE,fontsize=14)

p1+labs(x = "PCA 1 (33.4%)", y = "PCA 2 (21.5%)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=14))

# loadings

var <- get_pca_var(pca.env)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

# simple density plots in Rplot
ecospat.plot.niche2 <- function(z, title = "", name.axis1 = "Axis 1", name.axis2 = "Axis 2", cor = FALSE, pal50kk) {
  if (is.null(z$y)) {
    R <- length(z$x)
    x <- z$x
    xx <- sort(rep(1:length(x), 2))
    if (cor == FALSE)
      y1 <- z$z.uncor/max(z$z.uncor)
    if (cor == TRUE)
      y1 <- z$z.cor/max(z$z.cor)
    Y1 <- z$Z/max(z$Z)
    yy1 <- sort(rep(1:length(y1), 2))[-c(1:2, length(y1) * 2)]
    YY1 <- sort(rep(1:length(Y1), 2))[-c(1:2, length(Y1) * 2)]
    plot(x, y1, type = "n", xlab = name.axis1, ylab = "density of occurrence")
    polygon(x[xx], c(0, y1[yy1], 0, 0), col = "grey")
    lines(x[xx], c(0, Y1[YY1], 0, 0))
  }
  if (!is.null(z$y)) {
    if (cor == FALSE)
      image(x=z$x,y=z$y,z=t(as.matrix(z$z.uncor))[,nrow(as.matrix(z$z.uncor)):1], col = pal50kk, zlim = c(1e-06, cellStats(z$z.uncor,"max")),
            xlab = name.axis1, ylab = name.axis2)
    if (cor == TRUE)
      image(x=z$x,y=z$y,z=t(as.matrix(z$z.uncor))[,nrow(as.matrix(z$z.uncor)):1], col = pal50kk, zlim = c(1e-06, cellStats(z$z.cor,"max")), 
            xlab = name.axis1, ylab = name.axis2)
    z$Z<-t(as.matrix(z$Z))[,nrow(as.matrix(z$Z)):1]
    contour(x=z$x,y=z$y,z$Z, add = TRUE, levels = quantile(z$Z[z$Z > 0], c(0, 0.5)), drawlabels = FALSE,
            lty = c(1, 2))
  }
  title(title)
}

pal <- colorRampPalette(c("grey95", "goldenrod4"))
pal50grw<- pal(50)

ecospat.plot.niche2(grw_pca12,title="Great Reed Warbler",name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50grw)

pal2 <- colorRampPalette(c("grey95", "green3"))
pal50kk<- pal2(50)

ecospat.plot.niche2(kk_pca12,title="Reed Warbler",name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50kk)

pal3 <- colorRampPalette(c("grey95", "deeppink"))
pal50sn<- pal3(50)

ecospat.plot.niche2(sn_pca12,title="Savi's warbler",name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50sn)

# simple PCA plot in Rplot

pca.env_vis<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=2)

s.arrow(-1*pca.env_vis$co, clab = 1,boxes=FALSE)

s.corcircle(-1*pca.env_vis$co[, 1:2]/max(abs(-1*pca.env_vis$co[, 1:2])),cgrid = 2,clab = 1,possub="topleft",grid = FALSE,csub = 2.5,box=FALSE)
title(sub = paste("PCA 1 = ", round(pca.env_vis$eig[1]/sum(pca.env_vis$eig) *100, 2), "%", "PCA 2 = ", round(pca.env_vis$eig[2]/sum(pca.env_vis$eig) * 100, 2), "%"))

s.corcircle(-1*pca.env_vis$co, cgrid = 2, 
            full = TRUE, csub = 2.5, box = FALSE,grid=FALSE)

# altogether

par(mfrow=c(2,2))
ecospat.plot.contrib(contrib=-1*pca.env_vis$co, eigen=pca.env_vis$eig)
ecospat.plot.niche2(grw_pca12,title="Great Reed Warbler",name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50grw)
ecospat.plot.niche2(kk_pca12,title="Reed Warbler",name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50kk)
ecospat.plot.niche2(sn_pca12,title="Savi's warbler",name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50sn)
