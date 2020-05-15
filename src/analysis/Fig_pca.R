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
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v12/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
KK=read.csv("KK_wlandsc.csv")
Sn=read.csv("Sn_wlandsc.csv")
Bgr=read.csv("Bgr_wlandsc.csv")

grw_pca12 <- readRDS("grw_kdens.rds")
kk_pca12 <- readRDS("kk_kdens.rds")
sn_pca12 <- readRDS("sn_kdens.rds")

# PCA plot
data_merged=rbind(GrW,KK,Sn,Bgr)

noffea=9

# 200 m only reed
data_merged=subset(data_merged,select=c(11,10,9,7,8,12,14,13,18,15,16,4,5,2))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch",
                        "species","occurrence","x","y","id")

data_merged=data_merged[(data_merged$VV_p95<30),]
data_merged[is.na(data_merged)==TRUE] <- 0

# PCA -- Fig.2. a PCA 1,2

pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.env$co[2]=pca.env$co[2]*-1

p1=fviz_pca_var(pca.env,axes = c(1, 2), col.var = "black",repel = TRUE,fontsize=14)

p1+labs(x = "PCA 1 (37.2%)", y = "PCA 2 (23.9%)")+theme(axis.text=element_text(family="Sans",size=14),axis.title=element_text(family="Sans",size=14))+ggtitle("a) PCA")+
  theme(text=element_text(family="Sans", size=14))

# loadings

var <- get_pca_var(pca.env)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

# simple density plots in Rplot
ecospat.plot.niche2 <- function(z, name.axis1 = "Axis 1", name.axis2 = "Axis 2", cor = FALSE, pal50kk,col_line) {
  if (!is.null(z$y)) {
    if (cor == FALSE)
      image(x=z$x,y=z$y,z=t(as.matrix(z$z.uncor))[,nrow(as.matrix(z$z.uncor)):1], col = pal50kk, zlim = c(1e-06, cellStats(z$z.uncor,"max")),
            xlab = name.axis1, ylab = name.axis2)
    if (cor == TRUE)
      image(x=z$x,y=z$y,z=t(as.matrix(z$z.uncor))[,nrow(as.matrix(z$z.uncor)):1], col = pal50kk, zlim = c(1e-06, cellStats(z$z.cor,"max")), 
            xlab = name.axis1, ylab = name.axis2)
    z$Z<-t(as.matrix(z$Z))[,nrow(as.matrix(z$Z)):1]
    contour(x=z$x,y=z$y,z$Z, add = TRUE, levels = quantile(z$Z[z$Z > 0], c(0,0.5)), drawlabels = FALSE,
            lty = c(1,2),lwd=2)
    
    z$uncor.norm<-t(as.matrix(z$z.uncor))[,nrow(as.matrix(z$z.uncor)):1]
    contour(x=z$x,y=z$y,z$uncor.norm, add = TRUE, levels = quantile(z$uncor.norm[z$uncor.norm > 0], c(0, 0.5)), drawlabels = FALSE,
            lty = c(1, 2),col=col_line,lwd=2)
  }
}

pal <- colorRampPalette(c("grey95", "goldenrod4"))
pal50grw<- pal(50)
par(cex.lab=1.5, cex.axis=1.5, cex.main=2)

ecospat.plot.niche2(grw_pca12,name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50grw,"goldenrod4")
title("b) Great reed warbler", adj = 0)
legend("topright", legend=c("Background 100%","Background 50%","Occ.niche 100% = 64%", "Occ.niche 50% = 4.3%"),
       col=c("black","black","goldenrod4","goldenrod4"), lty=c(1,2,1,2),lwd=2)

pal2 <- colorRampPalette(c("grey95", "green3"))
pal50kk<- pal2(50)
par(cex.lab=1.5, cex.axis=1.5, cex.main=2)

ecospat.plot.niche2(kk_pca12,name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50kk,"green3")
title("c) Reed warbler", adj = 0)
legend("topright", legend=c("Background 100%","Background 50%","Occ.niche 100% = 94%", "Occ.niche 50% = 19.5%"),
       col=c("black","black","green3","green3"), lty=c(1,2,1,2),lwd=2)

pal3 <- colorRampPalette(c("grey95", "deeppink"))
pal50sn<- pal3(50)
par(cex.lab=1.5, cex.axis=1.5, cex.main=2)

ecospat.plot.niche2(sn_pca12,name.axis1 = "PCA 1",name.axis2 = "PCA 2",cor = FALSE,pal50sn,"deeppink")
title("d) Savi's warbler", adj = 0)
legend("topright", legend=c("Background 100%","Background 50%","Occ.niche 100% = 76%", "Occ.niche 50% = 13.5%"),
       col=c("black","black","deeppink","deeppink"), lty=c(1,2,1,2),lwd=2)



