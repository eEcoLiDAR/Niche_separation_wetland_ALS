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

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v8/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
KK=read.csv("KK_wlandsc.csv")
Sn=read.csv("Sn_wlandsc.csv")
Bgr=read.csv("Bgr_wlandsc.csv")

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

p1=fviz_pca_var(pca.env,axes = c(1, 2), col.var = "contrib",repel = TRUE)+scale_color_gradient2(low="white", mid="blue",high="red", midpoint=7.5)

# loadings

var <- get_pca_var(pca.env)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))
