library(ecospat)
library(dplyr)
library(stringr)
library(tidyr)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v11/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
KK=read.csv("KK_wlandsc.csv")
Sn=read.csv("Sn_wlandsc.csv")
Bgr=read.csv("Bgr_wlandsc.csv")

data_merged=rbind(GrW,KK,Sn,Bgr)

noffea=9

# 200 m only reed
data_merged=subset(data_merged,select=c(11,10,9,7,8,12,14,13,18,15,16,4,5,2))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch",
                        "species","occurrence","x","y","id")

data_merged=data_merged[(data_merged$VV_p95<30),]
data_merged[is.na(data_merged)==TRUE] <- 0

# PCA 

pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.env$co[2]=pca.env$co[2]*-1

# calc densities input
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))
bgr=dplyr::filter(data_merged,str_detect(data_merged$species,"Background"))

scores.globclim<-pca.env$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:noffea])$li
scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:noffea])$li
scores.sp.snor<-suprow(pca.env,snor[,1:noffea])$li
scores.clim.background<-suprow(pca.env,bgr[,1:noffea])$li

# rotate
scores.globclim$Axis2=scores.globclim$Axis2*-1

scores.sp.grotekarakiet$Axis2=scores.sp.grotekarakiet$Axis2*-1
scores.sp.kleinekarakiet$Axis2=scores.sp.kleinekarakiet$Axis2*-1
scores.sp.snor$Axis2=scores.sp.snor$Axis2*-1
scores.clim.background$Axis2=scores.clim.background$Axis2*-1

# PCA 1 vs PCA 2 -- rotated

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500) 

saveRDS(grid.clim.grotekarakiet, "grw_kdens_r.rds")
saveRDS(grid.clim.kleinekarakiet, "kk_kdens_r.rds")
saveRDS(grid.clim.snor, "sn_kdens_r.rds")

# PCA 1 vs PCA 2 -- rotated with threshold

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500,th.sp=0.5,th.env=0.5) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500,th.sp=0.5,th.env=0.5) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500,th.sp=0.5,th.env=0.5) 

saveRDS(grid.clim.grotekarakiet, "grw_kdens_r50.rds")
saveRDS(grid.clim.kleinekarakiet, "kk_kdens_r50.rds")
saveRDS(grid.clim.snor, "sn_kdens_r50.rds")