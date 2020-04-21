library(ecospat)

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

# PCA 

pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)

# calc densities input
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))
bgr=dplyr::filter(data_merged,str_detect(data_merged$species,"Background"))

scores.globclim<--1*pca.env$li

scores.sp.grotekarakiet<--1*suprow(pca.env,grotekarakiet[,1:noffea])$li
scores.sp.kleinekarakiet<--1*suprow(pca.env,kleinekarakiet[,1:noffea])$li
scores.sp.snor<--1*suprow(pca.env,snor[,1:noffea])$li
scores.clim.background<--1*suprow(pca.env,bgr[,1:noffea])$li

# PCA 1 vs PCA 2 -- rotated

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500) 

saveRDS(grid.clim.grotekarakiet, "grw_kdens_r.rds")
saveRDS(grid.clim.kleinekarakiet, "kk_kdens_r.rds")
saveRDS(grid.clim.snor, "sn_kdens_r.rds")

# PCA 1 vs PCA 2 -- rotated with threshold

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500,th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500,th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500,th.sp=0.5,th.env=0.1) 

saveRDS(grid.clim.grotekarakiet, "grw_kdens_r90.rds")
saveRDS(grid.clim.kleinekarakiet, "kk_kdens_r90.rds")
saveRDS(grid.clim.snor, "sn_kdens_r90.rds")