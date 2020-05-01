library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

# Global
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v13/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,332,41,42,43))

KK=read.csv("KK_wlandsc.csv")
KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,332,41,42,43))

Sn=read.csv("Sn_wlandsc.csv")
Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,332,41,42,43))

Bgr=read.csv("Bgr_wlandsc.csv")
Bgr_lgn8 <- subset(Bgr, lgn8 %in% c(16,17,30,322,332,41,42,43))

#data_merged=rbind(GrW_lgn8,KK_lgn8,Sn_lgn8,Bgr_lgn8)
data_merged=rbind(GrW,KK,Sn,Bgr)

noffea=9

# 200 m only reed
data_merged=subset(data_merged,select=c(11,10,9,7,8,12,14,13,18,15,16,4,5,2))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch",
                        "species","occurrence","x","y","id")

data_merged=data_merged[(data_merged$VV_p95<30),]
data_merged[is.na(data_merged)==TRUE] <- 0


#### Ecospat
pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.env$co[2]=pca.env$co[2]*-1

fviz_pca_var(pca.env,axes = c(1, 2), col.var = "black",repel = TRUE,fontsize=14)

var <- get_pca_var(pca.env)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

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

# PCA 1 vs PCA 2

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500) 

#saveRDS(grid.clim.grotekarakiet, "grw_kdens.rds")
#saveRDS(grid.clim.kleinekarakiet, "kk_kdens.rds")
#saveRDS(grid.clim.snor, "sn_kdens.rds")

# PCA 1 vs PCA 3

grid.clim.grotekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.background[,c(1,3)], sp=scores.sp.grotekarakiet[,c(1,3)], R=500) 
grid.clim.kleinekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.background[,c(1,3)], sp=scores.sp.kleinekarakiet[,c(1,3)], R=500) 
grid.clim.snor2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.background[,c(1,3)], sp=scores.sp.snor[,c(1,3)], R=500) 

#saveRDS(grid.clim.grotekarakiet2, "grw_kdens2.rds")
#saveRDS(grid.clim.kleinekarakiet2, "kk_kdens2.rds")
#saveRDS(grid.clim.snor2, "sn_kdens2.rds")

# PCA 2 vs PCA 3

grid.clim.grotekarakiet3<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(2,3)], glob1=scores.clim.background[,c(2,3)], sp=scores.sp.grotekarakiet[,c(2,3)], R=500) 
grid.clim.kleinekarakiet3<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(2,3)], glob1=scores.clim.background[,c(2,3)], sp=scores.sp.kleinekarakiet[,c(2,3)], R=500) 
grid.clim.snor3<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(2,3)], glob1=scores.clim.background[,c(2,3)], sp=scores.sp.snor[,c(2,3)], R=500) 

#saveRDS(grid.clim.grotekarakiet3, "grw_kdens3.rds")
#saveRDS(grid.clim.kleinekarakiet3, "kk_kdens3.rds")
#saveRDS(grid.clim.snor3, "sn_kdens3.rds")

# Overlaps
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.snor, cor=TRUE)

ecospat.niche.overlap(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet2, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet2, grid.clim.snor2, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet2, grid.clim.snor2, cor=TRUE)

ecospat.niche.overlap(grid.clim.grotekarakiet3, grid.clim.kleinekarakiet3, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet3, grid.clim.snor3, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet3, grid.clim.snor3, cor=TRUE)

# Tests PCA 1 vs PCA 2

# Rand1

sim.test_gr_k_rand1<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_gr_k_rand1, "D", "Similarity")

sim.test_gr_s_rand1<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.snor, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_gr_s_rand1, "D", "Similarity")

sim.test_k_s_rand1<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_k_s_rand1, "D", "Similarity")

saveRDS(sim.test_gr_k_rand1, "sim.test_gr_k_rand1.rds")
saveRDS(sim.test_gr_s_rand1, "sim.test_gr_s_rand1.rds")
saveRDS(sim.test_k_s_rand1, "sim.test_k_s_rand1.rds")

#
sim.test_gr_k2_rand1<-ecospat.niche.similarity.test(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet2, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_gr_k2_rand1, "D", "Similarity")

sim.test_gr_s2_rand1<-ecospat.niche.similarity.test(grid.clim.grotekarakiet2, grid.clim.snor2, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_gr_s2_rand1, "D", "Similarity")

sim.test_k_s2_rand1<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet2, grid.clim.snor2, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_k_s2_rand1, "D", "Similarity")

saveRDS(sim.test_gr_k2_rand1, "sim.test_gr_k2_rand1.rds")
saveRDS(sim.test_gr_s2_rand1, "sim.test_gr_s2_rand1.rds")
saveRDS(sim.test_k_s2_rand1, "sim.test_k_s2_rand1.rds")

#
sim.test_gr_k3_rand1<-ecospat.niche.similarity.test(grid.clim.grotekarakiet3, grid.clim.kleinekarakiet3, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_gr_k_rand1, "D", "Similarity")

sim.test_gr_s3_rand1<-ecospat.niche.similarity.test(grid.clim.grotekarakiet3, grid.clim.snor3, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_gr_s3_rand1, "D", "Similarity")

sim.test_k_s3_rand1<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet3, grid.clim.snor3, rep=1000, alternative = "greater", rand.type = 1,ncores=5)
ecospat.plot.overlap.test(sim.test_k_s3_rand1, "D", "Similarity")

saveRDS(sim.test_gr_k3_rand1, "sim.test_gr_k3_rand1.rds")
saveRDS(sim.test_gr_s3_rand1, "sim.test_gr_s3_rand1.rds")
saveRDS(sim.test_k_s3_rand1, "sim.test_k_s3_rand1.rds")
