library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

# Global
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v11/"
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
#pca.env$co=pca.env$co*-1

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

# PCA 1 vs PCA 2

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.background[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500) 

saveRDS(grid.clim.grotekarakiet, "grw_kdens.rds")
saveRDS(grid.clim.kleinekarakiet, "kk_kdens.rds")
saveRDS(grid.clim.snor, "sn_kdens.rds")

# PCA 1 vs PCA 3

grid.clim.grotekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.background[,c(1,3)], sp=scores.sp.grotekarakiet[,c(1,3)], R=500) 
grid.clim.kleinekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.background[,c(1,3)], sp=scores.sp.kleinekarakiet[,c(1,3)], R=500) 
grid.clim.snor2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.background[,c(1,3)], sp=scores.sp.snor[,c(1,3)], R=500) 

saveRDS(grid.clim.grotekarakiet2, "grw_kdens2.rds")
saveRDS(grid.clim.kleinekarakiet2, "kk_kdens2.rds")
saveRDS(grid.clim.snor2, "sn_kdens2.rds")

# PCA 2 vs PCA 3

grid.clim.grotekarakiet3<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(2,3)], glob1=scores.clim.background[,c(2,3)], sp=scores.sp.grotekarakiet[,c(2,3)], R=500) 
grid.clim.kleinekarakiet3<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(2,3)], glob1=scores.clim.background[,c(2,3)], sp=scores.sp.kleinekarakiet[,c(2,3)], R=500) 
grid.clim.snor3<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(2,3)], glob1=scores.clim.background[,c(2,3)], sp=scores.sp.snor[,c(2,3)], R=500) 

saveRDS(grid.clim.grotekarakiet3, "grw_kdens3.rds")
saveRDS(grid.clim.kleinekarakiet3, "kk_kdens3.rds")
saveRDS(grid.clim.snor3, "sn_kdens3.rds")

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

# Grote Karakiet
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=TRUE)

eq.test_gr_k<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "lower")
sim.test_gr_k<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k, "D", "Similarity")

eq.test_gr_s<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "lower")
sim.test_gr_s<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s, "D", "Similarity")

saveRDS(eq.test_gr_k, "eq.test_gr_k.rds")
saveRDS(sim.test_gr_k, "sim.test_gr_k.rds")
saveRDS(eq.test_gr_s, "eq.test_gr_s.rds")
saveRDS(sim.test_gr_s, "sim.test_gr_s.rds")

# Kleine Karakiet
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.snor, cor=TRUE)

sim.test_k_gr<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(sim.test_k_gr, "D", "Similarity")

eq.test_k_s<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "lower")
sim.test_k_s<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s, "D", "Similarity")

saveRDS(sim.test_k_gr, "sim.test_k_gr.rds")
saveRDS(eq.test_k_s, "eq.test_k_s.rds")
saveRDS(sim.test_k_s, "sim.test_k_s.rds")

# Snor

sim.test_s_gr<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)
ecospat.plot.overlap.test(sim.test_s_gr, "D", "Similarity")

sim.test_s_k<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)
ecospat.plot.overlap.test(sim.test_s_k, "D", "Similarity")

saveRDS(sim.test_s_gr, "sim.test_s_gr.rds")
saveRDS(sim.test_s_k, "sim.test_s_k.rds")

# Tests PCA 1 vs PCA 3

# Grote Karakiet
ecospat.niche.overlap(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet2, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet2, grid.clim.snor2, cor=TRUE)

eq.test_gr_k2<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet2, rep=100, alternative = "lower")
sim.test_gr_k2<-ecospat.niche.similarity.test(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_k2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k2, "D", "Similarity")

eq.test_gr_s2<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet2, grid.clim.snor2, rep=100, alternative = "lower")
sim.test_gr_s2<-ecospat.niche.similarity.test(grid.clim.grotekarakiet2, grid.clim.snor2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_s2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s2, "D", "Similarity")

saveRDS(eq.test_gr_k2, "eq.test_gr_k2.rds")
saveRDS(sim.test_gr_k2, "sim.test_gr_k2.rds")
saveRDS(eq.test_gr_s2, "eq.test_gr_s2.rds")
saveRDS(sim.test_gr_s2, "sim.test_gr_s2.rds")

# Kleine Karakiet
ecospat.niche.overlap(grid.clim.kleinekarakiet2, grid.clim.snor2, cor=TRUE)

sim.test_k_gr2<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet2, grid.clim.grotekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(sim.test_k_gr2, "D", "Similarity")

eq.test_k_s2<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet2, grid.clim.snor2, rep=100, alternative = "lower")
sim.test_k_s2<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet2, grid.clim.snor2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_s2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s2, "D", "Similarity")

saveRDS(sim.test_k_gr2, "sim.test_k_gr2.rds")
saveRDS(eq.test_k_s2, "eq.test_k_s2.rds")
saveRDS(sim.test_k_s2, "sim.test_k_s2.rds")

# Snor

sim.test_s_gr2<-ecospat.niche.similarity.test(grid.clim.snor2, grid.clim.grotekarakiet2, rep=100, alternative = "greater", rand.type = 2)
ecospat.plot.overlap.test(sim.test_s_gr2, "D", "Similarity")

sim.test_s_k2<-ecospat.niche.similarity.test(grid.clim.snor2, grid.clim.kleinekarakiet2, rep=100, alternative = "greater", rand.type = 2)
ecospat.plot.overlap.test(sim.test_s_k2, "D", "Similarity")

saveRDS(sim.test_s_gr2, "sim.test_s_gr2.rds")
saveRDS(sim.test_s_k2, "sim.test_s_k2.rds")

# Tests PCA 2 vs PCA 3

# Grote Karakiet
ecospat.niche.overlap(grid.clim.grotekarakiet3, grid.clim.kleinekarakiet3, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet3, grid.clim.snor3, cor=TRUE)

eq.test_gr_k3<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet3, grid.clim.kleinekarakiet3, rep=100, alternative = "lower")
sim.test_gr_k3<-ecospat.niche.similarity.test(grid.clim.grotekarakiet3, grid.clim.kleinekarakiet3, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_k3, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k3, "D", "Similarity")

eq.test_gr_s3<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet3, grid.clim.snor3, rep=100, alternative = "lower")
sim.test_gr_s3<-ecospat.niche.similarity.test(grid.clim.grotekarakiet3, grid.clim.snor3, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_s3, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s3, "D", "Similarity")

saveRDS(eq.test_gr_k3, "eq.test_gr_k3.rds")
saveRDS(sim.test_gr_k3, "sim.test_gr_k3.rds")
saveRDS(eq.test_gr_s3, "eq.test_gr_s3.rds")
saveRDS(sim.test_gr_s3, "sim.test_gr_s3.rds")

# Kleine Karakiet
ecospat.niche.overlap(grid.clim.kleinekarakiet3, grid.clim.snor3, cor=TRUE)

sim.test_k_gr3<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet3, grid.clim.grotekarakiet3, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(sim.test_k_gr3, "D", "Similarity")

eq.test_k_s3<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet3, grid.clim.snor3, rep=100, alternative = "lower")
sim.test_k_s3<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet3, grid.clim.snor3, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_s3, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s3, "D", "Similarity")

saveRDS(sim.test_k_gr3, "sim.test_k_gr3.rds")
saveRDS(eq.test_k_s3, "eq.test_k_s3.rds")
saveRDS(sim.test_k_s3, "sim.test_k_s3.rds")

# Snor

sim.test_s_gr3<-ecospat.niche.similarity.test(grid.clim.snor3, grid.clim.grotekarakiet3, rep=100, alternative = "greater", rand.type = 2)
ecospat.plot.overlap.test(sim.test_s_gr3, "D", "Similarity")

sim.test_s_k3<-ecospat.niche.similarity.test(grid.clim.snor3, grid.clim.kleinekarakiet3, rep=100, alternative = "greater", rand.type = 2)
ecospat.plot.overlap.test(sim.test_s_k3, "D", "Similarity")

saveRDS(sim.test_s_gr3, "sim.test_s_gr3.rds")
saveRDS(sim.test_s_k3, "sim.test_s_k3.rds")

