library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
setwd(workingdirectory)

GrW=read.csv("GrW_territory_intersected.csv")
#GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,332,41,42,43))

KK=read.csv("KK_territory_intersected.csv")
#KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,332,41,42,43))

Sn=read.csv("Sn_territory_intersected.csv")
#Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,332,41,42,43))

#data_merged=rbind(GrW,KK,Sn)
data_merged=rbind(GrW_lgn8,KK_lgn8,Sn_lgn8)

data_merged=subset(data_merged,select=c(6,7,8,9,11,12,13,14,15,16,18,19,5))
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_100m",
                        "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","veg_var","species","occurrence","lgn8")

noffea=10

# per feature group

pca.fea_g1<-dudi.pca(data_merged[,c(4,9)],scannf=FALSE,center=TRUE,nf=2)

fviz_pca_var(pca.fea_g1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

pca.fea_g2<-dudi.pca(data_merged[,c(1,2,3)],scannf=FALSE,center=TRUE,nf=2)

fviz_pca_var(pca.fea_g2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

pca.fea_g3<-dudi.pca(data_merged[,c(6,7,8)],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.fea_g3, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

# apply group based pca

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))

pca.fea_g1<-dudi.pca(data_merged[,c(4,9)],scannf=FALSE,center=TRUE,nf=1)
pca.fea_g2<-dudi.pca(data_merged[,c(1,2,3)],scannf=FALSE,center=TRUE,nf=1)
pca.fea_g3<-dudi.pca(data_merged[,c(6,7,8)],scannf=FALSE,center=TRUE,nf=1)

scores.globclim_g1<-pca.fea_g1$li
scores.globclim_g2<-pca.fea_g2$li
scores.globclim_g3<-pca.fea_g3$li

scores.globclim=cbind(scores.globclim_g1,scores.globclim_g2,scores.globclim_g3)

scores.sp.grotekarakiet_g1<-suprow(pca.fea_g1,grotekarakiet[which(grotekarakiet[,noffea+2]==1),c(4,9)])$li
scores.clim.grotekarakiet_g1<-suprow(pca.fea_g1,grotekarakiet[,c(4,9)])$li
scores.sp.grotekarakiet_g2<-suprow(pca.fea_g2,grotekarakiet[which(grotekarakiet[,noffea+2]==1),c(1,2,3)])$li
scores.clim.grotekarakiet_g2<-suprow(pca.fea_g2,grotekarakiet[,c(1,2,3)])$li
scores.sp.grotekarakiet_g3<-suprow(pca.fea_g3,grotekarakiet[which(grotekarakiet[,noffea+2]==1),c(6,7,8)])$li
scores.clim.grotekarakiet_g3<-suprow(pca.fea_g3,grotekarakiet[,c(6,7,8)])$li

scores.sp.grotekarakiet=cbind(scores.sp.grotekarakiet_g1,scores.sp.grotekarakiet_g2,scores.sp.grotekarakiet_g3)
scores.clim.grotekarakiet=cbind(scores.clim.grotekarakiet_g1,scores.clim.grotekarakiet_g2,scores.clim.grotekarakiet_g3)

scores.sp.snor_g1<-suprow(pca.fea_g1,snor[which(snor[,noffea+2]==1),c(4,9)])$li
scores.clim.snor_g1<-suprow(pca.fea_g1,snor[,c(4,9)])$li
scores.sp.snor_g2<-suprow(pca.fea_g2,snor[which(snor[,noffea+2]==1),c(1,2,3)])$li
scores.clim.snor_g2<-suprow(pca.fea_g2,snor[,c(1,2,3)])$li
scores.sp.snor_g3<-suprow(pca.fea_g3,snor[which(snor[,noffea+2]==1),c(6,7,8)])$li
scores.clim.snor_g3<-suprow(pca.fea_g3,snor[,c(6,7,8)])$li

scores.sp.snor=cbind(scores.sp.snor_g1,scores.sp.snor_g2,scores.sp.snor_g3)
scores.clim.snor=cbind(scores.clim.snor_g1,scores.clim.snor_g2,scores.clim.snor_g3)

scores.sp.kleinekarakiet_g1<-suprow(pca.fea_g1,kleinekarakiet[which(kleinekarakiet[,noffea+2]==1),c(4,9)])$li
scores.clim.kleinekarakiet_g1<-suprow(pca.fea_g1,kleinekarakiet[,c(4,9)])$li
scores.sp.kleinekarakiet_g2<-suprow(pca.fea_g2,kleinekarakiet[which(kleinekarakiet[,noffea+2]==1),c(1,2,3)])$li
scores.clim.kleinekarakiet_g2<-suprow(pca.fea_g2,kleinekarakiet[,c(1,2,3)])$li
scores.sp.kleinekarakiet_g3<-suprow(pca.fea_g3,kleinekarakiet[which(kleinekarakiet[,noffea+2]==1),c(6,7,8)])$li
scores.clim.kleinekarakiet_g3<-suprow(pca.fea_g3,kleinekarakiet[,c(6,7,8)])$li

scores.sp.kleinekarakiet=cbind(scores.sp.kleinekarakiet_g1,scores.sp.kleinekarakiet_g2,scores.sp.kleinekarakiet_g3)
scores.clim.kleinekarakiet=cbind(scores.clim.kleinekarakiet_g1,scores.clim.kleinekarakiet_g2,scores.clim.kleinekarakiet_g3)

# g1 vs g2
grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.grotekarakiet[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.snor[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.kleinekarakiet[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")

# overlap
par(mfrow=c(2,2))
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0,
                       interest=1, title= "GrW vs Sn")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "GrW vs RW")
ecospat.plot.niche.dyn(grid.clim.snor, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "Sn vs RW")


ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=FALSE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=FALSE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=FALSE)

# g1 vs g3
grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.grotekarakiet[,c(1,3)], sp=scores.sp.grotekarakiet[,c(1,3)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.snor[,c(1,3)], sp=scores.sp.snor[,c(1,3)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.kleinekarakiet[,c(1,3)], sp=scores.sp.kleinekarakiet[,c(1,3)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")

# overlap
par(mfrow=c(2,2))
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0,
                       interest=1, title= "GrW vs Sn")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "GrW vs RW")
ecospat.plot.niche.dyn(grid.clim.snor, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "Sn vs RW")


ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=FALSE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=FALSE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=FALSE)


