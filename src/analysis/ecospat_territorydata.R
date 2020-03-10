library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v2/"
setwd(workingdirectory)

GrW=read.csv("GrW_territory_intersected.csv")
KK=read.csv("KK_territory_intersected.csv")
Sn=read.csv("Sn_territory_intersected.csv")

data_merged=rbind(GrW,KK,Sn)

data_presabs_stat <- data_merged %>%
  group_by(species,occurrence) %>%
  summarise(nofobs = length(occurrence))

data_merged=subset(data_merged,select=c(4,5,6,7,8,10,13,16,18,19,20,21))
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd",
                        "lowveg_sd", "lowveg_prop","veg_cover","veg_var","species","occurrence")

data_merged=data_merged[data_merged$veg_height95<20,]


#### Ecospat
pca.env<-dudi.pca(data_merged[,1:10],scannf=FALSE,center=TRUE,nf=3)
pca.env_vis<-dudi.pca(data_merged[,1:10],scannf=FALSE,center=TRUE,nf=2)

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))

scores.globclim<-pca.env$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,12]==1),1:10])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:10])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,12]==1),1:10])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:10])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,12]==1),1:10])$li
scores.clim.snor<-suprow(pca.env,snor[,1:10])$li

# PCA 1 vs PCA 2

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.grotekarakiet[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500, th.sp=0.2,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.kleinekarakiet[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500, th.sp=0.2,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.snor[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500, th.sp=0.2,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")
ecospat.plot.contrib(contrib=pca.env_vis$co, eigen=pca.env_vis$eig)

# PCA 1 vs PCA 3

grid.clim.grotekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.grotekarakiet[,c(1,3)], sp=scores.sp.grotekarakiet[,c(1,3)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.kleinekarakiet[,c(1,3)], sp=scores.sp.kleinekarakiet[,c(1,3)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.snor[,c(1,3)], sp=scores.sp.snor[,c(1,3)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet2,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet2,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor2,title="Savi's Warbler")

# Visualize overlap
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0,
                       interest=1, title= "GrW vs Sn")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "GrW vs RW")
ecospat.plot.niche.dyn(grid.clim.snor, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "Sn vs RW")
ecospat.plot.contrib(contrib=pca.env_vis$co, eigen=pca.env_vis$eig)


# Tests PCA 1 vs PCA 2

# Grote Karakiet

ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=TRUE)

eq.test_gr_k<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "greater")
sim.test_gr_k<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k, "D", "Similarity")


eq.test_gr_s<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "greater")
sim.test_gr_s<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s, "D", "Similarity")

# Kleine Karakiet
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.snor, cor=TRUE)

eq.test_k_gr<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, rep=100, alternative = "greater")
sim.test_k_gr<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_gr, "D", "Similarity")

eq.test_k_s<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "greater")
sim.test_k_s<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s, "D", "Similarity")

# Snor
ecospat.niche.overlap(grid.clim.snor, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=TRUE)

eq.test_s_gr<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.grotekarakiet, rep=100, alternative = "greater")
sim.test_s_gr<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_gr, "D", "Similarity")

eq.test_s_k<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "lower")
sim.test_s_k<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_k, "D", "Similarity")

# Extra visualization
sp_GrW=data.frame(grid.clim.grotekarakiet[["sp"]])
sp_Sn=data.frame(grid.clim.snor[["sp"]])
sp_RW=data.frame(grid.clim.kleinekarakiet[["sp"]])

sp_GrW$species <- "GrW"
sp_Sn$species <- "Sn"
sp_RW$species <- "RW"

sp_pca=rbind(sp_GrW,sp_Sn,sp_RW)

ggplot(sp_pca, aes(x=species, y=Axis1, color=species)) +
  geom_boxplot()

ggplot(sp_pca, aes(x=species, y=Axis2, color=species)) +
  geom_boxplot()