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

data_presabs_stat <- data_merged %>%
  group_by(species,occurrence) %>%
  summarise(nofobs = length(occurrence))

data_presabs_stat_landcover <- data_merged %>%
  group_by(species,occurrence,lgn8) %>%
  summarise(nofobs = length(occurrence))

data_merged=subset(data_merged,select=c(6,7,8,9,11,12,13,14,15,16,18,19,5))
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_100m",
                        "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","veg_var","species","occurrence","lgn8")

noffea=10

# Loading vs. scale of lidar metrics

pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)

var <- get_pca_var(pca.env)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

#### PCA anal

# All together

par(mfrow=c(1,1))

fviz_pca_var(pca.env, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_pca_var(pca.env, axes = c(1, 3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_pca_var(pca.env, axes = c(2, 3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Absence as seperate species class

data_merged_mod=data_merged
levels(data_merged_mod$species) = c("Grote Karekiet","Kleine Karekiet","Snor", "None")
data_merged_mod[data_merged_mod$occurrence==0,11] <- "None"


pca.env2<-dudi.pca(data_merged_mod[,1:noffea],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.env2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_pca_biplot(pca.env2, axes=c(1,2), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod$species), col.ind = "black",
                pointshape = 21, pointsize = 0.1,
                palette=c("red","green","purple","black"),
                addEllipses = TRUE, ellipse.level = 0.8,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contribution"))

fviz_pca_biplot(pca.env2, axes=c(1,3), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod$species), col.ind = "black",
                pointshape = 21, pointsize = 0.1,
                palette=c("red","green","purple","black"),
                addEllipses = TRUE, ellipse.level = 0.8,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contribution"))

fviz_pca_biplot(pca.env2, axes=c(1,2), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod$lgn8), col.ind = "black",
                pointshape = 21, pointsize = 0.001,
                palette=c("dodgerblue4","deepskyblue4","darkorchid","gold4","darkorange","deeppink","olivedrab","forestgreen"),
                addEllipses = TRUE, ellipse.level = 0.9,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Land cover type", color = "Contribution"))

# Only presence
data_merged_mod_onlypres=data_merged
data_merged_mod_onlypres=data_merged_mod_onlypres[data_merged_mod_onlypres$occurrence>0,]
data_merged_mod_onlypres=data_merged_mod_onlypres[data_merged_mod_onlypres$veg_height95<20,]

data_merged_mod_onlypres <- subset(data_merged_mod_onlypres, lgn8 %in% c(16,17,30,322,332,41,42))

pca.env3<-dudi.pca(data_merged_mod_onlypres[,1:noffea],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.env3, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_pca_biplot(pca.env3, axes=c(1,2), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod_onlypres$species), col.ind = "black",
                pointshape = 21, pointsize = 0.1,
                palette=c("red","green","purple","black"),
                addEllipses = TRUE, ellipse.level = 0.8,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contribution"))

fviz_pca_biplot(pca.env3, axes=c(1,2), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod_onlypres$lgn8), col.ind = "black",
                pointshape = 21, pointsize = 0.001,
                palette=c("dodgerblue4","deepskyblue4","darkorchid","gold4","darkorange","deeppink","olivedrab","forestgreen"),
                addEllipses = TRUE, ellipse.level = 0.9,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Land cover type", color = "Contribution"))

# Per species

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))

pca.sp1<-dudi.pca(grotekarakiet[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.sp2<-dudi.pca(kleinekarakiet[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.sp3<-dudi.pca(snor[,1:noffea],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_biplot(pca.sp1, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(grotekarakiet$occ), col.ind = "black",
                pointshape = 21, pointsize = 1,
                palette = c("blue","red"),
                addEllipses = TRUE, ellipse.level = 0.9,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp2, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(kleinekarakiet$occ), col.ind = "black",
                pointshape = 21, pointsize = 1,
                palette = c("blue","red"),
                addEllipses = TRUE, ellipse.level = 0.9,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp3, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(snor$occ), col.ind = "black",
                pointshape = 21, pointsize = 1,
                palette = c("blue","red"),
                addEllipses = TRUE, ellipse.level = 0.9,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contrib"))

# Niche visualization

#### Ecospat PCA 1 vs PCA 2 all together
pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.env_vis<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=2)

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))

scores.globclim<-pca.env$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,noffea+2]==1),1:noffea])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:noffea])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,noffea+2]==1),1:noffea])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:noffea])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,noffea+2]==1),1:noffea])$li
scores.clim.snor<-suprow(pca.env,snor[,1:noffea])$li

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.grotekarakiet[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.kleinekarakiet[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.snor[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")
ecospat.plot.contrib(contrib=pca.env_vis$co, eigen=pca.env_vis$eig)

# overlap

ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0,
                       interest=1, title= "GrW vs Sn")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "GrW vs RW")
ecospat.plot.niche.dyn(grid.clim.snor, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "Sn vs RW")
ecospat.plot.contrib(contrib=pca.env_vis$co, eigen=pca.env_vis$eig)

ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=FALSE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=FALSE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=FALSE)

#### Ecospat PCA 1 vs PCA 2 just species
pca.env<-dudi.pca(data_merged_mod_onlypres[,1:noffea],scannf=FALSE,center=TRUE,nf=3)
pca.env_vis<-dudi.pca(data_merged_mod_onlypres[,1:noffea],scannf=FALSE,center=TRUE,nf=2)

grotekarakiet=dplyr::filter(data_merged_mod_onlypres,str_detect(data_merged_mod_onlypres$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged_mod_onlypres,str_detect(data_merged_mod_onlypres$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged_mod_onlypres,str_detect(data_merged_mod_onlypres$species,"Snor"))

scores.globclim<-pca.env$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,noffea+2]==1),1:noffea])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:noffea])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,noffea+2]==1),1:noffea])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:noffea])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,noffea+2]==1),1:noffea])$li
scores.clim.snor<-suprow(pca.env,snor[,1:noffea])$li

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.grotekarakiet[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.kleinekarakiet[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.snor[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")
ecospat.plot.contrib(contrib=pca.env_vis$co, eigen=pca.env_vis$eig)

# overlap

ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0,
                       interest=1, title= "GrW vs Sn")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "GrW vs RW")
ecospat.plot.niche.dyn(grid.clim.snor, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "Sn vs RW")
ecospat.plot.contrib(contrib=pca.env_vis$co, eigen=pca.env_vis$eig)

ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=TRUE)

