library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

workingdirectory="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/all_10m/onlywetland/"
setwd(workingdirectory)

GrW=read.csv("GrW_territory_intersected_20.csv")
KK=read.csv("KK_territory_intersected_20.csv")
Sn=read.csv("Sn_territory_intersected_20.csv")

data_merged=rbind(GrW,KK,Sn)

data_presabs_stat <- data_merged %>%
  group_by(species,occurrence) %>%
  summarise(nofobs = length(occurrence))

data_merged=subset(data_merged,select=c(4,5,6,7,8,10,13,16,18,19,20,21))
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd",
                        "lowveg_sd", "lowveg_prop","veg_cover","veg_var","species","occurrence")

data_merged=data_merged[data_merged$veg_height95<30,]

#### PCA anal

# All together

par(mfrow=c(1,1))

pca.env<-dudi.pca(data_merged[,1:10],scannf=FALSE,center=TRUE,nf=3)

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


pca.env2<-dudi.pca(data_merged_mod[,1:10],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.env2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_pca_biplot(pca.env2, axes=c(1,2), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod$species), col.ind = "black",
                pointshape = 21, pointsize = 1,
                palette=c("blue","green","purple","red","black"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contribution"))

fviz_pca_biplot(pca.env2, axes=c(1,3), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod$species), col.ind = "black",
                pointshape = 21, pointsize = 1,
                palette=c("blue","green","purple","red","black"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contribution"))

var <- get_pca_var(pca.env2)
factor_loadings=var$cor

corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

# Per species

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))

pca.sp1<-dudi.pca(grotekarakiet[,1:10],scannf=FALSE,center=TRUE,nf=3)
pca.sp2<-dudi.pca(kleinekarakiet[,1:10],scannf=FALSE,center=TRUE,nf=3)
pca.sp3<-dudi.pca(snor[,1:10],scannf=FALSE,center=TRUE,nf=3)

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

#### Ecospat

scores.globclim<-pca.env$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,12]==1),1:10])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:10])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,12]==1),1:10])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:10])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,12]==1),1:10])$li
scores.clim.snor<-suprow(pca.env,snor[,1:10])$li

# PCA 1 vs PCA 2

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.grotekarakiet[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.kleinekarakiet[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.snor[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")

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