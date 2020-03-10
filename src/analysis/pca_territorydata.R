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

data_merged_100m=subset(data_merged,select=c(4,5,6,7,8,10,13,16,18,19,20,21))
names(data_merged_100m) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_100m",
                        "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","veg_var","species","occurrence")

data_merged_50m=subset(data_merged,select=c(4,5,6,7,8,9,12,15,18,19,20,21))
names(data_merged_50m) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_50m",
                        "lowveg_sd_50m", "lowveg_prop_50m","veg_cover","veg_var","species","occurrence")

data_merged_250m=subset(data_merged,select=c(4,5,6,7,8,11,14,17,18,19,20,21))
names(data_merged_250m) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_250m",
                        "lowveg_sd_250m", "lowveg_prop_250m","veg_cover","veg_var","species","occurrence")

# Loading vs. scale of lidar metrics

pca.env_100m<-dudi.pca(data_merged_100m[,1:10],scannf=FALSE,center=TRUE,nf=3)
pca.env_50m<-dudi.pca(data_merged_50m[,1:10],scannf=FALSE,center=TRUE,nf=3)
pca.env_250m<-dudi.pca(data_merged_250m[,1:10],scannf=FALSE,center=TRUE,nf=3)

var <- get_pca_var(pca.env_100m)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

var <- get_pca_var(pca.env_50m)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

var <- get_pca_var(pca.env_250m)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

#### PCA anal

data_merged=subset(data_merged,select=c(4,5,6,7,8,10,13,16,18,19,20,21))
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_100m",
                             "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","veg_var","species","occurrence")

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