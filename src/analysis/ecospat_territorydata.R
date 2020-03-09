library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

workingdirectory="D:/Koma/_PhD/Chapter3/Data_Preprocess/escience_lidar_data_v2/selected_layers_for_chapter3/masked/all_10m/"
setwd(workingdirectory)

GrW=read.csv("GrW_territory_intersected.csv")
KK=read.csv("KK_territory_intersected.csv")
Sn=read.csv("Sn_territory_intersected.csv")

data_merged=rbind(GrW,KK,Sn)

data_merged=subset(data_merged,select=c(4:15))
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

fviz_pca_biplot(pca.env2, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod$species), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette=c("blue","green","purple","black"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contribution"))

fviz_pca_biplot(pca.env2, axes=c(1,3), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged_mod$species.x), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette=c("blue","green","purple","red","black"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contribution"))

var <- get_pca_var(pca.env2)
factor_loadings=var$cor

corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

# ecospat run 1

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))

pca.env<-dudi.pca(data_merged[,1:10],scannf=FALSE,center=TRUE,nf=2)
par(mfrow=c(1,1))
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim<-pca.env$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,12]==1),1:10])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:10])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,12]==1),1:10])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:10])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,12]==1),1:10])$li
scores.clim.snor<-suprow(pca.env,snor[,1:10])$li

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.grotekarakiet, sp=scores.sp.grotekarakiet, R=100, th.sp=0.2,th.env=0.05) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.kleinekarakiet, sp=scores.sp.kleinekarakiet, R=100, th.sp=0.2,th.env=0.05) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.snor, sp=scores.sp.snor, R=100, th.sp=0.2,th.env=0.05) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

### Test
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

eq.test_s_k<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "greater")
sim.test_s_k<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_k, "D", "Similarity")

# Vis

ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0, interest=2,title= "Niche Overlap Great reed warbler vs Savi's warbler", name.axis1="PC1",name.axis2="PC2")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0, interest=2,title= "Niche Overlap Great reed warbler vs Reed warbler", name.axis1="PC1",name.axis2="PC2")
ecospat.plot.niche.dyn(grid.clim.kleinekarakiet, grid.clim.snor, quant=0, interest=2,title= "Niche Overlap Reed warbler vs Savi's warbler", name.axis1="PC1",name.axis2="PC2")

