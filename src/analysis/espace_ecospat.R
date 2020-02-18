library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

library(xlsx)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/processed/20200217/"
setwd(workingdirectory)

#### Import data

data=read.csv("veg_metrics_100m_pres.csv")
dataabs=read.csv("veg_metrics_100m_abs.csv")

data_sub=subset(data,select=c(5:16,20,34,38,39))
dataabs_sub=subset(dataabs,select=c(5:16,29,31,38,39))

data_merged=rbind(data_sub,dataabs_sub)
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height25","veg_height95","dsm_sd",
                        "lowveg_sd", "lowveg_prop", "trees_prop",
                        "veg_cover","veg_var","species.x","occ","x","y")

data_merged=data_merged[data_merged$species.x!="Baardman",]

#### Filters per species

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Kleine Karekiet"))
rietzanger=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Rietzanger"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Snor"))

#### PCA anal

# All together

par(mfrow=c(1,1))

pca.env<-dudi.pca(data_merged[,1:12],scannf=FALSE,center=TRUE,nf=3)

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
levels(data_merged_mod$species.x) = c("Baardman","Grote Karekiet","Kleine Karekiet","Rietzanger","Snor", "None")
data_merged_mod[data_merged_mod$occ==0,13] <- "None"


pca.env2<-dudi.pca(data_merged_mod[,1:12],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.env2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_pca_biplot(pca.env2, 
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

write.xlsx(factor_loadings, "PCA_loading_factors.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

# Per species

pca.sp1<-dudi.pca(grotekarakiet[,1:12],scannf=FALSE,center=TRUE,nf=3)
pca.sp2<-dudi.pca(kleinekarakiet[,1:12],scannf=FALSE,center=TRUE,nf=3)
pca.sp3<-dudi.pca(rietzanger[,1:12],scannf=FALSE,center=TRUE,nf=3)
pca.sp4<-dudi.pca(snor[,1:12],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_biplot(pca.sp1, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(grotekarakiet$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp2, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(kleinekarakiet$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp3, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(rietzanger$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp4, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(snor$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                legend.title = list(fill = "Species", color = "Contrib"))

# per feature group

pca.fea_g1<-dudi.pca(data_merged[,c(4,6,11,12)],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.fea_g1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_contrib(pca.fea_g1, choice = "var", axes = 1, top = 5)

pca.fea_g2<-dudi.pca(data_merged[,c(1,2,3,5)],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.fea_g2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_contrib(pca.fea_g2, choice = "var", axes = 1, top = 5)

pca.fea_g3<-dudi.pca(data_merged[,c(7,8,9,10)],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.fea_g3, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_contrib(pca.fea_g3, choice = "var", axes = 1, top = 5)

#### Ecospat

scores.globclim<-pca.env$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,14]==1),1:12])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:12])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,14]==1),1:12])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:12])$li

scores.sp.rietzanger<-suprow(pca.env,rietzanger[which(rietzanger[,14]==1),1:12])$li
scores.clim.rietzanger<-suprow(pca.env,rietzanger[,1:12])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,14]==1),1:12])$li
scores.clim.snor<-suprow(pca.env,snor[,1:12])$li

# PCA 1 vs PCA 2

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.grotekarakiet[,c(1,2)], sp=scores.sp.grotekarakiet[,c(1,2)], R=100, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.kleinekarakiet[,c(1,2)], sp=scores.sp.kleinekarakiet[,c(1,2)], R=100, th.sp=0.1,th.env=0.1) 
grid.clim.rietzanger<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.rietzanger[,c(1,2)], sp=scores.sp.rietzanger[,c(1,2)], R=100, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,2)], glob1=scores.clim.snor[,c(1,2)], sp=scores.sp.snor[,c(1,2)], R=100, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.rietzanger,title="Sedge Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")

# PCA 1 vs PCA 3

grid.clim.grotekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.grotekarakiet[,c(1,3)], sp=scores.sp.grotekarakiet[,c(1,3)], R=100, th.sp=0.1,th.env=0.1) 
grid.clim.kleinekarakiet2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.kleinekarakiet[,c(1,3)], sp=scores.sp.kleinekarakiet[,c(1,3)], R=100, th.sp=0.1,th.env=0.1) 
grid.clim.rietzanger2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.rietzanger[,c(1,3)], sp=scores.sp.rietzanger[,c(1,3)], R=100, th.sp=0.1,th.env=0.1) 
grid.clim.snor2<-ecospat.grid.clim.dyn(glob=scores.globclim[,c(1,3)], glob1=scores.clim.snor[,c(1,3)], sp=scores.sp.snor[,c(1,3)], R=100, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet2,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet2,title="Reed Warbler")
ecospat.plot.niche(grid.clim.rietzanger2,title="Sedge Warbler")
ecospat.plot.niche(grid.clim.snor2,title="Savi's Warbler")

# Tests PCA 1 vs PCA 2

# Grote Karakiet

ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.rietzanger, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=TRUE)

eq.test_gr_k<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "lower")
sim.test_gr_k<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k, "D", "Similarity")

eq.test_gr_r<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.rietzanger, rep=100, alternative = "lower")
sim.test_gr_r<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.rietzanger, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_r, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_r, "D", "Similarity")

eq.test_gr_s<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "lower")
eq.test_gr_s<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "greater")
sim.test_gr_s<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s, "D", "Similarity")

# Kleine Karakiet
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.rietzanger, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.snor, cor=TRUE)

eq.test_k_gr<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, rep=100, alternative = "lower")
sim.test_k_gr<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_gr, "D", "Similarity")

eq.test_k_r<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.rietzanger, rep=100, alternative = "lower")
sim.test_k_r<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.rietzanger, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_r, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_r, "D", "Similarity")

eq.test_k_s<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "lower")
eq.test_k_s<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "greater")
sim.test_k_s<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s, "D", "Similarity")

# Rietzanger

ecospat.niche.overlap(grid.clim.rietzanger, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.rietzanger, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.rietzanger, grid.clim.snor, cor=TRUE)

eq.test_r_gr<-ecospat.niche.equivalency.test(grid.clim.rietzanger, grid.clim.grotekarakiet, rep=100, alternative = "lower")
sim.test_r_gr<-ecospat.niche.similarity.test(grid.clim.rietzanger, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_gr, "D", "Similarity")

eq.test_r_k<-ecospat.niche.equivalency.test(grid.clim.rietzanger, grid.clim.kleinekarakiet, rep=100, alternative = "lower")
sim.test_r_k<-ecospat.niche.similarity.test(grid.clim.rietzanger, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_k, "D", "Similarity")

eq.test_r_s<-ecospat.niche.equivalency.test(grid.clim.rietzanger, grid.clim.snor, rep=100, alternative = "lower")
sim.test_r_s<-ecospat.niche.similarity.test(grid.clim.rietzanger, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_s, "D", "Similarity")

# Snor
ecospat.niche.overlap(grid.clim.snor, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.rietzanger, cor=TRUE)

eq.test_s_gr<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.grotekarakiet, rep=100, alternative = "lower")
sim.test_s_gr<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_gr, "D", "Similarity")

eq.test_s_k<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "lower")
sim.test_s_k<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_k, "D", "Similarity")

eq.test_s_r<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.rietzanger, rep=100, alternative = "lower")
sim.test_s_r<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.rietzanger, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_r, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_r, "D", "Similarity")

# Tests PCA 1 vs PCA 3

# Grote Karakiet

ecospat.niche.overlap(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet2, grid.clim.rietzanger, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet2, grid.clim.snor, cor=TRUE)

eq.test_gr_k2<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet2, rep=100, alternative = "lower")
sim.test_gr_k2<-ecospat.niche.similarity.test(grid.clim.grotekarakiet2, grid.clim.kleinekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_k2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k2, "D", "Similarity")

eq.test_gr_r2<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet2, grid.clim.rietzanger2, rep=100, alternative = "lower")
sim.test_gr_r2<-ecospat.niche.similarity.test(grid.clim.grotekarakiet2, grid.clim.rietzanger2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_r2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_r2, "D", "Similarity")

eq.test_gr_s2<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet2, grid.clim.snor2, rep=100, alternative = "lower")
sim.test_gr_s2<-ecospat.niche.similarity.test(grid.clim.grotekarakiet2, grid.clim.snor2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_s2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s2, "D", "Similarity")

# Kleine Karakiet
ecospat.niche.overlap(grid.clim.kleinekarakiet2, grid.clim.grotekarakiet2, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet2, grid.clim.rietzanger2, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet2, grid.clim.snor2, cor=TRUE)

eq.test_k_gr2<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet2, grid.clim.grotekarakiet2, rep=100, alternative = "lower")
sim.test_k_gr2<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet2, grid.clim.grotekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_gr2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_gr2, "D", "Similarity")

eq.test_k_r2<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet2, grid.clim.rietzanger2, rep=100, alternative = "lower")
sim.test_k_r2<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet2, grid.clim.rietzanger2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_r2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_r2, "D", "Similarity")

eq.test_k_s2<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet2, grid.clim.snor2, rep=100, alternative = "lower")
sim.test_k_s2<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet2, grid.clim.snor2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_s2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s2, "D", "Similarity")

# Rietzanger

ecospat.niche.overlap(grid.clim.rietzanger2, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.rietzanger2, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.rietzanger2, grid.clim.snor, cor=TRUE)

eq.test_r_gr2<-ecospat.niche.equivalency.test(grid.clim.rietzanger2, grid.clim.grotekarakiet2, rep=100, alternative = "lower")
sim.test_r_gr2<-ecospat.niche.similarity.test(grid.clim.rietzanger2, grid.clim.grotekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_gr2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_gr2, "D", "Similarity")

eq.test_r_k2<-ecospat.niche.equivalency.test(grid.clim.rietzanger2, grid.clim.kleinekarakiet2, rep=100, alternative = "lower")
sim.test_r_k2<-ecospat.niche.similarity.test(grid.clim.rietzanger2, grid.clim.kleinekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_k2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_k2, "D", "Similarity")

eq.test_r_s2<-ecospat.niche.equivalency.test(grid.clim.rietzanger2, grid.clim.snor2, rep=100, alternative = "lower")
sim.test_r_s2<-ecospat.niche.similarity.test(grid.clim.rietzanger2, grid.clim.snor2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_s2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_s2, "D", "Similarity")

# Snor
ecospat.niche.overlap(grid.clim.snor, grid.clim.grotekarakiet2, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet2, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.rietzanger2, cor=TRUE)

eq.test_s_gr2<-ecospat.niche.equivalency.test(grid.clim.snor2, grid.clim.grotekarakiet2, rep=100, alternative = "lower")
sim.test_s_gr2<-ecospat.niche.similarity.test(grid.clim.snor2, grid.clim.grotekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_gr2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_gr2, "D", "Similarity")

eq.test_s_k2<-ecospat.niche.equivalency.test(grid.clim.snor2, grid.clim.kleinekarakiet2, rep=100, alternative = "lower")
sim.test_s_k2<-ecospat.niche.similarity.test(grid.clim.snor2, grid.clim.kleinekarakiet2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_k2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_k2, "D", "Similarity")

eq.test_s_r2<-ecospat.niche.equivalency.test(grid.clim.snor2, grid.clim.rietzanger2, rep=100, alternative = "lower")
sim.test_s_r2<-ecospat.niche.similarity.test(grid.clim.snor2, grid.clim.rietzanger2, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_r2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_r2, "D", "Similarity")

# Visualize overlap - not overlap PCA 1 vs 2

ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=TRUE)
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0, interest=2,title= "Niche Overlap Great reed warbler vs Savi's warbler", name.axis1="PC1",name.axis2="PC2")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.rietzanger, quant=0, interest=2,title= "Niche Overlap Great reed warbler vs Sedge warbler", name.axis1="PC1",name.axis2="PC2")

ecospat.plot.niche.dyn(grid.clim.kleinekarakiet, grid.clim.snor, quant=0, interest=2,title= "Niche Overlap Reed warbler vs Savi's warbler", name.axis1="PC1",name.axis2="PC2")

ecospat.plot.niche.dyn(grid.clim.rietzanger, grid.clim.snor, quant=0, interest=2,title= "Niche Overlap Sedge warbler vs Savi's warbler", name.axis1="PC1",name.axis2="PC2")
