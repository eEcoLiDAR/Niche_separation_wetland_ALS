library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

workingdirectory="D:/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
setwd(workingdirectory)

data=read.csv("veg_metrics_50m.csv")
dataabs=read.csv("veg_metrics_50m_abs.csv")

data_sub=subset(data,select=c(29,32,17,30,8,5,6,26,16,25,19,33,50))
dataabs_sub=subset(dataabs,select=c(29,32,17,30,8,5,6,26,16,25,19,33,47))

data_merged=rbind(data_sub,dataabs_sub)

# PCA
par(mfrow=c(1,1))

res.pca=PCA(data_merged[,1:11], scale.unit = TRUE, ncp = 5, graph = FALSE)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE)

corrplot(var$contrib, is.corr=FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)


# ecospat run 1

#baardman=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Baardman"))
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Kleine Karekiet"))
rietzanger=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Rietzanger"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Snor"))

pca.env<-dudi.pca(data_merged[,1:11],scannf=FALSE,nf=2)
par(mfrow=c(1,1))
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim<-pca.env$li

#scores.sp.baardman<-suprow(pca.env,baardman[which(baardman[,30]==1),1:28])$li
#scores.clim.baardman<-suprow(pca.env,baardman[,1:28])$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,13]==1),1:11])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:11])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,13]==1),1:11])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:11])$li

scores.sp.rietzanger<-suprow(pca.env,rietzanger[which(rietzanger[,13]==1),1:11])$li
scores.clim.rietzanger<-suprow(pca.env,rietzanger[,1:11])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,13]==1),1:11])$li
scores.clim.snor<-suprow(pca.env,snor[,1:11])$li

#grid.clim.baardman<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.baardman, sp=scores.sp.baardman, R=250, th.sp=0.05,th.env=0.05)
grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.grotekarakiet, sp=scores.sp.grotekarakiet, R=100, th.sp=0.05,th.env=0.05) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.kleinekarakiet, sp=scores.sp.kleinekarakiet, R=100, th.sp=0.05,th.env=0.05) 
grid.clim.rietzanger<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.rietzanger, sp=scores.sp.rietzanger, R=100, th.sp=0.05,th.env=0.05) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.snor, sp=scores.sp.snor, R=100, th.sp=0.05,th.env=0.05) 

par(mfrow=c(2,2))
#ecospat.plot.niche(grid.clim.baardman,title="Bearded Readling")
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.rietzanger,title="Sedge Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

# Tests
# Grote Karakiet

ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.rietzanger, cor=TRUE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=TRUE)

eq.test_gr_k<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "greater")
sim.test_gr_k<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k, "D", "Similarity")

eq.test_gr_r<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.rietzanger, rep=100, alternative = "greater")
sim.test_gr_r<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.rietzanger, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_r, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_r, "D", "Similarity")

eq.test_gr_s<-ecospat.niche.equivalency.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "greater")
sim.test_gr_s<-ecospat.niche.similarity.test(grid.clim.grotekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_gr_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s, "D", "Similarity")

# Kleine Karakiet
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.rietzanger, cor=TRUE)
ecospat.niche.overlap(grid.clim.kleinekarakiet, grid.clim.snor, cor=TRUE)

eq.test_k_gr<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, rep=100, alternative = "greater")
sim.test_k_gr<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_gr, "D", "Similarity")

eq.test_k_r<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.rietzanger, rep=100, alternative = "greater")
sim.test_k_r<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.rietzanger, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_r, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_r, "D", "Similarity")

eq.test_k_s<-ecospat.niche.equivalency.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "greater")
sim.test_k_s<-ecospat.niche.similarity.test(grid.clim.kleinekarakiet, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_k_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s, "D", "Similarity")

# Rietzanger

ecospat.niche.overlap(grid.clim.rietzanger, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.rietzanger, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.rietzanger, grid.clim.snor, cor=TRUE)

eq.test_r_gr<-ecospat.niche.equivalency.test(grid.clim.rietzanger, grid.clim.grotekarakiet, rep=100, alternative = "greater")
sim.test_r_gr<-ecospat.niche.similarity.test(grid.clim.rietzanger, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_gr, "D", "Similarity")

eq.test_r_k<-ecospat.niche.equivalency.test(grid.clim.rietzanger, grid.clim.kleinekarakiet, rep=100, alternative = "greater")
sim.test_r_k<-ecospat.niche.similarity.test(grid.clim.rietzanger, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_k, "D", "Similarity")

eq.test_r_s<-ecospat.niche.equivalency.test(grid.clim.rietzanger, grid.clim.snor, rep=100, alternative = "greater")
sim.test_r_s<-ecospat.niche.similarity.test(grid.clim.rietzanger, grid.clim.snor, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_r_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_r_s, "D", "Similarity")

# Snor
ecospat.niche.overlap(grid.clim.snor, grid.clim.grotekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=TRUE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.rietzanger, cor=TRUE)

eq.test_s_gr<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.grotekarakiet, rep=100, alternative = "greater")
sim.test_s_gr<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_gr, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_gr, "D", "Similarity")

eq.test_s_k<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "greater")
sim.test_s_k<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.kleinekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_k, "D", "Similarity")

eq.test_s_r<-ecospat.niche.equivalency.test(grid.clim.snor, grid.clim.rietzanger, rep=100, alternative = "greater")
sim.test_s_r<-ecospat.niche.similarity.test(grid.clim.snor, grid.clim.rietzanger, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test_s_r, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_s_r, "D", "Similarity")
