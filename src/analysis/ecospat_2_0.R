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

data_sub=subset(data,select=c(5:32,33,50))
dataabs_sub=subset(dataabs,select=c(5:32,33,47))

data_merged=rbind(data_sub,dataabs_sub)

# PCA
par(mfrow=c(1,1))

res.pca=PCA(data_merged[,1:16], scale.unit = TRUE, ncp = 5, graph = FALSE)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             )

corrplot(var$contrib, is.corr=FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)

fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

# ecospat run 1

baardman=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Baardman"))
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Kleine Karekiet"))
rietzanger=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Rietzanger"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Snor"))

pca.env<-dudi.pca(data_merged[,1:28],scannf=FALSE,nf=2)
par(mfrow=c(1,1))
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim<-pca.env$li

scores.sp.baardman<-suprow(pca.env,baardman[which(baardman[,30]==1),1:28])$li
scores.clim.baardman<-suprow(pca.env,baardman[,1:28])$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,30]==1),1:28])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:28])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,30]==1),1:28])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:28])$li

scores.sp.rietzanger<-suprow(pca.env,rietzanger[which(rietzanger[,30]==1),1:28])$li
scores.clim.rietzanger<-suprow(pca.env,rietzanger[,1:28])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,30]==1),1:28])$li
scores.clim.snor<-suprow(pca.env,snor[,1:28])$li

grid.clim.baardman<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.baardman, sp=scores.sp.baardman, R=250, th.sp=0.05,th.env=0.05)
grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.grotekarakiet, sp=scores.sp.grotekarakiet, R=250, th.sp=0.05,th.env=0.05) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.kleinekarakiet, sp=scores.sp.kleinekarakiet, R=250, th.sp=0.05,th.env=0.05) 
grid.clim.rietzanger<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.rietzanger, sp=scores.sp.rietzanger, R=250, th.sp=0.05,th.env=0.05) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.snor, sp=scores.sp.snor, R=250, th.sp=0.05,th.env=0.05) 

par(mfrow=c(2,3))
ecospat.plot.niche(grid.clim.baardman)
ecospat.plot.niche(grid.clim.grotekarakiet)
ecospat.plot.niche(grid.clim.kleinekarakiet)
ecospat.plot.niche(grid.clim.rietzanger)
ecospat.plot.niche(grid.clim.snor)
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)


# ecospat run 2

data_merged=subset(data_merged,select=c(1,2,3,4,8,12,13,14,15,17,21,22,25,26,27,28,29,30))

baardman=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Baardman"))
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Kleine Karekiet"))
rietzanger=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Rietzanger"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Snor"))

pca.env<-dudi.pca(data_merged[,1:16],scannf=FALSE,nf=2)
par(mfrow=c(1,1))
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim<-pca.env$li

scores.sp.baardman<-suprow(pca.env,baardman[which(baardman[,18]==1),1:16])$li
scores.clim.baardman<-suprow(pca.env,baardman[,1:16])$li

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,18]==1),1:16])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:16])$li

scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[which(kleinekarakiet[,18]==1),1:16])$li
scores.clim.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:16])$li

scores.sp.rietzanger<-suprow(pca.env,rietzanger[which(rietzanger[,18]==1),1:16])$li
scores.clim.rietzanger<-suprow(pca.env,rietzanger[,1:16])$li

scores.sp.snor<-suprow(pca.env,snor[which(snor[,18]==1),1:16])$li
scores.clim.snor<-suprow(pca.env,snor[,1:16])$li

grid.clim.baardman<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.baardman, sp=scores.sp.baardman, R=250, th.sp=0.05,th.env=0.05)
grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.grotekarakiet, sp=scores.sp.grotekarakiet, R=250, th.sp=0.05,th.env=0.05) 
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.kleinekarakiet, sp=scores.sp.kleinekarakiet, R=250, th.sp=0.05,th.env=0.05) 
grid.clim.rietzanger<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.rietzanger, sp=scores.sp.rietzanger, R=250, th.sp=0.05,th.env=0.05) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.snor, sp=scores.sp.snor, R=250, th.sp=0.05,th.env=0.05) 

par(mfrow=c(2,3))
ecospat.plot.niche(grid.clim.baardman)
ecospat.plot.niche(grid.clim.grotekarakiet)
ecospat.plot.niche(grid.clim.kleinekarakiet)
ecospat.plot.niche(grid.clim.rietzanger)
ecospat.plot.niche(grid.clim.snor)
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
