library(ecospat)

library(dplyr)
library(stringr)

inv <-ecospat.testNiche.inv
nat <-ecospat.testNiche.nat

pca.env<-dudi.pca(rbind(nat,inv)[,3:10],scannf=FALSE,nf=2)
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim<-pca.env$li
scores.sp.inv<-suprow(pca.env,inv[which(inv[,11]==1),3:10])$li
scores.clim.nat<-suprow(pca.env,nat[,3:10])$li
scores.clim.inv<-suprow(pca.env,inv[,3:10])$li
scores.sp.nat<-suprow(pca.env,nat[which(nat[,11]==1),3:10])$li

grid.clim.nat<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.nat, sp=scores.sp.nat, R=100, th.sp=0)
grid.clim.inv<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.inv, sp=scores.sp.inv, R=100, th.sp=0) 

ecospat.niche.overlap (grid.clim.nat, grid.clim.inv, cor=TRUE)

eq.test<-ecospat.niche.equivalency.test(grid.clim.nat, grid.clim.inv, rep=100, alternative = "greater")
sim.test<-ecospat.niche.similarity.test(grid.clim.nat, grid.clim.inv, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

niche.dyn <-ecospat.niche.dyn.index(grid.clim.nat, grid.clim.inv, intersection = 0.1)

ecospat.plot.niche.dyn(grid.clim.nat, grid.clim.inv, quant=0.25, interest=2,title= "Niche Overlap", name.axis1="PC1",name.axis2="PC2")
ecospat.shift.centroids(scores.sp.nat, scores.sp.inv, scores.clim.nat, scores.clim.inv)

############## With my data ##############
workingdirectory="D:/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
setwd(workingdirectory)

data=read.csv("veg_metrics_50m.csv")
dataabs=read.csv("veg_metrics_50m_abs.csv")

data_sub=subset(data,select=c(5:32,33,50))
dataabs_sub=subset(dataabs,select=c(5:32,33,47))

data_merged=rbind(data_sub,dataabs_sub)

baardman=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Baardman"))
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Grote Karekiet"))

data_merged2=rbind(baardman,grotekarakiet)

pca.env<-dudi.pca(data_merged2[,1:28],scannf=FALSE,nf=2)
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)

scores.globclim<-pca.env$li
scores.sp.baardman<-suprow(pca.env,baardman[which(baardman[,30]==1),1:28])$li
scores.clim.baardman<-suprow(pca.env,baardman[,1:28])$li
scores.clim.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:28])$li
scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[which(grotekarakiet[,30]==1),1:28])$li

grid.clim.baardman<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.baardman, sp=scores.sp.baardman, R=150, th.sp=0.2)
grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.grotekarakiet, sp=scores.sp.grotekarakiet, R=150, th.sp=0.2) 

ecospat.niche.overlap(grid.clim.baardman, grid.clim.grotekarakiet, cor=TRUE)

eq.test<-ecospat.niche.equivalency.test(grid.clim.baardman, grid.clim.grotekarakiet, rep=100, alternative = "greater")
sim.test<-ecospat.niche.similarity.test(grid.clim.baardman, grid.clim.grotekarakiet, rep=100, alternative = "greater", rand.type = 2)

ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")

ecospat.plot.niche.dyn(grid.clim.baardman, grid.clim.grotekarakiet, quant=0, interest=2,title= "Niche Overlap", name.axis1="PC1",name.axis2="PC2")
ecospat.plot.niche(grid.clim.baardman)
ecospat.plot.niche(grid.clim.grotekarakiet)

