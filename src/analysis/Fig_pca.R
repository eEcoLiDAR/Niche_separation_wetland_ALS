library(dplyr)
library(stringr)
library(tidyr)

library(FactoMineR)
library(factoextra)

library(ecospat)

library(ggplot2)
library(gridExtra)
library(GGally)

library(egg)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v6/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,332,41,42,43))

KK=read.csv("KK_wlandsc.csv")
KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,332,41,42,43))

Sn=read.csv("Sn_wlandsc.csv")
Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,332,41,42,43))

Bgr=read.csv("Bgr_wlandsc.csv")
Bgr_lgn8 <- subset(Bgr, lgn8 %in% c(16,17,30,322,332,41,42,43))

#data_merged=rbind(GrW,KK,Sn)
data_merged=rbind(GrW_lgn8,KK_lgn8,Sn_lgn8,Bgr_lgn8)
data_merged=rbind(GrW,KK,Sn,Bgr)

data_presabs_stat <- data_merged %>%
  group_by(species,occurrence) %>%
  summarise(nofobs = length(occurrence))

data_merged=subset(data_merged,select=c(11,10,9,7,8,14,20,18,25,29,12,16,22,23,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "HV_bushveg_sd", "HV_bushveg_prop",
                        "species","occurrence","lgn8","x","y","id")

data_merged=subset(data_merged,select=c(11,10,9,7,8,14,20,18,25,29,22,23,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "species","occurrence","lgn8","x","y","id")

data_merged=subset(data_merged,select=c(11,10,9,7,8,15,21,19,33,37,13,17,22,23,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "HV_bushveg_sd", "HV_bushveg_prop",
                        "species","occurrence","lgn8","x","y","id")

noffea=10

data_merged=data_merged[(data_merged$VV_p95<30),]
data_merged[is.na(data_merged)==TRUE] <- 0

# PCA -- Fig.2. a PCA 1,2

pca.env<-dudi.pca(data_merged[,1:noffea],scannf=FALSE,center=TRUE,nf=3)

p1=fviz_pca_var(pca.env,axes = c(1, 2), col.var = "contrib",repel = TRUE)+scale_color_gradient2(low="white", mid="blue",high="red", midpoint=7.5)

p2=fviz_pca_biplot(pca.env, axes=c(1,2), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","goldenrod4"),
                   addEllipses = TRUE, ellipse.type = "convex",
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Grote Karekiet" | data_merged[,noffea+1]=="Background")))
                   #select.ind = list(name = c(which(data_merged[,noffea+1]=="Grote Karekiet")))
                   )

p3=fviz_pca_biplot(pca.env, axes=c(1,2), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","green3"),
                   addEllipses = TRUE, ellipse.type = "convex",
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Kleine Karekiet" | data_merged[,noffea+1]=="Background")))
                   #select.ind = list(name = c(which(data_merged[,noffea+1]=="Kleine Karekiet")))
                   )

p4=fviz_pca_biplot(pca.env, axes=c(1,2), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","deeppink"),
                   addEllipses = TRUE, ellipse.type = "convex",
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Snor" | data_merged[,noffea+1]=="Background")))
                   #select.ind = list(name = c(which(data_merged[,noffea+1]=="Snor")))
                   )

fig2=grid.arrange(
  p1+labs(title ="a) PCA variable contribution", x = "PC1 (41.2%)", y = "PC2 (19.7%)")+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  p2+labs(title ="b) Great reed warbler", x = "PC1 (41.2%)", y = "PC2 (19.7%)")+xlim(-8,5)+ylim(-5,5)+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  p3+labs(title ="c) Reed warbler", x = "PC1 (41.2%)", y = "PC2 (19.7%)")+xlim(-8,5)+ylim(-5,5)+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  p4+labs(title ="d) Savi's warbler", x = "PC1 (41.2%)", y = "PC2 (19.7%)")+xlim(-8,5)+ylim(-5,5)+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  ncol=2,
  nrow=2
)

ggsave("Fig2_pca_a.png",plot = fig2,width = 15, height = 12)



