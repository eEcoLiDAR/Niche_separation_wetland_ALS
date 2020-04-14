library(dplyr)
library(stringr)
library(tidyr)

library(FactoMineR)
library(factoextra)
library(corrplot)

library(ecospat)

library(ggplot2)
library(gridExtra)
library(GGally)

library(egg)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v7/"
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

#data_merged=rbind(GrW_lgn8,KK_lgn8,Sn_lgn8,Bgr_lgn8)
data_merged=rbind(GrW,KK,Sn,Bgr)

noffea=10

# 100 m with bushveg
data_merged=subset(data_merged,select=c(11,10,9,7,8,14,20,18,25,29,12,16,22,23,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "HV_bushveg_sd", "HV_bushveg_prop",
                        "species","occurrence","lgn8","x","y","id")
# 100 m only reed
data_merged=subset(data_merged,select=c(11,10,9,7,8,14,20,18,25,29,22,23,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "species","occurrence","lgn8","x","y","id")

# 200 m with bushveg
data_merged=subset(data_merged,select=c(11,10,9,7,8,15,21,19,33,37,13,17,22,23,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "HV_bushveg_sd", "HV_bushveg_prop",
                        "species","occurrence","lgn8","x","y","id")
# 200 m only reed
data_merged=subset(data_merged,select=c(11,10,9,7,8,15,21,19,33,37,22,23,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HV_sd","HV_reedveg_sd", "HV_reedveg_prop","HV_reedveg_patch","HV_reedveg_edge",
                        "species","occurrence","lgn8","x","y","id")

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
                   palette=c("grey","goldenrod4"),
                   addEllipses = TRUE, ellipse.type = "convex",
                   col.var = "black",
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
                   palette=c("grey","green3"),
                   addEllipses = TRUE, ellipse.type = "convex",
                   col.var = "black",
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
                   palette=c("grey","deeppink"),
                   addEllipses = TRUE, ellipse.type = "convex",
                   col.var = "black",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Snor" | data_merged[,noffea+1]=="Background")))
                   #select.ind = list(name = c(which(data_merged[,noffea+1]=="Snor")))
                   )

fig2=grid.arrange(
  p1+labs(title ="a) PCA variable contribution")+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  p2+labs(title ="b) Great reed warbler")+xlim(-8,5)+ylim(-5,5)+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  p3+labs(title ="c) Reed warbler")+xlim(-8,5)+ylim(-5,5)+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  p4+labs(title ="d) Savi's warbler")+xlim(-8,5)+ylim(-5,5)+theme(axis.text=element_text(size=14),axis.title=element_text(size=14)),
  ncol=2,
  nrow=2
)

ggsave("Fig2_pca_a_200mreed_wlgn8.png",plot = fig2,width = 15, height = 12)

# loadings

var <- get_pca_var(pca.env)
corrplot(as.matrix(var$cor), is.corr=FALSE,method="number",col=colorRampPalette(c("dodgerblue4","white","firebrick"))(200))

# specialization (convex hull area)
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))
bgr=dplyr::filter(data_merged,str_detect(data_merged$species,"Background"))

scores.sp.grotekarakiet<-suprow(pca.env,grotekarakiet[,1:noffea])$li
scores.sp.kleinekarakiet<-suprow(pca.env,kleinekarakiet[,1:noffea])$li
scores.sp.snor<-suprow(pca.env,snor[,1:noffea])$li
scores.clim.background<-suprow(pca.env,bgr[,1:noffea])$li

convhull_GrW=chull(scores.sp.grotekarakiet[,c(1,2)])
poly_grw=scores.sp.grotekarakiet[c(convhull_GrW,convhull_GrW[1]),c(1,2)]
poly_grw_2 <- SpatialPolygons(list(Polygons(list(Polygon(poly_grw)), ID=1)))

convhull_kk=chull(scores.sp.kleinekarakiet[,c(1,2)])
poly_kk=scores.sp.kleinekarakiet[c(convhull_kk,convhull_kk[1]),c(1,2)]
poly_kk_2 <- SpatialPolygons(list(Polygons(list(Polygon(poly_kk)), ID=1)))

convhull_sn=chull(scores.sp.snor[,c(1,2)])
poly_sn=scores.sp.snor[c(convhull_sn,convhull_sn[1]),c(1,2)]
poly_sn_2 <- SpatialPolygons(list(Polygons(list(Polygon(poly_sn)), ID=1)))

convhull_bgr=chull(scores.clim.background[,c(1,2)])
poly_bgr=scores.clim.background[c(convhull_bgr,convhull_bgr[1]),c(1,2)]
poly_bgr_2 <- SpatialPolygons(list(Polygons(list(Polygon(poly_bgr)), ID=1)))



