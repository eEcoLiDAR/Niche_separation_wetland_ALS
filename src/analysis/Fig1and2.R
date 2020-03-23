library(dplyr)
library(stringr)
library(tidyr)

library(FactoMineR)
library(factoextra)

library(ecospat)

library(ggplot2)
library(gridExtra)
library(GGally)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
#workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
#GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,332,41,42))

KK=read.csv("KK_wlandsc.csv")
#KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,332,41,42))

Sn=read.csv("Sn_wlandsc.csv")
#Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,332,41,42))

#data_merged=rbind(GrW,KK,Sn)
data_merged=rbind(GrW_lgn8,KK_lgn8,Sn_lgn8)

data_presabs_stat <- data_merged %>%
  group_by(species,occurrence) %>%
  summarise(nofobs = length(occurrence))

#data_merged=subset(data_merged,select=c(7,8,9,10,12,13,14,15,18,20,24,27,21,22,6))
#names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_100m",
                        #"lowveg_sd_100m", "lowveg_prop_100m","veg_cover","veg_var","medveg_patchiness",
                        #"medveg_edgedens","species","occurrence","lgn8")

data_merged=subset(data_merged,select=c(12,20,10,7,8,9,18,13,14,15,24,27,21,22,6))
names(data_merged) <- c("VV_p95","VV_var","VV_FHD","VD_1_2","VD_2_3","VD_0_1","VD_cov",
                        "HH_sd","HH_lowveg_sd", "HH_lowveg_prop","HH_medveg_patch","HH_medveg_edge",
                        "species","occurrence","lgn8")

noffea=12

data_merged=data_merged[(data_merged$VV_p95<30 & data_merged$VV_p95>0.1),]
data_merged[is.na(data_merged)==TRUE] <- 0

# boxplot per species  Fig.1.

data_merged_mod=data_merged
levels(data_merged_mod$species) = c("Grote Karekiet","Kleine Karekiet","Snor","Great reed warbler","Reed warbler","Savi's warbler","Absence")
data_merged_mod[data_merged_mod$species=="Grote Karekiet",noffea+1] <- "Great reed warbler"
data_merged_mod[data_merged_mod$species=="Kleine Karekiet",noffea+1] <- "Reed warbler"
data_merged_mod[data_merged_mod$species=="Snor",noffea+1] <- "Savi's warbler"
data_merged_mod[data_merged_mod$occurrence==0,noffea+1] <- "Absence"

#data_merged_mod$log_veg_height95=log(data_merged_mod[,5])
#data_merged_mod$logveg_var=log(data_merged_mod[,11])

data_sel=subset(data_merged_mod,select=c(1:noffea,noffea+1))
data_sel2=data_sel %>% gather(-species,key = "var", value = "value")
as.factor(data_sel2$var)
data_sel2$var <- factor(data_sel2$var, levels = c("VV_p95","VV_var","VV_FHD","VD_1_2","VD_2_3","VD_0_1","VD_cov",
                                                  "HH_sd","HH_lowveg_sd", "HH_lowveg_prop","HH_medveg_patch","HH_medveg_edge"))

p0=ggplot(data=data_sel2,aes(x = species, y = value, fill=species)) + geom_boxplot() + facet_wrap(~var, scales = "free") + 
  scale_fill_manual(values=c("goldenrod4","green3","deeppink","black"))+ 
  ylab("LiDAR metrics")+ theme_bw(base_size = 20)

ggsave("Fig1_boxplot.png",plot = p0,width = 35, height = 20)

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
                addEllipses = TRUE, ellipse.level = 0.9,
                col.var = "lightblue2",
                alpha.ind=1,
                legend.title = "Occurrence",
                select.ind = list(name = c(which(data_merged[,noffea+1]=="Grote Karekiet"))))

p3=fviz_pca_biplot(pca.env, axes=c(1,2), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged$occurrence),
                col.ind =  as.factor(data_merged$occurrence),
                pointshape = 21, pointsize = 1,
                palette=c("black","green3"),
                addEllipses = TRUE, ellipse.level = 0.9,
                col.var = "lightblue2",
                alpha.ind=1,
                legend.title = "Occurrence",
                select.ind = list(name = c(which(data_merged[,noffea+1]=="Kleine Karekiet"))))

p4=fviz_pca_biplot(pca.env, axes=c(1,2), 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged$occurrence),
                col.ind =  as.factor(data_merged$occurrence),
                pointshape = 21, pointsize = 1,
                palette=c("black","deeppink"),
                addEllipses = TRUE, ellipse.level = 0.9,
                col.var = "lightblue2",
                alpha.ind=1,
                legend.title = "Occurrence",
                select.ind = list(name = c(which(data_merged[,noffea+1]=="Snor"))))

fig2=grid.arrange(
  p1+labs(title ="PCA variable contribution", x = "PC1 (35%)", y = "PC2 (18.4%)"),
  p2+labs(title ="Great reed warbler", x = "PC1 (35%)", y = "PC2 (18.4%)")+xlim(-15,6)+ylim(-5,6),
  p3+labs(title ="Reed warbler", x = "PC1 (35%)", y = "PC2 (18.4%)")+xlim(-15,6)+ylim(-5,6),
  p4+labs(title ="Savi's warbler", x = "PC1 (35%)", y = "PC2 (18.4%)")+xlim(-15,6)+ylim(-5,6),
  ncol=2,
  nrow=2
)

ggsave("Fig2_pca_a.png",plot = fig2,width = 15, height = 12)

# PCA -- Fig.2. b PCA 1,3

p1=fviz_pca_var(pca.env,axes = c(1, 3), col.var = "contrib",repel = TRUE)+scale_color_gradient2(low="white", mid="blue",high="red", midpoint=7.5)

p2=fviz_pca_biplot(pca.env, axes=c(1,3), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","goldenrod4"),
                   addEllipses = TRUE, ellipse.level = 0.9,
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Grote Karekiet"))))

p3=fviz_pca_biplot(pca.env, axes=c(1,3), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","green3"),
                   addEllipses = TRUE, ellipse.level = 0.9,
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Kleine Karekiet"))))

p4=fviz_pca_biplot(pca.env, axes=c(1,3), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","deeppink"),
                   addEllipses = TRUE, ellipse.level = 0.9,
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Snor"))))

fig2=grid.arrange(
  p1+labs(title ="PCA variable contribution", x = "PC1 (35%)", y = "PC3 (12.1%)"),
  p2+labs(title ="Great reed warbler", x = "PC1 (35%)", y = "PC3 (12.1%)")+xlim(-10,6)+ylim(-4,4),
  p3+labs(title ="Reed warbler", x = "PC1 (35%)", y = "PC3 (12.1%)")+xlim(-10,6)+ylim(-4,4),
  p4+labs(title ="Savi's warbler", x = "PC1 (35%)", y = "PC3 (12.1%)")+xlim(-10,6)+ylim(-4,4),
  ncol=2,
  nrow=2
)

ggsave("Fig2_pca_b.png",plot = fig2,width = 15, height = 12)

# PCA -- Fig.2. c PCA 2,3

p1=fviz_pca_var(pca.env,axes = c(2, 3), col.var = "contrib",repel = TRUE)+scale_color_gradient2(low="white", mid="blue",high="red", midpoint=7.5)

p2=fviz_pca_biplot(pca.env, axes=c(2,3), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","goldenrod4"),
                   addEllipses = TRUE, ellipse.level = 0.9,
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Grote Karekiet"))))

p3=fviz_pca_biplot(pca.env, axes=c(2,3), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","green3"),
                   addEllipses = TRUE, ellipse.level = 0.9,
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Kleine Karekiet"))))

p4=fviz_pca_biplot(pca.env, axes=c(2,3), 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data_merged$occurrence),
                   col.ind =  as.factor(data_merged$occurrence),
                   pointshape = 21, pointsize = 1,
                   palette=c("black","deeppink"),
                   addEllipses = TRUE, ellipse.level = 0.9,
                   col.var = "lightblue2",
                   alpha.ind=1,
                   legend.title = "Occurrence",
                   select.ind = list(name = c(which(data_merged[,noffea+1]=="Snor"))))

fig2=grid.arrange(
  p1+labs(title ="PCA variable contribution", x = "PC2 (18.4%)", y = "PC3 (12.1%)"),
  p2+labs(title ="Great reed warbler", x = "PC2 (18.4%)", y = "PC3 (12.1%)")+xlim(-5,5)+ylim(-4,4),
  p3+labs(title ="Reed warbler", x = "PC2 (18.4%)", y = "PC3 (12.1%)")+xlim(-5,5)+ylim(-4,4),
  p4+labs(title ="Savi's warbler", x = "PC2 (18.4%)", y = "PC3 (12.1%)")+xlim(-5,5)+ylim(-4,4),
  ncol=2,
  nrow=2
)

ggsave("Fig2_pca_c.png",plot = fig2,width = 15, height = 12)
