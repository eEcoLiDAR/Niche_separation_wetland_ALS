library(dplyr)
library(stringr)

library(FactoMineR)
library(factoextra)

library(ggplot2)
library(gridExtra)
library(GGally)

library(usdm)

#workingdirectory="D:/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
workingdirectory="C:/Koma/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
setwd(workingdirectory)

data=read.csv("veg_metrics_10m.csv")
dataabs=read.csv("veg_metrics_10m_abs.csv")

data_sub=subset(data,select=c(42,5:23,24,41,45,46))
dataabs_sub=subset(dataabs,select=c(42,5:23,24,38,45,46))

data_merged=rbind(data_sub,dataabs_sub)

baardman=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Baardman"))
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Kleine Karekiet"))
rietzanger=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Rietzanger"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Snor"))

# VIF
vif_pres=vifcor(data[5:23], th=0.6) # method='spearman'
data_noncor=exclude(data,vif_pres)
data_cor=subset(data,select=c("perc_95_normalized_height","entropy_norm_z","std_norm_z","perc_25_normalized_height"))

baardman_noncorr=exclude(baardman,vif_pres)
grotekarakiet_noncorr=exclude(grotekarakiet,vif_pres)
kleinekarakiet_noncorr=exclude(kleinekarakiet,vif_pres)
rietzanger_noncorr=exclude(rietzanger,vif_pres)
snor_noncorr=exclude(snor,vif_pres)

selfea=c("veg_dens_1_2m","veg_dens_2_3m","veg_dens_above_mean","dsm_med_50m","rough_10m","dsm_sd_50m","lowveg_count_50m","25th_perc","pulsepen","std")

#PCA
data.pca <- PCA(data[5:23], graph = FALSE)

fviz_pca_biplot(data.pca, 
                col.ind = data$species.y, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 


data_noncor.pca <- PCA(data_noncor, graph = FALSE)

fviz_pca_biplot(data_noncor.pca, 
                col.ind = data$species.y, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

baardman.pca <- PCA(baardman_noncorr, graph = FALSE)

fviz_pca_biplot(baardman.pca, 
                col.ind = factor(baardman$occrrnc), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

grotekarakiet.pca <- PCA(grotekarakiet_noncorr, graph = FALSE)

fviz_pca_biplot(grotekarakiet.pca, 
                col.ind = factor(grotekarakiet$occrrnc), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

kleinekarakiet.pca <- PCA(kleinekarakiet_noncorr, graph = FALSE)

fviz_pca_biplot(kleinekarakiet.pca, 
                col.ind = factor(kleinekarakiet$occrrnc), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

rietzanger.pca <- PCA(rietzanger_noncorr, graph = FALSE)

fviz_pca_biplot(rietzanger.pca, 
                col.ind = factor(rietzanger$occrrnc), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

snor.pca <- PCA(snor_noncorr, graph = FALSE)

fviz_pca_biplot(snor.pca, 
                col.ind = factor(snor$occrrnc), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

# Pairsplot
names(data_noncor) <- selfea
data_noncor=cbind(data_noncor,data$species.y)
colnames(data_noncor)[11] <- "species"

data_noncor$species=str_replace(data_noncor$species,"Baardman","B")
data_noncor$species=str_replace(data_noncor$species,"Grote Karekiet","GK")
data_noncor$species=str_replace(data_noncor$species,"Kleine Karekiet","KK")
data_noncor$species=str_replace(data_noncor$species,"Rietzanger","R")
data_noncor$species=str_replace(data_noncor$species,"Snor","S")

ggpairs(data_noncor, aes(colour =species, alpha = 0.4))

ggpairs(data_noncor,
             columns=1:8, 
             upper = list(continuous = "density"),
             title="Noncorrelated lidar metrics",
        aes(colour =species, alpha = 0.4))

names(data_cor) <- c("veg_height","fhd","std_h","p25")
data_cor=cbind(data_cor,data$species.y)
colnames(data_cor)[5] <- "species"

data_cor$species=str_replace(data_cor$species,"Baardman","B")
data_cor$species=str_replace(data_cor$species,"Grote Karekiet","GK")
data_cor$species=str_replace(data_cor$species,"Kleine Karekiet","KK")
data_cor$species=str_replace(data_cor$species,"Rietzanger","R")
data_cor$species=str_replace(data_cor$species,"Snor","S")

ggpairs(data_cor, aes(colour =species, alpha = 0.4))

names(baardman_noncorr) <- selfea
baardman_noncorr=cbind(baardman_noncorr,baardman$occrrnc)
colnames(baardman_noncorr)[11] <- "occurrance"
baardman_noncorr$occurrance=as.factor(baardman_noncorr$occurrance)

ggpairs(baardman_noncorr, aes(colour =occurrance, alpha = 0.4))

names(grotekarakiet_noncorr) <- selfea
grotekarakiet_noncorr=cbind(grotekarakiet_noncorr,grotekarakiet$occrrnc)
colnames(grotekarakiet_noncorr)[8] <- "occurrance"
grotekarakiet_noncorr$occurrance=as.factor(grotekarakiet_noncorr$occurrance)

ggpairs(grotekarakiet_noncorr, aes(colour =occurrance, alpha = 0.4))

names(kleinekarakiet_noncorr) <- selfea
kleinekarakiet_noncorr=cbind(kleinekarakiet_noncorr,kleinekarakiet$occrrnc)
colnames(kleinekarakiet_noncorr)[8] <- "occurrance"
kleinekarakiet_noncorr$occurrance=as.factor(kleinekarakiet_noncorr$occurrance)

ggpairs(kleinekarakiet_noncorr, aes(colour =occurrance, alpha = 0.4))

names(rietzanger_noncorr) <- selfea
rietzanger_noncorr=cbind(rietzanger_noncorr,rietzanger$occrrnc)
colnames(rietzanger_noncorr)[8] <- "occurrance"
rietzanger_noncorr$occurrance=as.factor(rietzanger_noncorr$occurrance)

ggpairs(rietzanger_noncorr, aes(colour =occurrance, alpha = 0.4))

names(snor_noncorr) <- selfea
snor_noncorr=cbind(snor_noncorr,snor$occrrnc)
colnames(snor_noncorr)[8] <- "occurrance"
snor_noncorr$occurrance=as.factor(snor_noncorr$occurrance)

ggpairs(snor_noncorr, aes(colour =occurrance, alpha = 0.4))
