library(dplyr)
library(stringr)

library(FactoMineR)
library(factoextra)

library(ggplot2)
library(gridExtra)
library(GGally)

library(usdm)

workingdirectory="D:/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
#workingdirectory="C:/Koma/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
setwd(workingdirectory)

BReed=read.csv("veg_metrics_10m_BReed.csv")
GreedW=read.csv("veg_metrics_10m_GreedW.csv")
ReedW=read.csv("veg_metrics_10m_ReedW.csv")
SaviW=read.csv("veg_metrics_10m_SaviW.csv")

data_merged=rbind(BReed,GreedW,ReedW,SaviW)

# VIF
vif_pres=vifcor(data_merged[5:23], th=0.6, method='spearman') 
data_noncor=exclude(data_merged,vif_pres)

# PCA

data.pca <- PCA(data_merged[5:23], graph = FALSE)

fviz_pca_biplot(data.pca, 
                col.ind = data_merged$species.y, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

data_noncor.pca <- PCA(data_noncor, graph = FALSE)

fviz_pca_biplot(data_noncor.pca, 
                col.ind = data_merged$species.y, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

# Pairplot

selfea=c("veg_dens_1_2m","veg_dens_2_3m","veg_dens_ab3m","veg_dens_above_mean","dsm_med_50m","rough_10m","lowveg_count_50m","pulsepen")

names(data_noncor) <- selfea
data_noncor=cbind(data_noncor,data_merged$species.y)
colnames(data_noncor)[9] <- "species"

data_noncor$species=str_replace(data_noncor$species,"Baardman","B")
data_noncor$species=str_replace(data_noncor$species,"Grote Karekiet","GK")
data_noncor$species=str_replace(data_noncor$species,"Kleine Karekiet","KK")
data_noncor$species=str_replace(data_noncor$species,"Snor","S")

ggpairs(data_noncor, aes(colour =species, alpha = 0.4))

# heatplot

ggplot(BReed, aes(x=lowveg_count_50m, y=band_ratio_2_normalized_height_3)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(GreedW, aes(x=lowveg_count_50m, y=band_ratio_2_normalized_height_3)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(ReedW, aes(x=lowveg_count_50m, y=band_ratio_2_normalized_height_3)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(SaviW, aes(x=lowveg_count_50m, y=band_ratio_2_normalized_height_3)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()