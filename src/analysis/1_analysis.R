library(usdm)

library(FactoMineR)
library(factoextra)

workingdirectory="C:/Koma/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/forAnalysis/"
setwd(workingdirectory)

data=read.csv("veg_metrics_10m.csv")

# Corr environmental layers
cor_env_all=data.frame(cor(data[4:16], data[4:16],method = "spearman"))
write.csv(cor_env_all,"cor_env_all.csv")

vif=vifcor(data[4:16],th=0.6)

data_noncor=subset(data,select=c("species","X_band_ratio_1_normalized_height_2","X_band_ratio_2_normalized_height_3",
                                 "X_perc_25_normalized_height","X_pulse_penetration_ratio","X_std_norm_z"))

# PCA

data_noncor.pca <- PCA(data_noncor[2:6], graph = FALSE)

fviz_pca_biplot(data_noncor.pca, 
                col.ind = data_noncor$species, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

data.pca <- PCA(data[4:16], graph = FALSE)

fviz_pca_biplot(data.pca, 
                col.ind = data$species, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 