library(usdm)
library(corrplot)

library(FactoMineR)
library(factoextra)

library(stringr)
library(dplyr)

library(randomForest)
library(caret)

workingdirectory="D:/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
setwd(workingdirectory)

data=read.csv("veg_metrics_10m.csv")
data$occ <- 1
dataabs=read.csv("veg_metrics_10m_abs.csv")
dataabs$occ <- 0

data_merged=rbind(data,dataabs)


# Corr environmental layers
cor_env_all=cor(data_merged[4:16], data_merged[4:16],method = "spearman")

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(data_merged[4:16])

corrplot(cor_env_all, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

# VIF
vif_merged=vifcor(data_merged[4:16], th=0.6, method='spearman')
vif_pres=vifcor(data[4:16], th=0.6, method='spearman')

data_noncor=subset(data,select=c("species","X_band_ratio_1_normalized_height_2","X_band_ratio_2_normalized_height_3",
                                 "X_band_ratio_3_normalized_height","X_pulse_penetration_ratio","X_skew_norm_z"))

data_noncor_all=subset(data_merged,select=c("species","occ","X_band_ratio_1_normalized_height_2","X_band_ratio_2_normalized_height_3",
                                 "X_band_ratio_3_normalized_height","X_pulse_penetration_ratio","X_skew_norm_z"))

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

# Per species
Baardman=dplyr::filter(data_noncor_all,str_detect(data_noncor_all$species,"Baardman"))

Baardman.pca <- PCA(Baardman[3:7], graph = FALSE)
fviz_pca_biplot(Baardman.pca, 
                col.ind = factor(Baardman$occ), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Occurance") 

GroreK=dplyr::filter(data_noncor_all,str_detect(data_noncor_all$species,"Grote Karekiet"))

GroreK.pca <- PCA(GroreK[3:7], graph = FALSE)
fviz_pca_biplot(GroreK.pca, 
                col.ind = factor(GroreK$occ), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Occurance") 

KK=dplyr::filter(data_noncor_all,str_detect(data_noncor_all$species,"Kleine Karekiet"))

KK.pca <- PCA(KK[3:7], graph = FALSE)
fviz_pca_biplot(KK.pca, 
                col.ind = factor(KK$occ), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Occurance") 

Rietzanger=dplyr::filter(data_noncor_all,str_detect(data_noncor_all$species,"Rietzanger"))

Rietzanger.pca <- PCA(Rietzanger[3:7], graph = FALSE)
fviz_pca_biplot(Rietzanger.pca, 
                col.ind = factor(Rietzanger$occ), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Occurance") 

Snor=dplyr::filter(data_noncor_all,str_detect(data_noncor_all$species,"Snor"))

Snor.pca <- PCA(Snor[3:7], graph = FALSE)
fviz_pca_biplot(Snor.pca, 
                col.ind = factor(Snor$occ), palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Occurance") 

#RF classifier
set.seed(100)
train <- sample(nrow(data_noncor), 0.7*nrow(data_noncor), replace = FALSE)
TrainSet <- data_noncor[train,]
ValidSet <- data_noncor[-train,]

model1 <- randomForest(species ~ ., data = TrainSet, importance = TRUE,ntree=100)

importance(model1)        
varImpPlot(model1)   

predValid <- predict(model1, ValidSet, type = "class")

mean(predValid == ValidSet$species)                    
table(predValid,ValidSet$species)

conf_m=confusionMatrix(factor(predValid), factor(ValidSet$species),mode = "everything")
