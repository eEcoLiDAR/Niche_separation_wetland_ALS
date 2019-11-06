library(dplyr)
library(stringr)

library(FactoMineR)
library(factoextra)

library(ggplot2)
library(gridExtra)

workingdirectory="D:/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
setwd(workingdirectory)

data=read.csv("veg_metrics_10m.csv")
dataabs=read.csv("veg_metrics_10m_abs.csv")

data_sub=subset(data,select=c(39,40,5:17,18,35))
dataabs_sub=subset(dataabs,select=c(39,40,5:17,18,32))

data_merged=rbind(data_sub,dataabs_sub)

data_merged_env_baardman=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Baardman"))
data_merged_pres_baardman=dplyr::filter(data_merged_env_baardman,occrrnc==1)
baardman=subset(data_merged_pres_baardman,select=c(16,1,2))
names(baardman)<- c("sp","x","y")

baardman_env=subset(data_merged_env_baardman,select=c(1,2,3:15))
colnames(baardman_env)[1] <- "x"
colnames(baardman_env)[2] <- "y"

data_merged_abs_baardman=dplyr::filter(data_merged_env_baardman,occrrnc==0)

data_merged_env_snor=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Snor"))
data_merged_pres_snor=dplyr::filter(data_merged_env_snor,occrrnc==1)
snor=subset(data_merged_pres_snor,select=c(16,1,2))
names(snor)<- c("sp","x","y")

snor_env=subset(data_merged_env_snor,select=c(1,2,3:15))
colnames(snor_env)[1] <- "x"
colnames(snor_env)[2] <- "y"

data_merged_abs_snor=dplyr::filter(data_merged_env_snor,occrrnc==0)

#PCA based

data.pca <- PCA(data_merged[,3:15], graph = TRUE)

pca <- prcomp(data_merged[,3:15], retx=TRUE, center=TRUE, scale=TRUE)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100) 

# niche

#baardman
dataabs_sub_pca <- data.frame(predict(pca, newdata=dataabs_sub[,3:15]))
data_merged_abs_baardman_pca <- data.frame(predict(pca, newdata=data_merged_abs_baardman[,3:15]))
data_merged_pres_baardman_pca <- data.frame(predict(pca, newdata=data_merged_pres_baardman[,3:15]))


#snor
dataabs_sub_pca <- data.frame(predict(pca, newdata=dataabs_sub[,3:15]))
data_merged_abs_snor_pca <- data.frame(predict(pca, newdata=data_merged_abs_snor[,3:15]))
data_merged_pres_snor_pca <- data.frame(predict(pca, newdata=data_merged_pres_snor[,3:15]))

ggplot(data_merged_pres_snor_pca, aes(x=PC1, y=PC2)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(data_merged_pres_baardman_pca, aes(x=PC1, y=PC2)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(data_merged_pres_snor, aes(x=X_perc_25_normalized_height, y=X_pulse_penetration_ratio)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(data_merged_pres_baardman, aes(x=X_perc_25_normalized_height, y=X_pulse_penetration_ratio)) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

data_merged_pres_snor_pca$species<-1
data_merged_pres_baardman_pca$species<-2

df=rbind(data_merged_pres_snor_pca,data_merged_pres_baardman_pca)

p1=ggplot(df,aes(PC1, PC2, color=factor(species))) + 
  geom_point()

p2=ggplot(df,aes(PC1,fill=factor(species))) + 
  geom_density(alpha=.5) 

p3=ggplot(df,aes(PC2,fill=factor(species))) + 
  geom_density(alpha=.5) 

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

grid.arrange(p2, blankPlot, p1, p3, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))


