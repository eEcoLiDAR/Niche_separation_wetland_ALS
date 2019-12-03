library(dplyr)
library(stringr)
library(tidyr)

library(FactoMineR)
library(factoextra)

library(ggplot2)
library(gridExtra)
library(GGally)

library(usdm)
library(agricolae)

workingdirectory="D:/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
#workingdirectory="C:/Koma/Sync/_Amsterdam/03_Paper2_bird_lidar_sdm/Analysis2019Nov/"
setwd(workingdirectory)

data=read.csv("veg_metrics_50m.csv")

# ggpairs good ones: 21, 24, 30, 6, 8, 19, 20
data_sub=subset(data,select=c(5:20,33))
ggpairs(data_sub, aes(colour =species.x, alpha = 0.4))

data_sub2=subset(data,select=c(21:32,33))
ggpairs(data_sub2, aes(colour =species.x, alpha = 0.4))

# PCA
data.pca <- PCA(data[5:32], graph = FALSE)

fviz_pca_biplot(data.pca, 
                col.ind = data$species.x, palette = "jco", 
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

#  data sel 
data_sel=subset(data,select=c(21,24,30,6,8,19,20,33))

data_test=gather(data_sel,-species.x,key = "var", value = "value")

data_test_onlyone=data_test[data_test$var=="lowveg_med_20m",]

# Tukey test
data.lm <- lm(data_test_onlyone$value ~ data_test_onlyone$species.x, data = data_test_onlyone)

data.av <- aov(data.lm)
summary(data.av)

data.test <- TukeyHSD(data.av)
data.test

plot(data.test)

# plot boxplots

data_sel1=subset(data,select=c(5:14,33))
data_sel2=subset(data,select=c(15:24,33))
data_sel3=subset(data,select=c(25:32,33))

data_sel %>%
  gather(-species.x,key = "var", value = "value") %>%
  ggplot(aes(x = species.x, y = value, fill=species.x)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()

data_sel1 %>%
  gather(-species.x,key = "var", value = "value") %>%
  ggplot(aes(x = species.x, y = value, fill=species.x)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()

data_sel2 %>%
  gather(-species.x,key = "var", value = "value") %>%
  ggplot(aes(x = species.x, y = value, fill=species.x)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()

data_sel3 %>%
  gather(-species.x,key = "var", value = "value") %>%
  ggplot(aes(x = species.x, y = value, fill=species.x)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()

# Histogram
data_sel1 %>%
  gather(-species.x,key = "var", value = "value") %>%
  ggplot(aes(x = value, fill=species.x)) +
  geom_density(alpha=0.6)+
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()

data_sel2 %>%
  gather(-species.x,key = "var", value = "value") %>%
  ggplot(aes(x = value, fill=species.x)) +
  geom_density(alpha=0.6) +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()

data_sel3 %>%
  gather(-species.x,key = "var", value = "value") %>%
  ggplot(aes(x = value, fill=species.x)) +
  geom_density(alpha=0.6) +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()
