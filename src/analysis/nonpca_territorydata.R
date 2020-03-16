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
library(corrplot)

library(sdm)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_territory_intersected.csv")
#GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,332,41,42,43))

KK=read.csv("KK_territory_intersected.csv")
#KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,332,41,42,43))

Sn=read.csv("Sn_territory_intersected.csv")
#Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,332,41,42,43))

#data_merged=rbind(GrW,KK,Sn)
data_merged=rbind(GrW_lgn8,KK_lgn8,Sn_lgn8)

data_presabs_stat <- data_merged %>%
  group_by(species,occurrence) %>%
  summarise(nofobs = length(occurrence))

data_presabs_stat_landcover <- data_merged %>%
  group_by(species,occurrence,lgn8) %>%
  summarise(nofobs = length(occurrence))

data_merged=subset(data_merged,select=c(6,7,8,9,11,12,13,14,15,16,18,19,5))
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height95","dsm_sd_100m",
                        "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","veg_var","species","occurrence","lgn8")

noffea=10

# boxplot per species

data_merged_mod=data_merged
levels(data_merged_mod$species) = c("Grote Karekiet","Kleine Karekiet","Snor", "None")
data_merged_mod[data_merged_mod$occurrence==0,11] <- "None"

data_sel=subset(data_merged_mod,select=c(1:noffea,11))

data_sel %>%
  gather(-species,key = "var", value = "value") %>%
  ggplot(aes(x = species, y = value, fill=species)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free") +
  ggtitle("Boxplots") +
  theme_bw()

# VIF
vif=vifcor(data_merged_mod[1:noffea], th=0.6) # method='spearman'
vif_sel=vifcor(data_merged_mod[c(1,2,3,4,6,7,8,9)], th=0.6)

# Corr
cor_env_all=cor(data_merged_mod[1:noffea], data_merged_mod[1:noffea],method = "spearman")

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
p.mat <- cor.mtest(data_merged_mod[1:noffea])

col <- colorRampPalette(c("#4477AA","#77AADD", "#FFFFFF", "#EE9988","#BB4444"))
corrplot(cor_env_all, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

# sdm fea importance
GrW_lgn8
GrW_selforsdm=subset(GrW_lgn8,select=c(6,7,9,12,13,14,15,19))

d_GrW <- sdmData(occurrence~.,train=GrW_selforsdm)

model_GrW <- sdm(occurrence~.,data=d_GrW ,methods=c('rf'),replication='boot',test.percent=30,n=10)
model_GrW 

vi <- getVarImp(model_GrW, method='rf')
vi
plot(vi)

rcurve(model_GrW , id=1:100)

# Turkey test

data_test=gather(data_sel,-species,key = "var", value = "value")
data_test_onlyone=data_test[data_test$var=="FHD",]

# Tukey test
data.lm <- lm(data_test_onlyone$value ~ data_test_onlyone$species, data = data_test_onlyone)

data.av <- aov(data.lm)
summary(data.av)

data.test <- TukeyHSD(data.av)
data.test

plot(data.test)
