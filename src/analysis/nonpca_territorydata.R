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
library(ecospat)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
#workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
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
# GrW

GrW_selforsdm=subset(GrW_lgn8,select=c(6,7,9,12,13,14,15,19))
names(GrW_selforsdm) <- c("veg_dens_1_2","veg_dens_2_3","FHD","dsm_sd_100m",
                        "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","occurrence")

d_GrW <- sdmData(occurrence~.,train=GrW_selforsdm)

model_GrW <- sdm(occurrence~.,data=d_GrW ,methods=c('rf'),replication='boot',test.percent=30,n=100)
model_GrW 

vi <- getVarImp(model_GrW, method='rf')
plot(vi)

rcurve(model_GrW , id=1:100)

#Sn

Sn_selforsdm=subset(Sn_lgn8,select=c(6,7,9,12,13,14,15,19))
names(Sn_selforsdm) <- c("veg_dens_1_2","veg_dens_2_3","FHD","dsm_sd_100m",
                          "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","occurrence")

d_Sn <- sdmData(occurrence~.,train=Sn_selforsdm)

model_Sn <- sdm(occurrence~.,data=d_Sn ,methods=c('rf'),replication='boot',test.percent=30,n=100)
model_Sn

vi <- getVarImp(model_Sn, method='rf')
plot(vi)

rcurve(model_Sn , id=1:100)

#KK

KK_selforsdm=subset(KK_lgn8,select=c(6,7,9,12,13,14,15,19))
names(KK_selforsdm) <- c("veg_dens_1_2","veg_dens_2_3","FHD","dsm_sd_100m",
                         "lowveg_sd_100m", "lowveg_prop_100m","veg_cover","occurrence")

# subsample -- too many observation - modelling takes forever ...
#KK_selforsdm_abs=KK_selforsdm[KK_selforsdm$occurrence==0,]
#KK_selforsdm_pres=KK_selforsdm[KK_selforsdm$occurrence==1,]

#KK_selforsdm_abs_sampled=KK_selforsdm_abs[sample(nrow(KK_selforsdm_abs), 500), ]
#KK_selforsdm_pres_sampled=KK_selforsdm_pres[sample(nrow(KK_selforsdm_pres), 500), ]

#KK_selforsdm_sampled=rbind(KK_selforsdm_abs_sampled,KK_selforsdm_pres_sampled)

d_KK <- sdmData(occurrence~.,train=KK_selforsdm)

model_KK <- sdm(occurrence~.,data=d_KK ,methods=c('rf'),replication='boot',test.percent=30,n=25)
model_KK

vi <- getVarImp(model_KK, method='rf')
plot(vi)

rcurve(model_KK , id=1:25)

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

# Ecospat
grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species,"Kleine Karekiet"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species,"Snor"))

# FHD vs. lowveg_prop

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=data_merged[,c(4,8)], glob1=grotekarakiet[grotekarakiet$occurrence==0,c(4,8)], sp=grotekarakiet[grotekarakiet$occurrence==1,c(4,8)], R=500, th.sp=0.1,th.env=0.1)
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=data_merged[,c(4,8)], glob1=kleinekarakiet[kleinekarakiet$occurrence==0,c(4,8)], sp=kleinekarakiet[kleinekarakiet$occurrence==1,c(4,8)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=data_merged[,c(4,8)], glob1=snor[snor$occurrence==0,c(4,8)], sp=snor[snor$occurrence==1,c(4,8)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")

# overlap

ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0,
                       interest=1, title= "GrW vs Sn")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "GrW vs RW")
ecospat.plot.niche.dyn(grid.clim.snor, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "Sn vs RW")


ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=FALSE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=FALSE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=FALSE)

# FHD vs. veg dens

grid.clim.grotekarakiet<-ecospat.grid.clim.dyn(glob=data_merged[,c(4,1)], glob1=grotekarakiet[grotekarakiet$occurrence==0,c(4,1)], sp=grotekarakiet[grotekarakiet$occurrence==1,c(4,1)], R=500, th.sp=0.1,th.env=0.1)
grid.clim.kleinekarakiet<-ecospat.grid.clim.dyn(glob=data_merged[,c(4,1)], glob1=kleinekarakiet[kleinekarakiet$occurrence==0,c(4,1)], sp=kleinekarakiet[kleinekarakiet$occurrence==1,c(4,1)], R=500, th.sp=0.1,th.env=0.1) 
grid.clim.snor<-ecospat.grid.clim.dyn(glob=data_merged[,c(4,1)], glob1=snor[snor$occurrence==0,c(4,1)], sp=snor[snor$occurrence==1,c(4,1)], R=500, th.sp=0.1,th.env=0.1) 

par(mfrow=c(2,2))
ecospat.plot.niche(grid.clim.grotekarakiet,title="Great Reed Warbler")
ecospat.plot.niche(grid.clim.kleinekarakiet,title="Reed Warbler")
ecospat.plot.niche(grid.clim.snor,title="Savi's Warbler")

# overlap

ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.snor, quant=0,
                       interest=1, title= "GrW vs Sn")
ecospat.plot.niche.dyn(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "GrW vs RW")
ecospat.plot.niche.dyn(grid.clim.snor, grid.clim.kleinekarakiet, quant=0,
                       interest=1, title= "Sn vs RW")


ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.snor, cor=FALSE)
ecospat.niche.overlap(grid.clim.grotekarakiet, grid.clim.kleinekarakiet, cor=FALSE)
ecospat.niche.overlap(grid.clim.snor, grid.clim.kleinekarakiet, cor=FALSE)


