library(ecospat)
library(raster)

library(ggplot2)
library(gridExtra)

library(dplyr)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v11/"
setwd(workingdirectory)

grw_pca12 <- readRDS("grw_kdens.rds")
kk_pca12 <- readRDS("kk_kdens.rds")
sn_pca12 <- readRDS("sn_kdens.rds")

dens_area<- function(grw_pca12,layername) {
  
  grw_presdens=grw_pca12[[layername]]
  
  grw_presdens[grw_presdens>0] <-1
  
  full_area=as.data.frame(grw_presdens) %>%
    group_by(layer) %>%
    tally() %>%
    mutate(area = n * res(grw_presdens)[1] * res(grw_presdens)[2])
  
  print(full_area)
  
}

dens_area(grw_pca12,"z.uncor")
dens_area(kk_pca12,"z.uncor")
dens_area(sn_pca12,"z.uncor")
dens_area(sn_pca12,"Z")

# 50%

grw_pca12_50 <- readRDS("grw_kdens_r50.rds")
kk_pca12_50 <- readRDS("kk_kdens_r50.rds")
sn_pca12_50 <- readRDS("sn_kdens_r50.rds")

dens_area(grw_pca12_50,"z.uncor")
dens_area(kk_pca12_50,"z.uncor")
dens_area(sn_pca12_50,"z.uncor")
dens_area(sn_pca12_50,"Z")

