library(ecospat)
library(FactoMineR)
library(factoextra)
library(corrplot)

library(dplyr)
library(stringr)

library(flextable)
library(magrittr)

workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/2_Dataset/processed/20200217/"
setwd(workingdirectory)

#### Import data

data=read.csv("veg_metrics_100m_pres.csv")
dataabs=read.csv("veg_metrics_100m_abs.csv")

data_sub=subset(data,select=c(5:16,20,34,38,39))
dataabs_sub=subset(dataabs,select=c(5:16,29,31,38,39))

data_merged=rbind(data_sub,dataabs_sub)
names(data_merged) <- c("veg_dens_1_2","veg_dens_2_3","veg_dens_0_1","FHD","veg_height25","veg_height95","dsm_sd",
                        "lowveg_sd", "lowveg_prop", "trees_prop",
                        "veg_cover","veg_var","species.x","occ","x","y")

data_merged=data_merged[data_merged$species.x!="Baardman",]

#### Filters per species

grotekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Grote Karekiet"))
kleinekarakiet=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Kleine Karekiet"))
rietzanger=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Rietzanger"))
snor=dplyr::filter(data_merged,str_detect(data_merged$species.x,"Snor"))

#### PCA anal

# Full

par(mfrow=c(1,1))

pca.env<-dudi.pca(data_merged[,1:12],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.env, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

fviz_pca_var(pca.env, axes = c(1, 3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_pca_var(pca.env, axes = c(2, 3), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)



fviz_pca_biplot(pca.env, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(data_merged$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                
                legend.title = list(fill = "Species", color = "Contrib"))

# Per species

pca.sp1<-dudi.pca(grotekarakiet[,1:12],scannf=FALSE,center=TRUE,nf=3)
pca.sp2<-dudi.pca(kleinekarakiet[,1:12],scannf=FALSE,center=TRUE,nf=3)
pca.sp3<-dudi.pca(rietzanger[,1:12],scannf=FALSE,center=TRUE,nf=3)
pca.sp4<-dudi.pca(snor[,1:12],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_biplot(pca.sp1, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(grotekarakiet$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp2, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(kleinekarakiet$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp3, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(rietzanger$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                
                legend.title = list(fill = "Species", color = "Contrib"))

fviz_pca_biplot(pca.sp4, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(snor$occ), col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = c("blue","red"),
                addEllipses = FALSE,
                # Variables
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                
                legend.title = list(fill = "Species", color = "Contrib"))

# per feature group

pca.fea_g1<-dudi.pca(data_merged[,c(4,6,11,12)],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.fea_g1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

pca.fea_g2<-dudi.pca(data_merged[,c(1,2,3,5)],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.fea_g2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

pca.fea_g3<-dudi.pca(data_merged[,c(7,8,9,10)],scannf=FALSE,center=TRUE,nf=3)

fviz_pca_var(pca.fea_g3, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE
)

## Export
var <- get_pca_var(pca.env)
factor_loadings=var$cor
factor_loadings=round(as.matrix(factor_loadings),3)

col_palette <- c("dodgerblue4","dodgerblue2","white","firebrick2","firebrick4")

mycut <- cut(
  factor_loadings, 
  breaks = c(-1,-0.6,0,0.6,1), 
  include.lowest = TRUE, label = FALSE)

mycolors <- col_palette[mycut]

df <- data.frame(rowname = row.names(factor_loadings), stringsAsFactors = FALSE) %>%
  cbind(factor_loadings) 

flextable(df) %>%
  bg(j = colnames(factor_loadings), bg = mycolors) %>%
  align(align = "center", part = "all") %>%
  compose(i = 1, j = 1, value = as_paragraph(""), part = "header")

#### Ecospat
