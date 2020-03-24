library(ecospat)
library(raster)

library(ggplot2)
library(gridExtra)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
#workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
setwd(workingdirectory)

grw_pca12 <- readRDS("grw_kdens.rds")
kk_pca12 <- readRDS("kk_kdens.rds")
sn_pca12 <- readRDS("sn_kdens.rds")

grw_pca12_df <- as.data.frame(grw_pca12[["z.uncor"]], xy = TRUE)
kk_pca12_df <- as.data.frame(kk_pca12[["z.uncor"]], xy = TRUE)
sn_pca12_df <- as.data.frame(sn_pca12[["z.uncor"]], xy = TRUE)

grw_pca12_df$species <- "Great reed warbler"
kk_pca12_df$species <- "Reed warbler"
sn_pca12_df$species <- "Savi's warbler"

pca12=rbind(grw_pca12_df,kk_pca12_df,sn_pca12_df)

pca12_gr_kk=rbind(grw_pca12_df,kk_pca12_df)
pca12_gr_sn=rbind(grw_pca12_df,sn_pca12_df)
pca12_kk_sn=rbind(kk_pca12_df,sn_pca12_df)

ggplot(data=pca12)+stat_contour(aes(x=x, y=y, z=layer,fill=species), geom="polygon", bins=4, alpha=.3)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 2")

p1=ggplot(data=pca12_gr_kk)+stat_contour(aes(x=x, y=y, z=layer,fill=species), geom="polygon", bins=4, alpha=.3)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 2")+scale_fill_manual(values=c("goldenrod4","green3"))

p2=ggplot(data=pca12_gr_sn)+stat_contour(aes(x=x, y=y, z=layer,fill=species), geom="polygon", bins=4, alpha=.3)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 2")+scale_fill_manual(values=c("goldenrod4","deeppink"))

p3=ggplot(data=pca12_kk_sn)+stat_contour(aes(x=x, y=y, z=layer,fill=species),geom="polygon", bins=4, alpha=.3,show.legend = FALSE)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 2")+scale_fill_manual(values=c("green3","deeppink"))

pTop <- ggplot(pca12_kk_sn[pca12_kk_sn$layer>0,], aes(x = x,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE)+scale_fill_manual(values=c("green3","deeppink"))+theme_bw()+xlab("PCA 1")

pRight <- ggplot(pca12_kk_sn[pca12_kk_sn$layer>0,], aes(x = y,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE) + coord_flip()+scale_fill_manual(values=c("green3","deeppink"))+theme_bw()+xlab("PCA 2")

pEmpty <- ggplot(pca12_kk_sn, aes(x = x, y = y)) +
  geom_blank() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        line = element_blank(),
        panel.background = element_blank())

forlegp3=ggplot(data=pca12_kk_sn)+stat_contour(aes(x=x, y=y, z=layer,fill=species),geom="polygon", bins=4, alpha=.3,show.legend = TRUE)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 2")+scale_fill_manual(values=c("green3","deeppink"))

legend_p3 <- get_legend(forlegp3)

grid.arrange(pTop, legend_p3, p3, pRight,
             ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
