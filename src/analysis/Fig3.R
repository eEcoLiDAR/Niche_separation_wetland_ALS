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

pca12_gr_kk=rbind(grw_pca12_df,kk_pca12_df)
pca12_gr_sn=rbind(grw_pca12_df,sn_pca12_df)
pca12_kk_sn=rbind(kk_pca12_df,sn_pca12_df)

######
p1=ggplot(data=pca12_gr_kk)+stat_contour(aes(x=x, y=y, z=layer,fill=species), geom="polygon", bins=4, alpha=.3,show.legend = FALSE)+
  theme_bw()+xlab("PCA 2")+ylab("PCA 3")+scale_fill_manual(values=c("goldenrod4","green3"))

pTop1 <- ggplot(pca12_gr_kk[pca12_gr_kk$layer>0,], aes(x = x,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE)+scale_fill_manual(values=c("goldenrod4","green3"))+theme_bw()+xlab("PCA 2")

pRight1 <- ggplot(pca12_gr_kk[pca12_gr_kk$layer>0,], aes(x = y,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE) + coord_flip()+scale_fill_manual(values=c("goldenrod4","green3"))+theme_bw()+xlab("PCA 3")

forlegp1=ggplot(data=pca12_gr_kk)+stat_contour(aes(x=x, y=y, z=layer,fill=species),geom="polygon", bins=4, alpha=.3,show.legend = TRUE)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 3")+scale_fill_manual(values=c("goldenrod4","green3"))
legend_p1 <- get_legend(forlegp1)

fig3a=grid.arrange(pTop1, legend_p1, p1, pRight1,
             ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))

ggsave("Fig3_a_pc23.png",plot = fig3a,width = 15, height = 12)

######

p2=ggplot(data=pca12_gr_sn)+stat_contour(aes(x=x, y=y, z=layer,fill=species), geom="polygon", bins=4, alpha=.3,show.legend = FALSE)+
  theme_bw()+xlab("PCA 2")+ylab("PCA 3")+scale_fill_manual(values=c("goldenrod4","deeppink"))

pTop2 <- ggplot(pca12_gr_sn[pca12_gr_sn$layer>0,], aes(x = x,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE)+scale_fill_manual(values=c("goldenrod4","deeppink"))+theme_bw()+xlab("PCA 2")

pRight2 <- ggplot(pca12_gr_sn[pca12_gr_sn$layer>0,], aes(x = y,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE) + coord_flip()+scale_fill_manual(values=c("goldenrod4","deeppink"))+theme_bw()+xlab("PCA 3")

forlegp2=ggplot(data=pca12_gr_sn)+stat_contour(aes(x=x, y=y, z=layer,fill=species),geom="polygon", bins=4, alpha=.3,show.legend = TRUE)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 2")+scale_fill_manual(values=c("goldenrod4","deeppink"))
legend_p2 <- get_legend(forlegp2)

fig3b=grid.arrange(pTop2, legend_p2, p2, pRight2,
             ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))

ggsave("Fig3_b_pc23.png",plot = fig3b,width = 15, height = 12)

######

p3=ggplot(data=pca12_kk_sn)+stat_contour(aes(x=x, y=y, z=layer,fill=species),geom="polygon", bins=4, alpha=.3,show.legend = FALSE)+
  theme_bw()+xlab("PCA 2")+ylab("PCA 3")+scale_fill_manual(values=c("green3","deeppink"))

pTop3 <- ggplot(pca12_kk_sn[pca12_kk_sn$layer>0,], aes(x = x,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE)+scale_fill_manual(values=c("green3","deeppink"))+theme_bw()+xlab("PCA 2")

pRight3 <- ggplot(pca12_kk_sn[pca12_kk_sn$layer>0,], aes(x = y,fill=species)) +
  geom_density(alpha=.3,show.legend = FALSE) + coord_flip()+scale_fill_manual(values=c("green3","deeppink"))+theme_bw()+xlab("PCA 3")

forlegp3=ggplot(data=pca12_kk_sn)+stat_contour(aes(x=x, y=y, z=layer,fill=species),geom="polygon", bins=4, alpha=.3,show.legend = TRUE)+
  theme_bw()+xlab("PCA 1")+ylab("PCA 2")+scale_fill_manual(values=c("green3","deeppink"))
legend_p3 <- get_legend(forlegp3)

fig3c=grid.arrange(pTop3, legend_p3, p3, pRight3,
             ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))

ggsave("Fig3_c_pc23.png",plot = fig3c,width = 15, height = 12)

###### tests vis.

humboldt.plot.density <- function(x, type = "D", title = "Background") {
  if (type == "D") {
    obs <- x$obs$D
    sim <- x$sim$D
    p <- x$p.D
  }
  if (type == "I") {
    obs <- x$obs$I
    sim <- x$sim$I
    p <- x$p.I
  }
  # densObs <- density(obs)
  densSim <- density(sim, adjust = 0.5)
  # xlim <- c(0,1)
  r0 <- c(sim, obs)
  l0 <- max(sim) - min(sim)
  w0 <- l0/(log(length(sim), base = 2) + 1)
  xlim <- range(r0) + c(-w0, w0)
  if (xlim[1] < 0) {
    xlim <- c(0, xlim[2])
  }
  if (xlim[2] > 1) {
    xlim <- c(xlim[1], 1)
  }
  ylim <- range(0, densSim$y, obs)
  # pick the colours obsCol <- rgb(1,0,0,0.2)
  simCol <- rgb(0, 0, 1, 0.2)
  ## plot the carrots and set up most of the plot parameters
  plot(densSim, xlim = xlim, ylim = ylim, main = title, xlab = type, sub = paste("p.value = ", 
                                                                                 round(p, 5)))
  # put our density plots in polygon(densObs, density = -1, col = obsCol)
  polygon(densSim, density = -1, col = simCol)
  y0 <- max(densSim$y, obs)
  lines(c(obs, obs), c(y0 * 0.7, 0), col = "red")
  points(obs, y0 * 0.7, pch = 18, cex = 2, col = "red")
  invisible()
}

# grw test

pdf("NicheTests_pca12.pdf") 

eq_grw_kk <- readRDS("eq.test_gr_k.rds")
sim_grw_kk <- readRDS("sim.test_gr_k.rds")
sim_kk_grw <- readRDS("sim.test_k_gr.rds")

humboldt.plot.density(sim_grw_kk,"D","Background test (Great reed warbler->Reed warbler)") 
humboldt.plot.density(sim_kk_grw,"D","Background test (Reed warbler -> Great reed warbler)")
humboldt.plot.density(eq_grw_kk,"D","Equivalency test (Great reed warbler and Reed warbler)") 

# kk test

eq_grw_kk <- readRDS("eq.test_gr_k.rds")
sim_grw_kk <- readRDS("sim.test_gr_k.rds")
sim_kk_grw <- readRDS("sim.test_k_gr.rds")

humboldt.plot.density(sim_grw_kk,"D","Background test (Great reed warbler->Reed warbler)") 
humboldt.plot.density(sim_kk_grw,"D","Background test (Reed warbler -> Great reed warbler)")
humboldt.plot.density(eq_grw_kk,"D","Equivalency test (Great reed warbler and Reed warbler)") 

# sn test

eq_k_s <- readRDS("eq.test_k_s.rds")
sim_k_s <- readRDS("sim.test_k_s.rds")
sim_s_k <- readRDS("sim.test_s_k.rds")

humboldt.plot.density(sim_k_s,"D","Background test (Reed warbler->Savi's warbler)") 
humboldt.plot.density(sim_s_k,"D","Background test (Savi's warbler -> Reed warbler)")
humboldt.plot.density(eq_k_s,"D","Equivalency test (Reed warbler and Savi's warbler)") 

dev.off() 
