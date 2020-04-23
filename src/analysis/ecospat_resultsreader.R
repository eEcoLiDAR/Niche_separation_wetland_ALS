library(ecospat)

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v11/"
setwd(workingdirectory)

# PCA 1 vs PCA 2

eq.test_gr_k<- readRDS("eq.test_gr_k.rds")
sim.test_gr_k<- readRDS("sim.test_gr_k.rds")
sim.test_k_gr<- readRDS("sim.test_k_gr.rds")

ecospat.plot.overlap.test(eq.test_gr_k, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_k_gr, "D", "Similarity")

eq.test_k_s<- readRDS("eq.test_k_s.rds")
sim.test_k_s<- readRDS("sim.test_k_s.rds")
sim.test_s_k<- readRDS("sim.test_s_k.rds")

ecospat.plot.overlap.test(eq.test_k_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_s_k, "D", "Similarity")

eq.test_gr_s<- readRDS("eq.test_gr_s.rds")
sim.test_gr_s<- readRDS("sim.test_gr_s.rds")
sim.test_s_gr<- readRDS("sim.test_s_gr.rds")

ecospat.plot.overlap.test(eq.test_gr_s, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_s_gr, "D", "Similarity")

# PCA 1 vs PCA 3

eq.test_gr_k2<- readRDS("eq.test_gr_k2.rds")
sim.test_gr_k2<- readRDS("sim.test_gr_k2.rds")
sim.test_k_gr2<- readRDS("sim.test_k_gr2.rds")

ecospat.plot.overlap.test(eq.test_gr_k2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k2, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_k_gr2, "D", "Similarity")

eq.test_k_s2<- readRDS("eq.test_k_s2.rds")
sim.test_k_s2<- readRDS("sim.test_k_s2.rds")
sim.test_s_k2<- readRDS("sim.test_s_k2.rds")

ecospat.plot.overlap.test(eq.test_k_s2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s2, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_s_k2, "D", "Similarity")

eq.test_gr_s2<- readRDS("eq.test_gr_s2.rds")
sim.test_gr_s2<- readRDS("sim.test_gr_s2.rds")
sim.test_s_gr2<- readRDS("sim.test_s_gr2.rds")

ecospat.plot.overlap.test(eq.test_gr_s2, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s2, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_s_gr2, "D", "Similarity")

# PCA 2 vs PCA 3

eq.test_gr_k3<- readRDS("eq.test_gr_k3.rds")
sim.test_gr_k3<- readRDS("sim.test_gr_k3.rds")
sim.test_k_gr3<- readRDS("sim.test_k_gr3.rds")

ecospat.plot.overlap.test(eq.test_gr_k3, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_k3, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_k_gr3, "D", "Similarity")

eq.test_k_s3<- readRDS("eq.test_k_s3.rds")
sim.test_k_s3<- readRDS("sim.test_k_s3.rds")
sim.test_s_k3<- readRDS("sim.test_s_k3.rds")

ecospat.plot.overlap.test(eq.test_k_s3, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_k_s3, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_s_k3, "D", "Similarity")

eq.test_gr_s3<- readRDS("eq.test_gr_s3.rds")
sim.test_gr_s3<- readRDS("sim.test_gr_s3.rds")
sim.test_s_gr3<- readRDS("sim.test_s_gr3.rds")

ecospat.plot.overlap.test(eq.test_gr_s3, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test_gr_s3, "D", "Similarity")
ecospat.plot.overlap.test(sim.test_s_gr3, "D", "Similarity")