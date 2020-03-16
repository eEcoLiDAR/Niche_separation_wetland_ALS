library(sdm)

workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
setwd(workingdirectory)


GrW=read.sdm("GrW.sdd")
KK=read.sdm("KK.sdd")
Sn=read.sdm("Sn.sdd")

#GrW

names(GrW@features)<-c("rID","id","vegdens_1_2","veg_dens_2_3","veg_dens_b1","shannon","p25","p95","dsm_sd","lowveg_sd","lowveg_prop","pulse_pen","std","var","species")
GrW@features.name=c("id","vegdens_1_2","veg_dens_2_3","veg_dens_b1","shannon","p25","p95","dsm_sd","lowveg_sd","lowveg_prop","pulse_pen","std","var","species")

model_GrW <- sdm(occurrence~vegdens_1_2+veg_dens_2_3+veg_dens_b1+shannon+p25+p95+dsm_sd+lowveg_sd+lowveg_prop+pulse_pen+std+var,data=GrW,methods=c('rf'),replication='boot',test.percent=30,n=100)
model_GrW 

vi <- getVarImp(model_GrW, method='rf')
vi
plot(vi)

rcurve(model_GrW , id=1:100)
