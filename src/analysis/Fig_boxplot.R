library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(multcompView)

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- variable[,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Global
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
#workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter3_wetlandniche/3_Dataprocessing/Niche_v3/"
setwd(workingdirectory)

# Import data
GrW=read.csv("GrW_wlandsc.csv")
#GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
GrW_lgn8 <- subset(GrW, lgn8 %in% c(16,17,30,322,332,41,42))

KK=read.csv("KK_wlandsc.csv")
#KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
KK_lgn8 <- subset(KK, lgn8 %in% c(16,17,30,322,332,41,42))

Sn=read.csv("Sn_wlandsc.csv")
#Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,323,332,333,41,42,43,45,46,47))
Sn_lgn8 <- subset(Sn, lgn8 %in% c(16,17,30,322,332,41,42))

#data_merged=rbind(GrW,KK,Sn)
data_merged=rbind(GrW_lgn8,KK_lgn8,Sn_lgn8)

data_presabs_stat <- data_merged %>%
  group_by(species,occurrence) %>%
  summarise(nofobs = length(occurrence))

data_merged=subset(data_merged,select=c(12,10,9,7,8,13,14,15,24,27,21,22,6,4,5,3))
names(data_merged) <- c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                        "HH_sd","HH_lowveg_sd", "HH_lowveg_prop","HH_medveg_patch","HH_medveg_edge",
                        "species","occurrence","lgn8","x","y","id")

noffea=10

data_merged=data_merged[(data_merged$VV_p95<30 & data_merged$VV_p95>0.1),]
data_merged[is.na(data_merged)==TRUE] <- 0

write.csv(data_merged,"data_merged.csv")

# boxplot per species  Fig.1.

data_merged_mod=data_merged
levels(data_merged_mod$species) = c("Grote Karekiet","Kleine Karekiet","Snor","Grw","Rw","Sw","Abs")
data_merged_mod[data_merged_mod$species=="Grote Karekiet",noffea+1] <- "Grw"
data_merged_mod[data_merged_mod$species=="Kleine Karekiet",noffea+1] <- "Rw"
data_merged_mod[data_merged_mod$species=="Snor",noffea+1] <- "Sw"
data_merged_mod[data_merged_mod$occurrence==0,noffea+1] <- "Abs"

data_sel=subset(data_merged_mod,select=c(1:noffea,noffea+1))
data_sel2=data_sel %>% gather(-species,key = "var", value = "value")
as.factor(data_sel2$var)
data_sel2$var <- factor(data_sel2$var, levels = c("VV_p95","VV_FHD","VD_0_1","VD_1_2","VD_2_3",
                                                  "HH_sd","HH_lowveg_sd", "HH_lowveg_prop","HH_medveg_patch","HH_medveg_edge"))

p <- ggplot(data=data_sel2, aes(x=species , y=value, fill=species)) +  
  geom_boxplot(outlier.shape=NA) +
  facet_wrap(~var,scales = "free") +
  scale_fill_brewer(palette="Greens") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

for (facetk in as.character(unique(data_sel2$var))) {   
  subdf <- subset(data_sel2, var==facetk)
  model=lm(value ~ species, data=subdf)
  ANOVA=aov(model)
  TUKEY <- TukeyHSD(ANOVA)
  
  labels <- generate_label_df(TUKEY , TUKEY[["species"]])
  names(labels) <- c('Letters','species')
  yvalue <- aggregate(.~species, data=subdf, quantile, probs=.75)  
  final <- merge(labels, yvalue)
  final$var <-  facetk
  
  p <- p + geom_text(data = final,  aes(x=species, y=value, label=Letters), 
                     vjust=-.5, hjust=-.5)
}
p

#### simple boxplot

p0=ggplot(data=data_sel2,aes(x = species, y = value, fill=species)) + geom_boxplot() + facet_wrap(~var, scales = "free") + 
  scale_fill_manual(values=c("goldenrod4","green3","deeppink","black"),name="Species",labels=c("Great reed warbler (Grw)","Reed warbler (Rw)","Savi's warbler (Sw)","Absence (Abs)"))+ 
  ylab("LiDAR metrics")+theme_bw(base_size=30)
