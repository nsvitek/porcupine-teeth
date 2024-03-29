# Load Dependencies -----
library(geomorph)
library(dplyr)
library(ggplot2)
library(ggthemes) #ptol_pal
library(car) #modelling of ratios
library(caret)
library(inlmisc) #for more Paul Tol color options
library(reshape2) #for reformatting linear data to facet
library(gridExtra) #for multiplotting learning models
library(kernlab) #part of machine learning
library(MASS) #part of machine learning
library(randomForest) #part of machine learning

scriptsdir <- "C://scripts/porcupine-teeth"
# scriptsdir <- "C://cygwin/home/N.S/scripts/porcupine-teeth"
source(paste(scriptsdir,"/../observer-free-morphotype-characterization/find_repeatablePCs.R",sep=""))
source(paste(scriptsdir,"/calculate_error.R",sep=""))

replicates<-1000 #bootstrap number, low for now, increase for final calcs

#for plotting
single.column.width<-3.27
double.column.width<-6.61
scale_n<-as.character(GetColors(10,scheme="sunset"))
scale_slice<-as.character(GetColors(9,scheme="discrete rainbow"))
scale_taxon<-as.character(GetColors(9,scheme="muted"))

# Data Input ------
setwd("D://Dropbox/Documents/research/mammals/porcupines/dental_variation/Pilot_Dataset_3_Digitized")
# setwd("C://Users/N.S/Dropbox/Documents/research/mammals/porcupines/dental_variation/Pilot_Dataset_3_Digitized")

#read in Procrustes Aligned coordinates
lm.raw<-readland.tps("digitized_aligned.tps")

#read in metadata
metadata.raw<-read.csv("Digitized_Porcupine_Dataset_3.csv")

#Remove fossils that will be studied in another paper
lm.3d<-lm.raw[,,-which(metadata.raw$Specimen%in%c("UF-UF-223810","UF-UF-121740"))]
metadata<-metadata.raw[-which(metadata.raw$Specimen%in%c("UF-UF-223810","UF-UF-121740")),]

#make a single variable that identifies the "thing" being replicated
#(ex: the base of the m3 of FMNH-M-43289, not to be mixed up with the middle or top of the tooth)
metadata$Identity<-paste(metadata$Specimen,metadata$Molar,metadata$Slice_from_Base,sep="_")

#reformat/reproject shape data a couple of different ways
lm.2d<-two.d.array(lm.3d)

#write all results to their own folder (Folder name changed post-review)
setwd("../output_revision/")
sink(file="session_info.txt")
sessionInfo() %>% print() #for reporting package versions
sink() 

# Error Analysis UNCOMMENT FOR FINAL ANALYSIS-------
#identity acts as marker of replicates: each slice should have a version from the left and right side
error_gdf<-geomorph.data.frame(coords=lm.2d, identity=factor(metadata$Identity))

#Procrustes Regression
errorANOVA<-procD.lm(coords~identity,data=error_gdf,iter=replicates)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to size by keeping the same specimen but varying the size
avg.number.replicates<-nrow(metadata)/(unique(c(metadata$Identity)) %>% length)
#error3d and find repeatable PCs from homemade code
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)

#Look at how error is distributed across PCs (Fruciano 2016)
PCA.all<-prcomp(lm.2d, scale.=FALSE)
jpeg(filename="error_pca.jpg")
repPC<-find_repeatablePCs(PCA.all$x,factor(metadata$Identity),avg.number.replicates)
title(paste("Overall Repeatability =", round(err$repeatability,3)))
text(100,0.95,labels=paste(paste("# PCs > 0.9 =", max(which(repPC>=0.9)))))
text(100,0.85,labels=paste(paste("# PCs > 0.8 =", max(which(repPC>=0.8)))))
text(100,0.755,labels=paste(paste("# PCs > 0.6 =", max(which(repPC>=0.6)))))
dev.off()

# to reduce error, average left and right sides --------
lm.2d.intermediate <- lm.2d %>% as.data.frame
lm.2d.intermediate$Identity<-metadata$Identity

lm.2d.avg<-lm.2d.intermediate %>% group_by(Identity) %>% summarise(across(starts_with("V"),mean)) #%>% select(-Identity)

# #AFTER REVIEWER: Reviewer asked that composite shapes not be created, to only use one side or the other for analysis
# #To give a sense of how this methodological choice would influence results, 
# #We analyze extant specimens with right side only (because a few of them do not have left sides,
# #but the included fossil contains only part of the left mandible)
# lm.2d.avg<-lm.2d.intermediate[which(metadata$Side=="Right"),]

# #check
# lm.array<-arrayspecs(lm.2d.avg,p=73,k=2)
# plot(lm.array[,,46])
# lm.2d.avg$Identity #before Identity was selected out of dataset

#to get metaflexid closure to work we're going to convert to numeric 
#because there's some disagreement between left and right slices
metadata$metaflexid<-0 #metaflexid closed
metadata$metaflexid[which(metadata$metaflexid_closed=="No")]<-1 #metaflexid open

#same for buccal posterolophid
metadata$posterolophid_curvature<-0 #convex
metadata$posterolophid_curvature[which(metadata$buccal_posterolophid=="Straight")]<-1 #straight
metadata$posterolophid_curvature[which(metadata$buccal_posterolophid=="Concave")]<-2 #concave

#take averages for each Identity
metadata.avg<-metadata %>% group_by(Identity,Specimen,Slice_from_Base,Genus,Species) %>% 
  summarise(across(where(is.numeric),mean))

# #POST-REVIEW: Like shape data, edit to only sample right side
# metadata.avg<-metadata[which(metadata$Side=="Right"),]

metadata.avg$Taxon<-paste(metadata.avg$Genus,metadata.avg$Species)

# #check that everything is still in matching order
# cbind(metadata.avg$Identity,lm.2d.avg$Identity)

lm.2d.avg<-lm.2d.avg %>% select(-Identity) %>% as.matrix
# PCA FIGURES COMMENTED OUT. UNCOMMENT FOR FINAL VERSION-----------
PCA<-prcomp(lm.2d.avg, scale.=FALSE)
PCA.perc<-round(summary(PCA)$importance[2,]*100,1)

#plot PCA
metadata.avg$PC1<-PCA$x[,1]
metadata.avg$PC2<-PCA$x[,2]

ggplot(data=metadata.avg,aes(x=PC1,y=PC2))+
  geom_point(size=2,aes(color = Taxon, shape=Taxon)) +
  theme_classic() +
  xlab(paste("PC 1 (",PCA.perc[1],"%)",sep="")) +
  ylab(paste("PC 2 (",PCA.perc[2],"%)",sep="")) +
  scale_color_manual(values=scale_taxon) +
  scale_shape_manual(values=c(rep(16,6), 17, 15)) +
  theme(legend.text=element_text(face="italic"))
ggsave("PCA_spp.pdf", device = cairo_pdf, width = 12, height = 8,units="cm",dpi=600)

ggplot(data=metadata.avg,aes(x=PC1,y=PC2))+
  geom_point(size=2,aes(color = factor(Slice_from_Base), shape=Taxon)) +
  theme_classic() +
  xlab(paste("PC 1 (",PCA.perc[1],"%)",sep="")) +
  ylab(paste("PC 2 (",PCA.perc[2],"%)",sep="")) +
  scale_color_manual(name="Slice",values=scale_slice) +
  scale_shape_manual(values=c(rep(16,6), 17, 15))
ggsave("PCA_wear2.pdf", device = cairo_pdf, width = 12, height = 12,units="cm",dpi=600)

# Remove Fossil -----
metadata.avg<- metadata.avg %>% mutate("Anterofossettid Ratio" = anterofossettid_length/anterofossettid_width,
                                             "Hypolophid Ratio" = max_length_hypolophid/max_length_mesoflexid,
                                             "Ectolophid Ratio" = ectolophid_width/talonid_width,
                                             "Posterolophid Evenness" = mid_posterolophid_length/max_posterolophid_length,
                                             "Anterofossettid Angle" = anterofossettid_angle)


lm.2d.extant<-lm.2d.avg[-which(metadata.avg$Species %in% c("kleini")),]
metadata.extant<-metadata.avg[-which(metadata.avg$Species %in% c("kleini")),]

metadata.avg[which(metadata.avg$Species=="kleini"),] %>% print.data.frame

# #REDO FOR REVIEW
# lm.2d.extant<-lm.2d.avg
# metadata.extant<-metadata.avg

#check for match
# dim(metadata.extant)
# dim(lm.2d.extant)

# Procrustes ANOVA -----
source(paste(scriptsdir,"Procrustes_ANOVA.R",sep="/"))
# #Traditional Morphometrics -------
source(paste(scriptsdir,"univariate_analysis.R",sep="/"))
# machine learning -------
#variable choice
source(paste(scriptsdir,"/Choosing_PCs.R",sep=""))
# source(paste(scriptsdir,"/Choosing_PCs_Less_Worn.R",sep=""))

#model building, evaluation
source(paste(scriptsdir,"/Learning_Models.R",sep=""))
