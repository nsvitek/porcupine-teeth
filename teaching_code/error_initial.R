# Data Input ------
library(geomorph)
library(dplyr)

setwd("D://Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Porcupine_Project_Pilot_2/Combined_Digitized/")

#read in Procrustes Aligned coordinates
lm.raw<-readland.tps("digitized_aligned.tps")

#read in metadata
metadata<-read.csv("PorcupineMetadataComplete.csv")

#make a single variable that identifies the "thing" being replicated
#(ex: the base of the m3 of FMNH-M-43289, not to be mixed up with the middle or top of the tooth)
metadata$Identity<-paste(metadata$Specimen,metadata$Molar,metadata$Slice_from_Base,sep="_")

#add one specifically for right side reposition replicates
metadata$Reposition<-metadata$Reposition_Replicate==2

#reformat/reproject shape data a couple of different ways
lm.2d<-two.d.array(lm.raw)

#write all results to their own folder (Folder name changed post-review)
sink(file="session_info.txt")
sessionInfo() %>% print() #for reporting package versions
sink() 

# Error Analysis -------
#identity acts as marker of replicates: each slice should have a version from the left and right side
error_gdf<-geomorph.data.frame(coords=lm.2d, identity=factor(metadata$Identity),
                               side=factor(metadata$Side),replicate=factor(metadata$Reposition_Replicate),
                               landmark=factor(metadata$Landmark_Replicate),reposition=factor(metadata$Reposition))

#Procrustes Regression
errorANOVA<-procD.lm(coords~identity,data=error_gdf,iter=replicates)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to size by keeping the same specimen but varying the size
avg.number.replicates<-nrow(metadata)/(unique(c(metadata$Identity)) %>% length)
#error3d and find repeatable PCs from homemade code
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)

#check and see if order makes a difference
errorANOVA<-procD.lm(coords~identity/side/reposition/landmark,data=error_gdf,iter=replicates)
errorANOVA$aov.table

errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=3)
errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=4)

errorANOVA2<-procD.lm(coords~identity/reposition/side/landmark,data=error_gdf,iter=replicates)
errorANOVA2$aov.table
errorGM(errorANOVA2$aov.table,r=avg.number.replicates,f1=1,f2=2)

errorANOVA3<-procD.lm(coords~identity/landmark/reposition/side,data=error_gdf,iter=replicates)
errorANOVA3$aov.table
