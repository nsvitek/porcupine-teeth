library(geomorph)
library(dplyr)
# library(RColorBrewer)
# library(caret)

setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")
# setwd("C:/Users/N.S/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")


lm.raw<-readland.tps("digitized_aligned.tps") 
dim(lm.raw) 

# UPGMA ---------

#do a second Procrustes analysis, but just to calculate Procrustes distances between images
shapes<-gpagen(lm.raw)
procrustes.distance.matrix<-shapes$procD

# perform the clustering using UPGMA ("average")

cluster<-hclust(procrustes.distance.matrix, method="average")
# compute the distances along the branches
D2<-cophenetic(cluster)

# compute the correlation
cor(procrustes.distance.matrix,D2)

#See p. 107 o fRevisedPracticalCompanion.pdf inthe OSF Reading folder
# If the distances in the plot represent the data well, the correlation will be close to 1.0; values of
# 0.85 or less are taken to mean that the branching distances in the diagram distort the distances
# in the data. 

# draw the dendrogram
plot(cluster,hang=0.1)


# metadata ---------
metadata<-read.csv("PorcupineMetadataComplete.csv")
head(metadata)

#note one specimen is broken on one half, potentially influencing results: 
broken<-which(metadata$Specimen=="UF-M-7993" & metadata$Slice_from_Base==3 & metadata$Reposition_Replicate==0)
lm.complete<-lm.raw[,,-broken]
metadata.complete<-metadata[-broken,]

old.labels<-cluster$labels
cluster$labels
cluster$order
cluster$labels<-metadata.complete$Genus[cluster$order]
plot(cluster,hang=0.1)


