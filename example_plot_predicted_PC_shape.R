library(geomorph)
library(dplyr)

setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")

lm.raw<-readland.tps("digitized_aligned.tps") 
dim(lm.raw) 
lm.2d<-two.d.array(lm.raw)

# metadata ---------
metadata<-read.csv("PorcupineMetadataComplete.csv")
head(metadata)

# clean data -------
#note one specimen is broken on one half, potentially influencing results: 
broken<-which(metadata$Specimen=="UF-M-7993" & metadata$Slice_from_Base==3 & metadata$Reposition_Replicate==0)
lm.complete<-lm.raw[,,-broken]
metadata.complete<-metadata[-broken,]

# Step 11 draft linear model, results---------
#make a geomorph data frame
weargdf<-geomorph.data.frame(coords=lm.complete,
                             wear=factor(metadata.complete$Slice_from_Base))

#build the linear model
linear.model<-procD.lm(coords~wear,data=weargdf,iter=99) 


# Visualize shapes predicted by principal components --------
#find mean shape of the whole dataset
M <- mshape(lm.complete)

#make the residual values an object
remaining.shape<-linear.model$GM$residuals

#add mean shape back to residuals for later plotting. 
#This next line of code is not elegant or recommended but it gets the job done.
for(i in 1:dim(remaining.shape)[3]){ remaining.shape[,,i]<-remaining.shape[,,i] + M}

#use geomorphs PCA
PCA.residuals2<-gm.prcomp(remaining.shape)

#plot PCA plot
plot(PCA.residuals2)
plot(PCA.residuals2,col=c("red","blue","green")[factor(metadata.complete$Specimen)])
legend('topleft',legend = levels(factor(metadata.complete$Specimen)),col=c("red","blue","green"),pch=16)

#shape predicted by minimum PC1 value, arrows point to mean shape. mshape() function calculates mean shape.
plotRefToTarget(PCA.residuals2$shapes$shapes.comp1$min, mshape(remaining.shape), 
                method = "vector")
#shape predicted by maximum PC1 value, same setup
plotRefToTarget(PCA.residuals2$shapes$shapes.comp1$max, mshape(remaining.shape), 
                method = "vector")

#compare maximum (points) to minimum (arrow)
plotRefToTarget(PCA.residuals2$shapes$shapes.comp1$max, PCA.residuals2$shapes$shapes.comp1$min, 
                method = "vector")
