library(geomorph)
library(dplyr)
library(RColorBrewer)

# setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")
setwd("C:/Users/N.S/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")

lm.raw<-readland.tps("digitized_aligned.tps") 
dim(lm.raw) 

# PCA ------
PCA<-prcomp(lm.2d, scale.=FALSE)

# metadata ---------
metadata<-read.csv("PorcupineMetadataComplete.csv")
head(metadata)

#note one specimen is broken on one half, potentially influencing results: 
broken<-which(metadata$Specimen=="UF-M-7993" & metadata$Slice_from_Base==3 & metadata$Reposition_Replicate==0)
lm.complete<-lm.raw[,,-broken]
metadata.complete<-metadata[-broken,]

# Step 11 draft version ---------
#make a geomorph data frame
weargdf<-geomorph.data.frame(coords=lm.complete,
                             wear=factor(metadata.complete$Slice_from_Base))

#build the linear model
linear.model<-procD.lm(coords~wear,data=weargdf,iter=99) 

#summary statistics about the model
summary(linear.model)

# diagnostic plots
plot(linear.model, type = "diagnostics") 

# PC plot rotated to major axis of fitted values. 
# Remember that a PCA and resulting PC plot doesn't change the original data. It only looks at the data from a new 
# set of axes. It's sort of like looking at the Milky Way galaxy from another planet besides our own.
# The galaxy will look different because our perspective is different, but all of the stars are in the same place they were before.
# In a standard PCA, the main axis (PC1) is the one linear axis that can explain the greatest amount of variation all on its own. 
# In the PC below, we're finding yet another perspective from which to look at the same data.
# This time, "PC1" is major axis of fitted values, or the calculated regression line of the model. qa
plot(linear.model, type = "PC", pch = 19, col = c("blue","red","green","yellow")[factor(metadata$Slice_from_Base)] )

# Use fitted values from the model to make prediction lines and
# Use coefficients from the model to find the projected regression scores
rat.plot <- plot(linear.model, type = "regression", 
                 predictor = as.vector(weargdf$wear), reg.type = "RegScore", 
                 pch = 21, bg = "yellow") 

# Find fitted shapes for min and max scores in previous plot. 
# This code could be altered to find the mean regression score for each wear stage
preds <- shape.predictor(linear.model$GM$fitted, x = rat.plot$RegScore, 
                         predmin = min(rat.plot$RegScore), 
                         predmax = max(rat.plot$RegScore))

#find mean shape of the whole dataset
M <- mshape(lm.complete)

#look at shape differences associated with minimum and maximum in comparison to mean shape
plotRefToTarget(M, preds$predmin, mag=1,method="vector")
plotRefToTarget(M, preds$predmax, mag=1,method="vector")

linear.model$fitted[1:3, ] # the fitted values (for the first three specimens)
linear.model$GM$fitted[,, 1:3] # the fitted values as Procrustes coordinates (same specimens)

#make the residual values an object
remaining.shape<-linear.model$GM$residuals
plot(remaining.shape[,,1])
#the residual shape is hard to visualize unless mean shape is added back in.
plot(remaining.shape[,,1]+M)

#perform PCA on residuals
PCA.residual<-prcomp(linear.model$residuals, scale.=FALSE)

#explore results
summary(PCA.residual)
plot(PCA.residual$x[,1:2],pch=metadata.complete$Slice_from_Base,
     col=c("red","blue","green")[factor(metadata.complete$Specimen)])
