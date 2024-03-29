library(geomorph)
library(dplyr)

setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")

lm.raw<-readland.tps("digitized_aligned.tps") 
dim(lm.raw) 
# which is p? k? n?

# convert dimensions --------
#sometimes helpful to save the numbers with labels to retrieve later
lm.dimensions<-list(p=dim(lm.raw)[1],
                    k=dim(lm.raw)[2],
                    n=dim(lm.raw)[3])

lm.2d<-two.d.array(lm.raw)
dim(lm.2d)

lm.array<-arrayspecs(lm.2d, p=lm.dimensions$p, k=lm.dimensions$k)
dim(lm.array)

# 9.0.2 PCA ------
PCA<-prcomp(lm.2d, scale.=FALSE)

# 9.0.3 metadata ---------
metadata<-read.csv("PorcupineMetadataComplete.csv")
head(metadata)

#make a single variable that identifies the "thing" being replicated 
#(ex: the base of the m3 of FMNH-M-43289, not to be mixed up with the middle or top of the tooth)
metadata$Identity<-paste(metadata$Specimen,metadata$Molar,metadata$Slice_from_Base,sep="_")

# 9.0.4 Visualize -----
#this visualization will be slightly fancier than exercise 1
# install.packages("RColorBrewer")
library(RColorBrewer)
unique(metadata$Specimen) %>% length #how many colors do we need to represent each specimen?

specimen.colors<-brewer.pal(n = 8, name = "Dark2") #set colors using R Color Brewer Palettes
specimen.factor<-factor(metadata$Specimen) #subsetting, or putting things in brackets "[]" usually only works with factors

# #Try 1
# plot(PCA$x[,1:2],col=specimen.colors[specimen.factor]) #okay, but hard to see color.
# 
# #Try 2
# plot(PCA$x[,1:2],bg=specimen.colors[specimen.factor],pch=21) #note change from col to bg, addition of pch
# 
# #Try 3
# #Let's try to add one more layer of information: how high in the tooth is an image?
# unique(metadata$Slice_from_Base) #there are at least 10 options, need to switch back to first coloring option

lm.replicate.specs<-metadata$Identity[metadata$Landmark_Replicate==1] %>% unique
only.lm.reps<-which(metadata$Identity %in% lm.replicate.specs)

#look at reorient replicates
plot(PCA$x[,1:2],col=specimen.colors[specimen.factor],pch=metadata$Slice_from_Base) 
legend('topright',legend=levels(specimen.factor),col=specimen.colors,pch=16) #a legend gives the colors meaning. 

# #look at landmark replicates
# plot(PCA$x[only.lm.reps,1:2],col=metadata$Slice_from_Base[only.lm.reps],
#      pch=metadata$Landmark_Replicate[only.lm.reps]) 
# 
# #which specimens to compare?
filter(metadata,Slice_from_Base==1,Specimen=="UF-M-7993",Reposition_Replicate==1)
# #112, 133, need to +1 because order starts at 0
# 
# plotRefToTarget(lm.array[,,112+1],lm.array[,,133+1], method="vector")
# plotRefToTarget(lm.array[,,112+1],lm.array[,,133+1], method="points")
# 
# nrow(lm.array[,,1])
# #the combination of color+shape uniquely identifies a "shape" that you replicated through 
# #(1) left vs. right side of a specimen, 
# #(2) reorienting an image stack
# #(3) re-landmarking ab image


# cbind(PCA$x[,2],metadata$Identity) %>% .[which(PCA$x[,2]>=0.06),]
# 9.1.1 Geomorph Data Frame -------
testgdf<-geomorph.data.frame(coords=lm.raw,
                             specimen=factor(metadata$Identity),
                             reposition=factor(metadata$Reposition_Replicate),
                             landmark=factor(metadata$Landmark_Replicate))

# 9.1.2 Procrustes ANOVA ------
errorANOVA<-procD.lm(coords~specimen/reposition/landmark,data=testgdf,iter=99) 
errorANOVA$aov.table

# 9.1.3 Error Analysis -------
#The function as currently written:
#used with output of procD.lm
#r are the number of replicates
avg.number.replicates<-nrow(metadata)/(unique(metadata$Identity) %>% length)

errorGM<-function(ANOVA,r,f1=1,f2=2){ #f1 and f2 are which factors you want to compare
    #default f1 and f2 assume only one level in ANOVA, not nested
    # Fruciano 2016 repeatibility
    s.among<-(ANOVA$MS[f1]-ANOVA$MS[f2])/r
    repeatability<-s.among/(ANOVA$MS[f2]+s.among)
    # Yezerinac et al. 1992 p. 474 % measurement error
    percent_measurement_error<-(ANOVA$MS[f2]/(ANOVA$MS[f2]+s.among))*100
    result<-list(repeatability,percent_measurement_error)
    names(result)<-c("repeatability","PME")
    return(result)
}
 
errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2) #all possible sources of error
errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=3) #error due to landmark placement only

#note one specimen is broken on one half, potentially influencing results: 
broken<-which(metadata$Identity=="UF-M-7993_m3_3" & metadata$Reposition_Replicate==0)
testgdf<-geomorph.data.frame(coords=lm.raw[,,-broken],
                             specimen=factor(metadata$Identity[-broken]),
                             reposition=factor(metadata$Reposition_Replicate[-broken]),
                             landmark=factor(metadata$Landmark_Replicate[-broken]))
errorANOVA<-procD.lm(coords~specimen/reposition/landmark,data=testgdf,iter=99) 
errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2) #all possible sources of error
errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=3) #error due to landmark placement only

find_repeatablePCs<-function(PCscores,specimen,variable,rep){
  repeatability<-rep(NA,ncol(PCscores))
  for(i in 1:ncol(PCscores)){
    testgdf<-geomorph.data.frame(coords=PCscores[,i],specimen=factor(specimen), variable=factor(variable))
    errorANOVA<-procD.lm(coords~specimen/variable,data=testgdf,iter=999,RRPP=TRUE) %>% .$aov.table
    repeatability[i]<-errorGM(errorANOVA,rep) %>% .$repeatability
    if(repeatability[i]<0){repeatability[i]<-0}
  }
  plot(repeatability,xlab="principal components")
  lines(repeatability)
  abline(h=0.95,col="red",lty=2)
  abline(h=0.90,col="blue",lty=3)
  return(repeatability)
}

find_repeatablePCs(PCA$x,metadata$Identity,metadata$Reposition_Replicate,avg.number.replicates)

#compare
plot(c(1:nrow(PCA$x)),summary(PCA)$importance[2,])

# Step 11 draft version ###################
#note one specimen is broken on one half, potentially influencing results: 
broken<-which(metadata$Identity=="UF-M-7993_m3_3" & metadata$Reposition_Replicate==0)
lm.complete<-lm.raw[,,-broken]
metadata.complete<-metadata[-broken,]


weargdf<-geomorph.data.frame(coords=lm.complete,
                             wear=factor(metadata.complete$Slice_from_Base))

linear.model<-procD.lm(coords~wear,data=weargdf,iter=99) 

summary(linear.model)

shape.predictor(A= weargdf$coords,x=weargdf$wear)
?shape.predictor
