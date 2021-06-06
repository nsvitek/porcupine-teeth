library(geomorph)
# library(dplyr)
# library(RColorBrewer)
# library(caret)

# setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")
setwd("E://Pilot_Dataset_3_Digitized")
lm.raw<-readland.tps("digitized_aligned.tps")
lm.2d<-two.d.array(lm.raw)
# metadata ---------
metadata<-read.csv("Digitized_Porcupine_Dataset_3.csv")


# in this exampe, mean shape of species---------

#choose a slice to work with
slice_num<-2

#subset dataset
slice2.data<-lm.2d[which(metadata$Slice_from_Base==slice_num),]
slice2.metadata<-metadata[which(metadata$Slice_from_Base==slice_num),]

#compare the genera "Erethizon" and "Coendou"
g.coendou<-which(slice2.metadata$Genus=="Coendou")
g.erethizon<-which(slice2.metadata$Genus=="Erethizon")
length(g.coendou)
length(g.erethizon)

slice2.3d.data<-arrayspecs(slice2.data,p=ncol(slice2.data)/2, k=2)
#find mean shape for each species
M.coendou <- mshape(slice2.3d.data[,,g.coendou])
M.erethizon <- mshape(slice2.3d.data[,,g.erethizon])

dim(M.coendou)
dim(M.erethizon)
#look at shape differences associated with minimum and maximum in comparison to mean shape
#plotting the difference in mean shapes side by size
par(mfrow=c(1,2))
plotRefToTarget(M.coendou, M.erethizon, mag=1,method="vector")
grid(10,10,col = 'blue')
title("Slice =2; M.coendou - M.erethizon")

plotRefToTarget(M.erethizon,M.coendou, mag=1,method="vector")
grid(10,10,col = 'blue')
title("Slice =2; M.erethizon - M.coendou")

par(mfrow=c(1,2))
#plotting in mean shapes side by size
plot(M.coendou)
title("Slice =2; Genus = Coendou")
grid(10,10,col = 'blue')
plot(M.erethizon)
title("Slice =2; Genus = Erethizon")
grid(10,10,col = 'blue')
par(mfrow=c(1,1)) #resetting the graph region
