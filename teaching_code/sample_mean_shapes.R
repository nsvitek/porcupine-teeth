library(geomorph)
# library(dplyr)
# library(RColorBrewer)
# library(caret)

# setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")
setwd("C:/Users/N.S/Dropbox/Documents/research/mammals/porcupines/dental_variation/12102020-Pilot_Dataset_3_Digitized")

lm.raw<-readland.tps("digitized_aligned.tps") 
dim(lm.raw) 
lm.2d<-two.d.array(lm.raw)
# metadata ---------
metadata<-read.csv("Porcupine Mandible Full Dataset - PorcupineDataset3_SliceFromBaseData.csv")
head(metadata)

#check for same # of items
nrow(lm.2d) == nrow(metadata)

#set color scheme 
specimen.colors<-brewer.pal(n = 8, name = "Dark2") #set colors using R Color Brewer Palettes

# in this exampe, mean shape of species---------

#choose a slice to work with
slice<-2

#subset dataset
slice2.data<-lm.2d[which(metadata$Slice_from_Base==2),]
slice2.metadata<-metadata[which(metadata$Slice_from_Base==2),]

#pick 2 species to compare in this example. For your work, compare the genera "Erethizon" and "Coendou"
sp.prehensilis<-which(slice2.metadata$Species=="prehensilis")
sp.dorsatum<-which(slice2.metadata$Species=="dorsatum")


#optional: reformat data from 2D n x pk matrix to 3D p x k x n array
slice2.3d.data<-arrayspecs(slice2.data,p=ncol(slice2.data)/2, k=2)


#find mean shape for each species
M.prehensilis <- mshape(slice2.3d.data[,,sp.prehensilis])
M.dorsatum <- mshape(slice2.3d.data[,,sp.dorsatum])

#look at shape differences associated with minimum and maximum in comparison to mean shape
plotRefToTarget(M.prehensilis, M.dorsatum, mag=1,method="vector")
plotRefToTarget(M.dorsatum,M.prehensilis, mag=1,method="vector")

