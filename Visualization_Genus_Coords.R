library(geomorph)
library(dplyr)
library(RColorBrewer)
library(caret)

setwd("E:\\Pilot_Dataset_3_Digitized")

# landmark data
lm.raw<-readland.tps("digitized_aligned.tps")
lm.2d<-two.d.array(lm.raw)
# PCA ------
PCA<-prcomp(lm.2d, scale.=FALSE)
summary(PCA)
# metadata ---------
metadata<-read.csv("Digitized_Porcupine_Dataset_3.csv")

#take out the extincted specices for current analysis
extincted<-which(metadata$Specimen%in%c("UF-UF-21473","UF-UF-223810","UF-UF-121740"))
take_out<-c(extincted)
lm.complete<-lm.raw[,,-take_out]
metadata.complete<-metadata[-take_out,]

#make a geomorph data frame
genus_gdf<-geomorph.data.frame(coords=PCA$x[-take_out,1:25],
                               #coords=lm.complete,
                               genus=factor(metadata.complete$Genus)
                               )

#build Procrustes Regression with genus
linear.model<-procD.lm(coords~genus,data=genus_gdf,iter=99)
#summary statistics about the model
summary(linear.model)
# diagnostic plots
plot(linear.model, type = "diagnostics")

#set color scheme
#set colors using R Color Brewer Palettes
specimen.colors<-brewer.pal(n = 8, name = "Dark2")

plot(linear.model, type = "PC", pch = 19, col = specimen.colors[factor(metadata.complete$Genus)] )
mtext(side=3, line=0, at=0, adj=0, cex=1, "genus")
#a legend gives the colors meaning.
# legend('bottomright',legend=levels(factor(metadata.complete$Genus)),col=specimen.colors,pch=19)
