# Load Dependencies -----
library(geomorph)
library(dplyr)

scriptsdir <- "C://scripts/porcupine-teeth"
source(paste(scriptsdir,"/../observer-free-morphotype-characterization/find_repeatablePCs.R",sep=""))
source(paste(scriptsdir,"/calculate_error.R",sep=""))

iterations<-99 #bootstrap number, low for now, increase for final calcs


# Data Input ------
setwd("D://Dropbox/Documents/research/mammals/porcupines/dental_variation/Pilot_Dataset_3_Digitized")

#read in Procrustes Aligned coordinates
lm.raw<-readland.tps("digitized_aligned.tps")

#read in metadata
metadata.raw<-read.csv("Digitized_Porcupine_Dataset_3.csv")

#Remove fossils that will be studied in another paper
lm.3d<-lm.raw[,,-which(metadata.raw$Specimen%in%c("UF-UF-223810","UF-UF-121740"))]
metadata<-metadata[-which(metadata.raw$Specimen%in%c("UF-UF-223810","UF-UF-121740")),]

#make a single variable that identifies the "thing" being replicated
#(ex: the base of the m3 of FMNH-M-43289, not to be mixed up with the middle or top of the tooth)
metadata$Identity<-paste(metadata$Specimen,metadata$Molar,metadata$Slice_from_Base,sep="_")

#reformat/reproject shape data a couple of different ways
lm.2d<-two.d.array(lm.3d)
PCA.all<-prcomp(lm.2d, scale.=FALSE)

#Error Analysis -------
error_gdf<-geomorph.data.frame(coords=lm.2d, #before error has been calculated, shape should be quantified entire shape, either all PC scores or as the Procrustes-aligned coordinates
                             specimen=factor(metadata$Specimen),
                             identity=factor(metadata$Identity),
                             size=metadata$Centroid_Size,
                             side=factor(metadata$Side),
                             wear=factor(metadata$Slice_from_Base)
)

#Procrustes Regression
errorANOVA<-procD.lm(coords~identity,data=error_gdf,iter=iterations)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to size by keeping the same specimen but varying the size
avg.number.replicates<-nrow(metadata)/(unique(c(metadata$Identity)) %>% length)
#error3d and find repeatable PCs from homemade code
err<-error3d(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)


# Procrustes ANOVA




#Procrustes Regression for side
errorANOVA<-procD.lm(coords~specimen+side,data=macro_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to side by keeping the same specimen but varying the side
avg.number.replicates<-nrow(metadata)/(unique(c(metadata$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=3)
#append to result
error_df<-rbind(error_df, list('group'='macro', 'type'='side','repeatability'=err$repeatability, 'PME'=err$PME))

#Procrustes Regression for wear
errorANOVA<-procD.lm(coords~specimen+wear,data=macro_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to wear by keeping the same specimen but varying the wear
avg.number.replicates<-nrow(metadata)/(unique(c(metadata$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
#append to result
error_df<-rbind(error_df, list('group'='macro', 'type'='wear','repeatability'=err$repeatability, 'PME'=err$PME))

#Procrustes Regressions looking at effect of allometry
#does it matter that the two genera are distinctly different in body size?
allometryANOVA<-procD.lm(coords~genus*size,data=macro_gdf,iter=99)
allometryANOVA$aov.table

#just to check: does centroid size follow the prediction that Erethizon is larger than Coendou?
boxplot(metadata$Centroid_Size~metadata$Genus) #yes
