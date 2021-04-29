library(geomorph)
library(dplyr)

setwd("E:\\Pilot_Dataset_3_Digitized")

lm.raw<-readland.tps("digitized_aligned.tps")
lm.2d<-two.d.array(lm.raw)

PCA<-prcomp(lm.2d, scale.=FALSE)

metadata<-read.csv("Digitized_Porcupine_Dataset_3.csv")
#make a single variable that identifies the "thing" being replicated
#(ex: the base of the m3 of FMNH-M-43289, not to be mixed up with the middle or top of the tooth)
metadata$Identity<-paste(metadata$Specimen,metadata$Molar,
                         metadata$Slice_from_Base,
                         sep="_"
)


#Error Analysis -------
#The function as currently written:
#used with output of procD.lm
#r are the number of replicates

errorGM<-function(ANOVA,r,f1=1,f2=2){ #f1 and f2 are which factors you want to compare
  #default f1 and f2 assume only one level in ANOVA, not nested
  #Fruciano 2016 repeatibility
  #note also that f1 should be the between-individual component
  #and f2 should be the within-individual component
  s.among<-(ANOVA$MS[f1]-ANOVA$MS[f2])/r
  repeatability<-s.among/(ANOVA$MS[f2]+s.among)
  # Yezerinac et al. 1992 p. 474 % measurement error
  percent_measurement_error<-(ANOVA$MS[f2]/(ANOVA$MS[f2]+s.among))*100
  result<-list(repeatability,percent_measurement_error)
  names(result)<-c("repeatability","PME")
  return(result)
}

error_df <- data.frame()
#------------------------------------- MACRO ---------------------------
macro_gdf<-geomorph.data.frame(coords=lm.2d, #before error has been calculated, shape should be quantified entire shape, either all PC scores or as the Procrustes-aligned coordinates
                             specimen=factor(metadata$Specimen),
                             identity=factor(metadata$Identity),
                             size=metadata$Centroid_Size, #"specimen" and "size were both mapped to Identity. Do you want Size here?
                             side=factor(metadata$Side),
                             wear=factor(metadata$Slice_from_Base),
                             genus=factor(metadata$Genus)
)
#Procrustes Regression
errorANOVA<-procD.lm(coords~specimen,data=macro_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to size by keeping the same specimen but varying the size
avg.number.replicates<-nrow(metadata)/(unique(c(metadata$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
#append to result
error_df<-rbind(error_df, list('group'='macro', 'type'='size','repeatability'=err$repeatability, 'PME'=err$PME))

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

#------------------------------------- Coendou ---------------------------
#constructing geomorph data frame for coendou
coendou_id <- which(metadata$Genus == "Coendou")
coendou_gdf<-geomorph.data.frame(coords=PCA$x[coendou_id,1:25],
                             specimen=factor(metadata[coendou_id,]$Identity),
                             size=metadata[coendou_id,]$Centroid_Size,
                             side=factor(metadata[coendou_id,]$Side),
                             wear=factor(metadata[coendou_id,]$Slice_from_Base)
)
#subsetting metadata df for only coendous
coendou_data <- metadata[coendou_id, ]

#Procrustes Regression for size
allometryANOVA<-procD.lm(coords~specimen+size,data=coendou_gdf,iter=99)
#Procrustes ANOVA
allometryANOVA$aov.table
#error on landmarking due to size by keeping the same specimen but varying the size
avg.number.replicates<-nrow(coendou_data)/(unique(c(coendou_data$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
#append to result
error_df<-rbind(error_df, list('group'='coendou', 'type'='size','repeatability'=err$repeatability, 'PME'=err$PME))

#Procrustes Regression for size
errorANOVA<-procD.lm(coords~specimen+side,data=coendou_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to side by keeping the same specimen but varying the side
avg.number.replicates<-nrow(coendou_data)/(unique(c(coendou_data$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=3)
#append to result
error_df<-rbind(error_df, list('group'='coendou', 'type'='side','repeatability'=err$repeatability, 'PME'=err$PME))

#Procrustes Regression for wear
errorANOVA<-procD.lm(coords~specimen+wear,data=coendou_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to wear by keeping the same specimen but varying the wear
avg.number.replicates<-nrow(coendou_data)/(unique(c(coendou_data$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
#append to result
error_df<-rbind(error_df, list('group'='coendou', 'type'='wear','repeatability'=err$repeatability, 'PME'=err$PME))

#------------------------------------- Erethizon ---------------------------
#constructing geomorph data frame for Erethizon
erethizon_id <- which(metadata$Genus == "Erethizon")
erethizon_gdf<-geomorph.data.frame(coords=PCA$x[erethizon_id,1:25],
                                 specimen=factor(metadata[erethizon_id,]$Identity),
                                 size=metadata[erethizon_id,]$Identity,
                                 side=factor(metadata[erethizon_id,]$Side),
                                 wear=factor(metadata[erethizon_id,]$Slice_from_Base)
                                 )

#subsetting metadata df for only erethizon
erethizon_data <- metadata[erethizon_id, ]

#Procrustes Regression for size
errorANOVA<-procD.lm(coords~specimen+size,data=erethizon_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to size by keeping the same specimen but varying the size
avg.number.replicates<-nrow(erethizon_data)/(unique(c(erethizon_data$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
#append to result
error_df<-rbind(error_df, list('group'='erethizon', 'type'='size','repeatability'=err$repeatability, 'PME'=err$PME))

#Procrustes Regression for side
errorANOVA<-procD.lm(coords~specimen+side,data=erethizon_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to side by keeping the same specimen but varying the side
avg.number.replicates<-nrow(erethizon_data)/(unique(c(erethizon_data$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
#append to result
error_df<-rbind(error_df, list('group'='erethizon', 'type'='side','repeatability'=err$repeatability, 'PME'=err$PME))

#Procrustes Regression for wear
errorANOVA<-procD.lm(coords~specimen+wear,data=erethizon_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
#error on landmarking due to wear by keeping the same specimen but varying the wear
avg.number.replicates<-nrow(erethizon_data)/(unique(c(erethizon_data$Identity)) %>% length)
err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)
#append to result
error_df<-rbind(error_df, list('group'='erethizon', 'type'='wear','repeatability'=err$repeatability, 'PME'=err$PME))

#Displaying overall result
error_df

#Procrustes Regression for genus
errorANOVA<-procD.lm(coords~genus,data=macro_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table

#Procrustes Regression for genus
errorANOVA<-procD.lm(coords~genus,data=macro_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table

#Procrustes Regression for size,side, and wear
#note: wear is not showing up
errorANOVA<-procD.lm(coords~size+side+wear,data=macro_gdf,iter=99)
#Procrustes ANOVA
errorANOVA$aov.table
