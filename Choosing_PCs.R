library(geomorph)
library(dplyr)
library(RColorBrewer)
library(caret)

setwd("E:\\Pilot_Dataset_3_Digitized")

#settings for classification following Courtenay et al. 2019
replicates=1000

lm.raw<-readland.tps("digitized_aligned.tps")
dim(lm.raw)
lm.2d<-two.d.array(lm.raw)
# metadata ---------
metadata<-read.csv("Digitized_Porcupine_Dataset_3.csv")
head(metadata)

nrow(lm.2d)==nrow(metadata) #check for same number of items in each dataset.

#using on a smaller dataset for working out the code
only.2<-which(metadata$Slice_from_Base==2)
lm.2<-lm.2d[only.2,]
metadata.2<-metadata[only.2,]
# classification example: formatting data ----------
#we will be analyzing PC scores of the landmarks themselves. Reduces the number of variables.
PCA<-lm.2d %>% prcomp(.,scale.=FALSE)

#we can't include all PCs in a predictive model unless we have many specimens (too many variables, over-fits)
#consider including only the PCs that account for 95% of the data, or some other subset of PCs
summary(PCA)

#put variable to be predicted (genus) and predictors (shape) in one object
classification.set<-data.frame(genus = as.factor(metadata$Genus),PCA$x)

#split the dataset: https://topepo.github.io/caret/data-splitting.html
set.seed(100)
inTrain<-createDataPartition(y=classification.set$genus, p=0.7, list=FALSE)

#create training and testing dataset. Validation will be carried out on the 'testing' dataset
#(remaining 20% of sample)
training<-classification.set[inTrain,]
testing<-classification.set[-inTrain,]

# variable selection -------------
###
# Recommended reading: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.192.6323&rep=rep1&type=pdf
###

#take a look at the first few PCs to see which ones are likely to be predictive
#three ways of plotting essentially the same information
#These are just exploratory plots. They should match statistical results later but you
#don't need these necessarily.
#these plots are all different ways of looking at the same thing
featurePlot(x = training[,2:ncol(classification.set)],  y = training$genus,plot="box")
featurePlot(x = training[,2:ncol(classification.set)],  y = training$genus)
featurePlot(x = training[,2:ncol(classification.set)],  y = training$genus,plot="pairs")

#A second optional step:
#recursive feature elimination below isn't completely applicable to PCs
#because they have to be used sequentially
#(for example: it's okay to use PCs 1-5, but not PCs 1, 5,6,7, and 10),
#but it will give us an indication of which PCs are likely to be important
#which will help inform whether we sample, say, all PCs or PCS 1-3 or PCs 1-14, etc.
#code copied from https://www.machinelearningplus.com/machine-learning/caret-package/
#see more at https://topepo.github.io/caret/recursive-feature-elimination.html#rfeexample
set.seed(100)
options(warn=-1)
# The simulation will fit models with subset sizes of 18, 15, 10, 5, 4, 3, 2, 1.
subsets <- c(1:15, 18, 20,24)

#rfeControl() and rfe() appear to be paired functions
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x=training[, 2:ncol(training)], y=training$genus,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile #do the results make sense based on our PC plots, linear model, and the diagnostic feature plot?

plot(lmProfile)
