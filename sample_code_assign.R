library(geomorph)
library(dplyr)
library(RColorBrewer)
library(caret)

setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/12102020-Pilot_Dataset_3_Digitized/")

#settings for classification following Courtenay et al. 2019
replicates=1000

lm.raw<-readland.tps("digitized_aligned.tps") 
dim(lm.raw) 
lm.2d<-two.d.array(lm.raw)
# metadata ---------
metadata<-read.csv("Porcupine Mandible Full Dataset - 3 - PorcupineDataset3_SliceFromBaseData.csv")
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

#Let's say we started out by using the first 6 PCs. Do the results of lm profile indicate that PC6 is important?
#how about PC5? etc.

#Done choosing variables. Now apply choice by subsetting dataset
training.sub<-classification.set[inTrain,c(1:15)]
testing.sub<-classification.set[-inTrain,c(1:15)]


# algorithm: Random Forest -------
#Now, choose different algorithms, build models of tooth genus using each algorithm, and compare models.
#the models we aim to try are SVM (Support Vector Machine) and RF (Random Forest)
#(the two consistently optimal in Courteny et al. 2019 and Puschel et al. 2018). Below are examples with RF. 

#change evaluation to k-fold cross-validation ['repeatedcv'] instead of default bootstrapping
ctrl <- trainControl(method = "repeatedcv", repeats = 10)
#quick intro to k-fold cross-validation: https://machinelearningmastery.com/k-fold-cross-validation/

#build training model
#if you wanted to use random forest (RF):
rfFit <- train(
  genus ~ ., #model: predict genus using the rest of the variables
  data = training.sub,
  method = "rf",
  tuneLength=10, #can change to 15 and see what happens
  trControl = ctrl) #implement the ctrl evaluation settings specified above

plot(rfFit)
#another example of training a random forest model: https://rpubs.com/phamdinhkhanh/389752


#use model to predict values for testing dataset (the 25% left out to help evaluate model performance)
rfClasses<-predict(rfFit, newdata = testing.sub) #example of possibilities for regression instead of categorical

#how do predicted values compare to real values?
cbind(rfClasses, testing.sub$genus)

#in Courtenay et al. 2019 sensitivity = type 1 error; specificity = type 2 error (p. 31 bottom right)

#another way of looking at reults:
#compare frequencies of mismatch between predicted vs. real classification: Confusion matrix
conf.mat<-confusionMatrix(data = rfClasses, factor(testing$genus)) #only for factored data

#this code copied and pasted as a way to visualize a confusion matrix: 
#plot shows how often right category was predicted and frequency of types of mis-identifications
conf.mat$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = levels(factor(classification.set$genus)))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  ggplot(aes(Prediction, Reference, fill = frac_fill)) +
  geom_tile() +
  geom_text(aes(label = paste(Freq, ", ", round(frac * 100), "%")), size = 6) +
  scale_fill_gradient(low = "white", high = "#badb33") +
  scale_x_discrete(position = "top") +
  geom_tile(color = "black", fill = "black", alpha = 0)

# algorithm: Support Vector Machine -------
# a little more about SVM: http://www.sthda.com/english/articles/36-classification-methods-essentials/144-svm-model-support-vector-machine-essentials/
# another explanation here: https://towardsdatascience.com/support-vector-machine-simply-explained-fee28eba5496
svmFit <- train(
  genus ~ ., #model: predict genus using the rest of the variables
  data = training.sub,
  method = "svmRadial",
  tuneLength=10, #can change to 15 and see what happens
  trControl = ctrl) #implement the ctrl evaluation settings specified above

plot(svmFit)

svmClasses<-predict(svmFit, newdata = testing.sub) #example of possibilities for regression instead of categorical

#how do predicted values compare to real values?
cbind(svmClasses, testing.sub$genus)

#in Courtenay et al. 2019 sensitivity = type 1 error; specificity = type 2 error (p. 31 bottom right)

#another way of looking at reults:
#compare frequencies of mismatch between predicted vs. real classification: Confusion matrix
conf.mat<-confusionMatrix(data = svmClasses, factor(testing$genus)) #only for factored data

conf.mat$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = levels(factor(classification.set$genus)))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  ggplot(aes(Prediction, Reference, fill = frac_fill)) +
  geom_tile() +
  geom_text(aes(label = paste(Freq, ", ", round(frac * 100), "%")), size = 6) +
  scale_fill_gradient(low = "white", high = "#66ccee") +
  scale_x_discrete(position = "top") +
  geom_tile(color = "black", fill = "black", alpha = 0)
