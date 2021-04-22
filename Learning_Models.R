library(geomorph)
library(caret)
library(dplyr)
library(ggfortify)

setwd("E:\\Pilot_Dataset_3_Digitized")

#settings for classification following Courtenay et al. 2019
replicates=1000

lm.raw<-readland.tps("digitized_aligned.tps")
dim(lm.raw)
lm.2d<-two.d.array(lm.raw)

metadata<-read.csv("Digitized_Porcupine_Dataset_3.csv")
metadata$Genus <- factor(metadata$Genus)
head(metadata)

PCA<-prcomp(lm.2d, scale.=FALSE)
#summary(PCA)
autoplot(PCA, data=metadata, colour='Genus')
metadata[which(PCA$x[,1]< -0.05 & PCA$x[,2]< -0.1),]
#using only the first 25PCs
pca_matrix <- PCA$x[,1:25]

total_data<-data.frame(Genus=metadata$Genus, pca_matrix)
#head(total_data)
dim(total_data)
#214  26

#splitting the extincted specimens out
extincted.id<-which(metadata$Specimen%in%c("UF-UF-21473","UF-UF-223810",
                                           "UF-UF-121740"))
extincted.testing <- total_data[extincted.id,]
model_data<-total_data[-extincted.id,]
#head(model_data)

#Randomly selects p% of all samples
set.seed(107)

inTrain <- createDataPartition(
  y =model_data$Genus,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

#Setting up the training and testing gdf
training <- model_data[ inTrain,]
testing  <- model_data[-inTrain,]
dim(training)
dim(testing)
dim(extincted.testing)
head(training)

#change evaluation to k-fold cross-validation ['repeatedcv'] instead of default bootstrapping
ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

#-------------------------Partial least squares regression---------------------------------------
#Training using PLS method
set.seed(123)
plsFit <- train(
  Genus~ .,
  data = training,
  method = "pls",
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl,
  metric = "ROC"
)

plsFit
ggplot(plsFit)

#Testing on test set
plsClasses <- predict(plsFit, newdata = testing)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
#looking at which wrong are incorrectly predicted
confusionMatrix(data = plsClasses, testing$Genus)
wrong_pred <- which(plsClasses!=testing$Genus)
metadata[-c(extincted.id,inTrain),][wrong_pred,]
unique(metadata[-c(extincted.id,inTrain),][wrong_pred,]$Specimen)

#Testing on extinct set
plsClasses <- predict(plsFit, newdata = extincted.testing)
plsProbs <- predict(plsFit, newdata = extincted.testing, type = "prob")

#looking at which wrong are incorrectly predicted
confusionMatrix(data = plsClasses, extincted.testing$Genus)
wrong_pred <- which(plsClasses!=extincted.testing$Genus)
raw_id <- extincted.id[wrong_pred]
metadata[raw_id,]
unique(metadata[raw_id,]$Specimen)

#-------------------------Regularized Discriminant Analysis---------------------------------------
#Training using rda method
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(
  Genus ~ .,
  data = training,
  method = "rda",
  tuneGrid = rdaGrid,
  trControl = ctrl,
  metric = "ROC"
)
rdaFit

#Tesing rda model with testing data
rdaClasses <- predict(rdaFit, newdata = testing)
rdaProbs <- predict(rdaFit, newdata = testing, type = "prob")
confusionMatrix(rdaClasses, testing$Genus)
#check which ones gives wrong predictions
wrong_pred <- which(rdaClasses!=testing$Genus)
metadata[-c(extincted.id,inTrain),][wrong_pred,]
unique(metadata[-c(extincted.id,inTrain),][wrong_pred,]$Specimen)

#Tesing rda model with extinct data
rdaClasses <- predict(rdaFit, newdata = extincted.testing)
rdaProbs <- predict(rdaFit, newdata = extincted.testing, type = "prob")
#looking at which wrong are incorrectly predicted
confusionMatrix(data = rdaClasses, extincted.testing$Genus)
wrong_pred <- which(rdaClasses!=extincted.testing$Genus)
raw_id <- extincted.id[wrong_pred]
metadata[raw_id,]
unique(metadata[raw_id,]$Specimen)

#-------------------------Random Forest---------------------------------------
#Training using RF method
rfFit <- train(
  Genus ~ ., #model: predict genus using the rest of the variables
  data = training,
  method = "rf",
  tuneLength=15, #can change to 15 and see what happens
  trControl = ctrl,
) #implement the ctrl evaluation settings specified above
rfFit
#compoent gives optimal when=4?
ggplot(rfFit)

#Tesing rf model with testing data
rfClasses <- predict(rfFit, newdata = testing)
rfProbs <- predict(rfFit, newdata = testing, type = "prob")

#looking at which wrong are incorrectly predicted
confusionMatrix(rfClasses, testing$Genus)
wrong_pred <- which(rfClasses!=testing$Genus)
metadata[-c(extincted.id,inTrain),][wrong_pred,]
unique(metadata[-c(extincted.id,inTrain),][wrong_pred,]$Specimen)

#Tesing rf model with extinct data
rfClasses <- predict(rfFit, newdata = extincted.testing)
rfProbs <- predict(rfFit, newdata = extincted.testing, type = "prob")

#looking at which wrong are incorrectly predicted
confusionMatrix(data = rfClasses, extincted.testing$Genus)
wrong_pred <- which(rfClasses!=extincted.testing$Genus)
raw_id <- extincted.id[wrong_pred]
metadata[raw_id,]
unique(metadata[raw_id,]$Specimen)

#-------------------------Suppor Vector Machine---------------------------------------
#svm learning model setup
svmFit <- train(
  Genus ~ ., #model: predict genus using the rest of the variables
  data = training,
  method = "svmRadial",
  tuneLength=10, #can change to 15 and see what happens
  trControl = ctrl,
) #implement the ctrl evaluation settings specified above
svmFit
ggplot(svmFit)

#Tesing svm on test set
svmClasses<-predict(svmFit, newdata = testing) #example of possibilities for regression instead of categorical
svmProbs <- predict(svmFit, newdata = testing, type = "prob")

#looking at which wrong are incorrectly predicted
confusionMatrix(svmClasses, testing$Genus)
wrong_pred <- which(svmClasses!=testing$Genus)
metadata[-c(extincted.id,inTrain),][wrong_pred,]
unique(metadata[-c(extincted.id,inTrain),][wrong_pred,]$Specimen)

#Tesing svm on extinct set
svmClasses<-predict(svmFit, newdata = extincted.testing) #example of possibilities for regression instead of categorical
svmProbs <- predict(svmFit, newdata = extincted.testing, type = "prob")

#looking at which wrong are incorrectly predicted
confusionMatrix(data = svmClasses, extincted.testing$Genus)
wrong_pred <- which(svmClasses!=extincted.testing$Genus)
raw_id <- extincted.id[wrong_pred]
metadata[raw_id,]
unique(metadata[raw_id,]$Specimen)
