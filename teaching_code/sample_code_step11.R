library(geomorph)
library(dplyr)
library(RColorBrewer)
library(caret)

setwd("D:/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")
# setwd("C:/Users/N.S/Dropbox/Documents/research/mammals/porcupines/dental_variation/07192020-Porcupine_Project_Pilot_2/Combined_Digitized")

#set color scheme 
specimen.colors<-brewer.pal(n = 8, name = "Dark2") #set colors using R Color Brewer Palettes


lm.raw<-readland.tps("digitized_aligned.tps") 
dim(lm.raw) 
lm.2d<-two.d.array(lm.raw)
# PCA ------
PCA<-prcomp(lm.2d, scale.=FALSE)

#How much variation is explained by each axis?
summary(PCA.complete)

# metadata ---------
metadata<-read.csv("PorcupineMetadataComplete.csv")
head(metadata)

#note one specimen is broken on one half, potentially influencing results: 
broken<-which(metadata$Specimen=="UF-M-7993" & metadata$Slice_from_Base==3 & metadata$Reposition_Replicate==0)
lm.complete<-lm.raw[,,-broken]
metadata.complete<-metadata[-broken,]

# Step 11 draft linear model, results---------
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
plot(linear.model, type = "PC", pch = 19, col = specimen.colors[factor(metadata$Slice_from_Base)] )

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
legend('topleft',legend = levels(factor(metadata.complete$Specimen)),col=c("red","blue","green"),pch=16)
text(PCA.residual$x[,1],PCA.residual$x[,2],labels=substr(metadata.complete$File_Name,30,44))

head(metadata.complete)

#Visualize shapes
#Another approach to achieve the same kind of PCA, using functions from the geomorph package
#First, add mean shape back to residuals for later plotting. 
#This next line of code is not elegant or recommended but it gets the job done.
remaining.shape.vis<-remaining.shape
for(i in 1:dim(remaining.shape)[3]){ remaining.shape.vis[,,i]<-remaining.shape[,,i] + M}

# use geomorph PCA to look at residual shape -------
#use geomorphs PCA
PCA.residuals2<-gm.prcomp(remaining.shape.vis)

#plot PCA plot
plot(PCA.residuals2)
plot(PCA.residuals2,col=c("red","blue","green")[factor(metadata.complete$Specimen)],pch=metadata.complete$Slice_from_Base)
legend('topleft',legend = levels(factor(metadata.complete$Specimen)),col=c("red","blue","green"),pch=16)

#shape predicted by minimum PC1 value, arrows point to mean shape. mshape() function calculates mean shape.
plotRefToTarget(PCA.residuals2$shapes$shapes.comp1$min, mshape(remaining.shape), 
                method = "vector")
#shape predicted by maximum PC1 value, same setup
plotRefToTarget(PCA.residuals2$shapes$shapes.comp1$max, mshape(remaining.shape), 
                method = "vector")

#compare maximum (points) to minimum (arrow)
plotRefToTarget(PCA.residuals2$shapes$shapes.comp1$max, PCA.residuals2$shapes$shapes.comp1$min, 
                method = "vector")

dim(remaining.shape.vis)
plot(remaining.shape.vis[,,3])

#figure out which rows ( = specimens) have maximum PC2 values: those are probably the blue squares
str(PCA.residuals2)
which(PCA.residuals2$x[,2]>=0.06 & PCA.residuals2$x[,1]>=0.05)
#blue squares are specimen rows 16 20 24 51 55 59

#this is an example of a shape and a file that occupy upper right quadrant of graph
plot(remaining.shape.vis[,,16], pch=21, bg="red")
metadata.complete$File_Name[16]

# Q1 # ------
#### what is the shape "gap" between slice 0 and slice 1 in all three specimen? Why is there such a big gap?
#sub-question 1: what is the "residual shape" of slice 1 and 2 for UF-M-23235? 
#do residual shapes *look* more similar to each other than original shapes?
which(metadata.complete$Specimen=="UF-M-23235" & metadata.complete$Slice_from_Base_Original==2) %>%
  metadata.complete$File_Name[.]

#equivalent formation:
any.variable<-which(metadata.complete$Specimen=="UF-M-23235" & metadata.complete$Slice_from_Base_Original==2)
metadata.complete$File_Name[any.variable]
# shape for slice 2: 15 19 23 50 54 58

which(metadata.complete$Specimen=="UF-M-23235" & metadata.complete$Slice_from_Base_Original==1)
# shape for slice 1: 14 18 22 49 53 57

plot(remaining.shape.vis[,,14], pch=21, bg="blue")
plot(remaining.shape.vis[,,15], pch=21, bg="lightblue")

# Q2 # -------
#### why is the order reversed between the two set of specimens? 

#so what is the shape in the lower right quadrant?
which(PCA.residuals2$x[,2]>=0.06 & PCA.residuals2$x[,1]>=0.05)


# Step 11 draft classification sample ----------
#we will be analyzing PC scores of the landmarks themselves. Reduces the number of variables.
PCA.complete<-two.d.array(lm.complete) %>% prcomp(.,scale.=FALSE)

#we can't include all PCs in a predictive model unless we have many specimens (too many variables, over-fits)
#consider including only the PCs that account for 95% of the data, or some other subset of PCs
summary(PCA.complete)

#put variable to be predicted (slice from base, or wear stage) and predictors (shape) in one object
classification.set<-data.frame(wear = metadata.complete$Slice_from_Base,PCA.complete$x[,1:6])

#split the dataset: https://topepo.github.io/caret/data-splitting.html
set.seed(100)
inTrain<-createDataPartition(y=classification.set$wear, p=0.75, list=FALSE)

#create training and testing dataset. Validation will be carried out on the 'testing' dataset
#(remaining 25% of sample)
training<-classification.set[inTrain,]
testing<-classification.set[-inTrain,]

#take a look at the first few PCs to see which ones are likely to be predictive
#three ways of plotting essentially the same information
#These are just exploratory plots. They should match statistical results later but you 
#don't need these necessarily.
featurePlot(x = training[,2:ncol(classification.set)],  y = training$wear)

featurePlot(x = training[,2:ncol(classification.set)],  y = training$wear,plot="pairs")

featurePlot(x = training[,2:ncol(classification.set)],  y = training$wear, 
            plot="pairs", groups=training$wear)

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
subsets <- c(1:5, 10, 15, 18)

#rfeControl() and rfe() appear to be paired functions
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x=training[, 2:ncol(training)], y=training$wear,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile #do the results make sense based on our PC plots, linear model, and the diagnostic feature plot?
#Let's say we started out by using the first 6 PCs. Do the results of lm profile indicate that PC6 is important?
#how about PC5? etc.

#Done choosing variables. 
#Now, choose different algorithms, build models of tooth wear using each algorithm, and compare models.
#the models we will try here are 'lda','rf'

#first, linear discriminant analysis, the classic. 
#change evaluation to k-fold cross-validation ['repeatedcv'] instead of default bootstrapping
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
#quick intro to k-fold cross-validation: https://machinelearningmastery.com/k-fold-cross-validation/

#build training model
#to do LDA, "wear" must be factored into a categorical variable.
ldaFit <- train(
  factor(wear) ~ . , #model: predict wear using the rest of the variables
  data = training,
  method = "lda",
  tuneLength = 10, #try different values of k from 1 to 10
  trControl = ctrl #implement the ctrl evaluation settings specified above
)

#if you wanted to use random forest instead of LDA:
#factoring is not necessary for random forest or SVM
rfFit <- train(
  wear ~ ., #model: predict wear using the rest of the variables
  data = training,
  method = "rf",
  tuneLength=10,
  trControl = ctrl) #implement the ctrl evaluation settings specified above

#use model to predict values for testing dataset (the 25% left out to help evaluate model performance)
ldaProbs <- predict(ldaFit, newdata = testing, type = "prob") #only for factored data
ldaClasses <- predict(ldaFit, newdata = testing) 

rfClasses<-predict(rfFit, newdata = testing) #example of possibilities for regression instead of categorical

#how do predicted values compare to real values?
cbind(ldaProbs, ldaClasses, testing$wear)
cbind(rfClasses, testing$wear)

#another way of looking at reults:
#compare frequencies of mismatch between predicted vs. real classification: Confusion matrix
conf.mat<-confusionMatrix(data = ldaClasses, factor(testing$wear)) #only for factored data

#this code copied and pasted as a way to visualize a confusion matrix: 
#plot shows how often right category was predicted and frequency of types of mis-identifications
conf.mat$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = levels(factor(classification.set$wear)))) %>%
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

