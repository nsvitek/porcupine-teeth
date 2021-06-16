freeze<-ls() #take stock of what objects are already here.
# classification: formatting data ----------
#we will be analyzing PC scores of the landmarks themselves. Reduces the number of variables.
PCA.extant<-lm.2d.extant %>% prcomp(.,scale.=FALSE)

#we can't include all PCs in a predictive model unless we have many specimens (too many variables, over-fits)
#consider including only the PCs that account for 95% of the data, or some other subset of PCs
summary(PCA.extant) #PC16 (rounding up) or PC17 get to 95%
summary(PCA) #inclusion/exclusion of fossil doesn't make a difference 

#put variable to be predicted (genus) and predictors (shape) in one object
classification.set<-data.frame(genus = as.factor(metadata.extant$Genus),PCA.extant$x)

# split the dataset -----------------
#https://topepo.github.io/caret/data-splitting.html
set.seed(100)

#because slices within specimens are not independent from another, need to randomly sample at the level of specimens, not slices
train.specimen<-factor(metadata.extant$Specimen) %>% levels() %>% 
  as.data.frame(.) %>% sample_frac(0.7)
inTrain<-which(metadata.extant$Specimen %in% train.specimen$.) 

#create training and testing dataset. Validation will be carried out on the 'testing' dataset
training.variables<-classification.set[inTrain,]
testing.variables<-classification.set[-inTrain,]

nrow(training.variables)
nrow(testing.variables)
# variable selection -------------
###
# Recommended reading: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.192.6323&rep=rep1&type=pdf
###

#take a look at the first few PCs to see which ones are likely to be predictive
#three ways of plotting essentially the same information
#These are just exploratory plots. They should match statistical results later but you
#don't need these necessarily.
#these plots are all different ways of looking at the same thing
featurePlot(x = training.variables[,2:ncol(classification.set)],  y = training.variables$genus,plot="box")
featurePlot(x = training.variables[,21:30],  y = training.variables$genus,plot="box")

# featurePlot(x = training.variables[,2:ncol(classification.set)],  y = training.variables$genus)
# featurePlot(x = training.variables[,2:ncol(classification.set)],  y = training.variables$genus,plot="pairs")
featurePlot(x = training.variables[,2:12],  y = training.variables$genus,plot="pairs")
featurePlot(x = training.variables[,12:21],  y = training.variables$genus,plot="pairs")

#initial observation: PC2, PC7, maybe PC17?

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
subsets <- c(1:25) #went up to c(1:50)
#rfeControl() and rfe() appear to be paired functions
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x=training.variables[, 2:ncol(training.variables)], y=training.variables$genus,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile #do the results make sense based on our PC plots, linear model, and the diagnostic feature plot?

plot(lmProfile)


#Overall:
#Top 3 variables within PCs 1-17, which covers 95% of the variation. Matches visual inspection. 
#Top 5 includes PC22
#accuracy and kappa increase to a maximum accuracy of .9286 at PC7 (85% of the dataset) when up to 25 variables tested
#when up to 50 variables, highest accuracy shifted to PC12
#PCs 1-17 captures the top 4 PCs, 95% of the data, and minimizes # of variables to help prevent overfitting
#on balance, seems the best choice. 
PCs<-c(1:17)

#remove everythign except PCs
rm(list = setdiff(ls(), c(freeze,"PCs")))