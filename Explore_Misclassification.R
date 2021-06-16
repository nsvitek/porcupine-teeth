# Look at low performing specific examples
which(all.eval$Kappa<=0)

which(all.eval$Kappa<=0.55 & all.eval$Kappa>=0.45)
# > which(all.eval$Kappa<=0)
# [1]   2  50  98 102 126 157 165 196 202 250 253 298

# > which(all.eval$Kappa<=0.55 & all.eval$Kappa>=0.45)
# [1]  11  13  19  21  29  37  41  46  47  63  67  69  79  82  83  88  89  92  94 104 120 121 123 129
# [25] 145 146 158 183 186 188 193 195 199 200 206 212 219 222 227 241 247 251 260 267 279 280 287 288
# [49] 289 291 294
# ------------------seed 200 consistently poor, look at that one --------
start.seed<-200
set.seed(start.seed)
#because slices within specimens are not independent from another, need to randomly sample at the level of specimens, not slices
train.specimen<-factor(metadata.extant$Specimen) %>% levels() %>% 
  as.data.frame(.) %>% sample_frac(0.75)

#specify which images go in the training set
inTrain<-which(metadata.avg$Specimen %in% train.specimen$.) 

#make the 3 datasets
training <- model_data[ inTrain,]
testing  <- model_data[-c(inTrain,extinct),]

#Linear Discriminant Analysis---------------------------------------
#Training using lda method
set.seed(start.seed)
ldaFit <- train(
  genus ~ .,
  data = training,
  method = "lda",
  trControl = ctrl,
  metric = performance_metric
)

#Random Forest---------------------------------------
#Training using RF method
#post-hoc note: after trying the manual grid search the model produced seemed even worse than the automated model
# tunegrid <- expand.grid(.mtry = c(1:10)) #or mtry
set.seed(start.seed)
rfFit <- train(
  genus ~ ., #model: predict genus using the rest of the variables
  data = training,
  method = "rf",
  tuneLength=10,
  metric = performance_metric,
  trControl = ctrl
  # tuneGrid=tunegrid,
) 

#Support Vector Machine---------------------------------------
#svm learning model setup
#tune grid values chosen after automated/default search that chose same sets of C and held sigma at 0.0382
#note: trying out the grid produced a terrible model. Went back to the automated system which produced a better model
# tunegrid.svm <- expand.grid(sigma=c(0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,5.12), 
#                             C = c(0.25,0.5,1,2,4,8,16,32,64,128))
set.seed(start.seed)
svmFit <- train(
  genus ~ ., #model: predict genus using the rest of the variables
  data = training,
  method = "svmRadial",
  tuneLength=10, 
  trControl = ctrl,
  # tuneGrid = tunegrid.svm,
  metric = performance_metric
) 

# Evaluate LDA --------------------------
#Evaluating lda model with testing data
ldaClasses <- predict(ldaFit, newdata = testing)

# #check which ones gives wrong predictions
wrong_pred <- which(ldaClasses!=testing$genus)
metadata.avg[-c(extinct,inTrain)][wrong_pred]

# Evaluate RF -------
#Tesing rf model with testing data
rfClasses <- predict(rfFit, newdata = testing)

# #looking at which wrong are incorrectly predicted
wrong_pred <- which(rfClasses!=testing$genus)
metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]

# Evaluate SVM ------
#Tesing svm on test set
svmClasses<-predict(svmFit, newdata = testing) #example of possibilities for regression instead of categorical


# #looking at which wrong are incorrectly predicted
wrong_pred <- which(svmClasses!=testing$genus)
metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]




# -----------------seed 88 consistently middling, look at that one --------
start.seed<-100+100*88
set.seed(start.seed)
#because slices within specimens are not independent from another, need to randomly sample at the level of specimens, not slices
train.specimen<-factor(metadata.extant$Specimen) %>% levels() %>% 
  as.data.frame(.) %>% sample_frac(0.75)

#specify which images go in the training set
inTrain<-which(metadata.avg$Specimen %in% train.specimen$.) 

#make the 3 datasets
training <- model_data[ inTrain,]
testing  <- model_data[-c(inTrain,extinct),]

#Linear Discriminant Analysis---------------------------------------
#Training using lda method
set.seed(start.seed)
ldaFit <- train(
  genus ~ .,
  data = training,
  method = "lda",
  trControl = ctrl,
  metric = performance_metric
)

#Random Forest---------------------------------------
#Training using RF method
#post-hoc note: after trying the manual grid search the model produced seemed even worse than the automated model
# tunegrid <- expand.grid(.mtry = c(1:10)) #or mtry
set.seed(start.seed)
rfFit <- train(
  genus ~ ., #model: predict genus using the rest of the variables
  data = training,
  method = "rf",
  tuneLength=10,
  metric = performance_metric,
  trControl = ctrl
  # tuneGrid=tunegrid,
) 

#Support Vector Machine---------------------------------------
#svm learning model setup
#tune grid values chosen after automated/default search that chose same sets of C and held sigma at 0.0382
#note: trying out the grid produced a terrible model. Went back to the automated system which produced a better model
# tunegrid.svm <- expand.grid(sigma=c(0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,5.12), 
#                             C = c(0.25,0.5,1,2,4,8,16,32,64,128))
set.seed(start.seed)
svmFit <- train(
  genus ~ ., #model: predict genus using the rest of the variables
  data = training,
  method = "svmRadial",
  tuneLength=10, 
  trControl = ctrl,
  # tuneGrid = tunegrid.svm,
  metric = performance_metric
) 

# Evaluate LDA --------------------------
#Evaluating lda model with testing data
ldaClasses <- predict(ldaFit, newdata = testing)

# #check which ones gives wrong predictions
wrong_pred <- which(ldaClasses!=testing$genus)
metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]

# Evaluate RF -------
#Tesing rf model with testing data
rfClasses <- predict(rfFit, newdata = testing)

# #looking at which wrong are incorrectly predicted
wrong_pred <- which(rfClasses!=testing$genus)
metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]

# Evaluate SVM ------
#Tesing svm on test set
svmClasses<-predict(svmFit, newdata = testing) #example of possibilities for regression instead of categorical


# #looking at which wrong are incorrectly predicted
wrong_pred <- which(svmClasses!=testing$genus)
metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]


