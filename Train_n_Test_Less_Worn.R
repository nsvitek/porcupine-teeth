for(j in 1:remodel){
  # create datasets --------
  set.seed(start.seed)
  
  
  #because slices within specimens are not independent from another, need to randomly sample at the level of specimens, not slices
  train.specimen<-factor(metadata.avg$Specimen) %>% levels() %>% 
    as.data.frame(.) %>% sample_frac(0.75)
  
  #specify which images go in the training set
  inTrain<-which(metadata.avg$Specimen %in% train.specimen$.) 
  
  #make the 3 datasets
  training <- model_data[ inTrain,]
  testing  <- model_data[-c(inTrain),]
  
  lda.total.eval$N_train[i]<-rf.total.eval$N_train[i]<-svm.total.eval$N_train[i]<-nrow(training)
  lda.total.eval$N_test[i]<-rf.total.eval$N_test[i]<-svm.total.eval$N_test[i]<-nrow(testing)
  
  #-------------------------Linear Discriminant Analysis---------------------------------------
  #Training using lda method
  set.seed(start.seed)
  ldaFit <- train(
    genus ~ .,
    data = training,
    method = "lda",
    trControl = ctrl,
    metric = performance_metric
  )
  
  #-------------------------Random Forest---------------------------------------
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
  # rfFit
  # ggplot(rfFit)
  
  #-------------------------Support Vector Machine---------------------------------------
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
  
  # svmFit
  # ggplot(svmFit)
  
  # Evaluate LDA --------------------------
  #Evaluating lda model with testing data
  ldaClasses <- predict(ldaFit, newdata = testing)
  ldaProbs <- predict(ldaFit, newdata = testing, type = "prob")
  ldaConfusion<-confusionMatrix(ldaClasses, testing$genus)
  
  #pull data needed to get AUC
  ldaTogether<-cbind(obs=testing$genus,pred=ldaClasses,ldaProbs)
  
  #Results LDA
  lda.total.confusion[,,i]<-ldaConfusion$table #the actual confusion matrix
  lda.total.eval$Accuracy[i]<-ldaConfusion$overall[1]
  lda.total.eval$Kappa[i]<-ldaConfusion$overall[2]
  lda.total.eval$Sensitivity[i]<-ldaConfusion$byClass[1]
  lda.total.eval$Specificity[i]<-ldaConfusion$byClass[2]
  lda.total.eval$PPV[i]<-ldaConfusion$byClass[3]
  lda.total.eval$NPV[i]<-ldaConfusion$byClass[4]
  lda.total.eval$AUC[i]<-prSummary(ldaTogether,lev=levels(ldaClasses))[1]
  
  #check which ones gives wrong predictions
  wrong_pred <- which(ldaClasses!=testing$genus)
  lda.misclassify[[i]]<-metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]
  
  # Evaluate RF -------
  #Tesing rf model with testing data
  rfClasses <- predict(rfFit, newdata = testing)
  rfProbs <- predict(rfFit, newdata = testing, type = "prob")
  rfConfusion<-confusionMatrix(rfClasses, testing$genus)
  rfTogether<-cbind(obs=testing$genus,pred=rfClasses,rfProbs)
  
  #Results RF
  rf.total.confusion[,,i]<-rfConfusion$table #the actual confusion matrix
  rf.total.eval$Accuracy[i]<-rfConfusion$overall[1]
  rf.total.eval$Kappa[i]<-rfConfusion$overall[2]
  rf.total.eval$Sensitivity[i]<-rfConfusion$byClass[1]
  rf.total.eval$Specificity[i]<-rfConfusion$byClass[2]
  rf.total.eval$PPV[i]<-rfConfusion$byClass[3]
  rf.total.eval$NPV[i]<-rfConfusion$byClass[4]
  rf.total.eval$AUC[i]<-prSummary(rfTogether,lev=levels(rfClasses))[1]
  
  #looking at which wrong are incorrectly predicted
  wrong_pred <- which(rfClasses!=testing$genus)
  rf.misclassify[[i]]<-metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]
  
  # Evaluate SVM ------
  #Tesing svm on test set
  svmClasses<-predict(svmFit, newdata = testing) #example of possibilities for regression instead of categorical
  svmProbs <- predict(svmFit, newdata = testing, type = "prob")
  svmConfusion<-confusionMatrix(svmClasses, testing$genus)
  svmTogether<-cbind(obs=testing$genus,pred=svmClasses,svmProbs)
  
  #Results SVM
  svm.total.confusion[,,i]<-svmConfusion$table #the actual confusion matrix
  svm.total.eval$Accuracy[i]<-svmConfusion$overall[1]
  svm.total.eval$Kappa[i]<-svmConfusion$overall[2]
  svm.total.eval$Sensitivity[i]<-svmConfusion$byClass[1]
  svm.total.eval$Specificity[i]<-svmConfusion$byClass[2]
  svm.total.eval$PPV[i]<-svmConfusion$byClass[3]
  svm.total.eval$NPV[i]<-svmConfusion$byClass[4]
  svm.total.eval$AUC[i]<-prSummary(svmTogether,lev=levels(svmClasses))[1]
  
  #looking at which wrong are incorrectly predicted
  wrong_pred <- which(svmClasses!=testing$genus)
  svm.misclassify[[i]]<-metadata.avg$Identity[-c(extinct,inTrain)][wrong_pred]
  
  
  # # Assign Fossils--------------------------------------
  # #Tesing rda model with extinct data
  # lda.Probs.e <- predict(ldaFit, newdata = extinct.testing, type = "prob")
  # lda.total.fossil[,1,i]<- lda.Probs.e$Coendou
  # lda.total.fossil[,2,i]<- lda.Probs.e$Erethizon
  # 
  # #Tesing rf model with extinct data
  # rfProbs.e <- predict(rfFit, newdata = extinct.testing, type = "prob")
  # rf.total.fossil[,1,i]<- rf.Probs.e$Coendou
  # rf.total.fossil[,2,i]<- rf.Probs.e$Erethizon
  # #Tesing svm on extinct set
  # svmProbs.e <- predict(svmFit, newdata = extinct.testing, type = "prob")
  # svm.total.fossil[,1,i]<- svmProbs.e$Coendou
  # svm.total.fossil[,2,i]<- svmProbs.e$Erethizon
  
  # jump up ------
  start.seed<-start.seed+i*100
  i<-j+1
}