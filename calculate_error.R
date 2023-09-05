#used with output of procD.lm
#r are the number of replicates
errorGM<-function(ANOVA,r,f1=1,f2=2){ #f1 and f2 are which factors you want to compare
  #default f1 and f2 assume only one level in ANOVA, not nested
  # Fruciano 2016 repeatibility
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

#used with summary results of aov() function
#r is the number of replicates
error.univar<-function(ANOVA,r,f1=1,f2=2){
  # Yezerinac et al. 1992 p. 474 % measurement error
  s.within<-ANOVA$`Mean Sq`[f2]
  s.among<-(ANOVA$`Mean Sq`[f1]-s.within)/r
  percent_measurement_error<-(s.within/(s.within+s.among))*100
  repeatability<-100-percent_measurement_error
  result<-list(repeatability,percent_measurement_error)
  names(result)<-c("repeatability","PME")
  return(result)
}

find_repeatablePCs<-function(PCscores,variable,rep){
  repeatability<-rep(NA,ncol(PCscores))
  for(i in 1:ncol(PCscores)){
    testgdf<-geomorph.data.frame(coords=PCscores[,i],specimen=factor(variable))
    errorANOVA<-procD.lm(coords~specimen,data=testgdf,iter=999,RRPP=TRUE) %>% .$aov.table
    repeatability[i]<-((errorANOVA$MS[1]-errorANOVA$MS[2])/rep)/(errorANOVA$MS[2]+((errorANOVA$MS[1]-errorANOVA$MS[2])/rep))
  }
  plot(repeatability,xlab="principal components")
  lines(repeatability)
  abline(h=0.95,col="red",lty=2)
  abline(h=0.90,col="blue",lty=3)
  return(repeatability)
}


