#set up tables for storing replicate results ------
remodel<-100 #number of times to reassign specimens and build new models

#empty array to hold multiple confusion matrices
#Note (because I can't figure out how to set this info in dimnames): rows are predicted, cols are reference
total.confusion<-array(dim=c(2,2,remodel))
dimnames(total.confusion)[1:2]<-list(c("Coendou","Erethizon"),c("Coendou","Erethizon"))

#empty array to hold model metrics
empty.col<-rep(NA,remodel)
total.eval<-data.frame(N_train = empty.col, N_test = empty.col, 
                       Sensitivity = empty.col, Specificity = empty.col, 
                       PPV = empty.col, NPV = empty.col, Accuracy = empty.col, 
                       Kappa = empty.col, AUC = empty.col)

lda.total.confusion<-rf.total.confusion<-svm.total.confusion<-total.confusion
lda.total.eval<-rf.total.eval<-svm.total.eval<-total.eval

assign.fossil<-total.confusion
rownames(assign.fossil)<-c("UF-UF-21743-00","UF-UF-21743-01")
lda.total.fossil<-rf.total.fossil<-svm.total.fossil<-assign.fossil

#build objects to keep track of misidentified specimens
misclassify<-vector(mode="list",length=remodel)
lda.misclassify<-rf.misclassify<-svm.misclassify<-misclassify

#using difference dataset from "Choosing_PCs.R" because want extinct specimen included
model_data<-data.frame(genus = as.factor(metadata.avg$Genus),PCA$x[,PCs])
extinct<-which(metadata.avg$Species=="kleini")
extinct.testing<-model_data[extinct,]
dim(extinct.testing)

#change evaluation to k-fold cross-validation ['repeatedcv'] instead of default bootstrapping
ctrl <- trainControl(
  method = "repeatedcv",
  repeats = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

performance_metric<-"ROC" #"Accuracy" or "ROC"?

start.seed<-100
i<-1

####################### start loop here --------------
source(source(paste(scriptsdir,"/Train_n_Test.R",sep="")))
############## End Loop ---------
write.csv(lda.total.confusion,"LDA_confusion.csv")
write.csv(lda.total.eval,"LDA_evaluation.csv")

write.csv(rf.total.confusion,"RF_confusion.csv")
write.csv(rf.total.eval,"RF_evaluation.csv")

write.csv(svm.total.confusion,"SVM_confusion.csv")
write.csv(svm.total.eval,"SVM_evaluation.csv")

write.csv(lda.total.fossil,"LDA_total_fossil.csv")
write.csv(rf.total.fossil,"RF_total_fossil.csv")
write.csv(svm.total.fossil,"SVM_total_fossil.csv")

sink("LDA_misclassify.txt")
print(lda.misclassify)
sink()

sink("RF_misclassify.txt")
print(rf.misclassify)
sink()

sink("SVM_misclassify.txt")
print(svm.misclassify)
sink()
# Compare Models ------
#find the average confusion matrix
all.eval<-rbind(lda.total.eval, rf.total.eval,svm.total.eval)
all.eval$Algorithm<-rep(c("LDA","RF","SVM"),each=remodel)

#create a faceted plot like Puschel et al. 2018: each facet a metric, three rows for each model,
#and a simple line plot: geom_pointrange()
all.eval.long<-melt(all.eval,id.vars=c("Algorithm","N_train","N_test"))
head(all.eval.long)

ggplot(data=all.eval.long, aes(x=value, y = Algorithm)) + 
  geom_line() +
  stat_summary(fun.data = "mean_cl_normal") +
  facet_wrap(vars(variable), strip.position="top") + theme_minimal() +
  theme(axis.title.x = element_blank())
ggsave("machine_learning_eval.pdf", device = cairo_pdf, width = double.column.width, 
       height = double.column.width,units="in",dpi=600)

#Create a table for reporting mean and sd values
all.eval %>% group_by(Algorithm) %>% summarise_all(list(mean,sd), na.rm=T) %>% write.csv("model_evaluation.csv")


# compare confusion matrices -------
mean.lda.confusion<-apply(lda.total.confusion, c(1,2), mean)
sd.lda.confusion<-apply(lda.total.confusion, c(1,2), sd)
mean.rf.confusion<-apply(rf.total.confusion, c(1,2), mean)
sd.rf.confusion<-apply(rf.total.confusion, c(1,2), sd)
mean.svm.confusion<-apply(svm.total.confusion, c(1,2), mean)
sd.svm.confusion<-apply(svm.total.confusion, c(1,2), sd)

mean.lda.fossil<-apply(lda.total.fossil, c(1,2), mean)
sd.lda.fossil<-apply(lda.total.fossil, c(1,2), sd)
mean.rf.fossil<-apply(rf.total.fossil, c(1,2), mean)
sd.rf.fossil<-apply(rf.total.fossil, c(1,2), sd)
mean.svm.fossil<-apply(svm.total.fossil, c(1,2), mean)
sd.svm.fossil<-apply(svm.total.fossil, c(1,2), sd)

rbind(mean.lda.confusion,sd.lda.confusion,
      mean.rf.confusion,sd.rf.confusion,
      mean.svm.confusion,sd.svm.confusion,
      mean.lda.fossil,sd.lda.fossil,
      mean.rf.fossil,sd.rf.fossil,
      mean.svm.fossil,sd.svm.fossil) %>% write.csv("confusion_summary.csv")

confusion.table<-data.frame(predicted.class=factor(c("C","E","C","E")),
                            reference.class=factor(c("C","C","E","E")),
                            mean.lda.confusion=as.vector(mean.lda.confusion) %>% round(1),
                            sd.lda.confusion=as.vector(sd.lda.confusion) %>% round(1),
                            mean.rf.confusion=as.vector(mean.rf.confusion) %>% round(1),
                            sd.rf.confusion=as.vector(sd.rf.confusion) %>% round(1),
                            mean.svm.confusion=as.vector(mean.svm.confusion) %>% round(1),
                            sd.svm.confusion=as.vector(sd.svm.confusion) %>% round(1))

confusion.table <- confusion.table %>%
  mutate(Classification = ifelse(predicted.class == reference.class, "correct", "incorrect")) %>%
  group_by(reference.class) %>%
  mutate(prop.lda = mean.lda.confusion/sum(mean.lda.confusion),
         prop.rf = mean.rf.confusion/sum(mean.rf.confusion),
         prop.svm = mean.svm.confusion/sum(mean.svm.confusion))

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
confuse.lda<-ggplot(data = confusion.table, mapping = aes(x = reference.class, 
                    y = predicted.class, fill = Classification)) +
  geom_tile(alpha = confusion.table$prop.lda) +
  geom_text(aes(label = paste(mean.lda.confusion,"\n(",sd.lda.confusion,")",sep="")), 
            vjust = .5, fontface  = "bold", alpha = .5) +
  scale_fill_manual(values = c(correct = scale_n[3], incorrect = scale_n[10])) +
  theme_minimal() + ylab("Predicted Genus") +
  theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face="italic"),legend.position = "none",
        axis.text.y = element_text(face="italic", angle=90), 
        axis.title.y=element_text(size=8))+  
  ylim(rev(levels(confusion.table$reference.class)))

confuse.rf<-ggplot(data = confusion.table, 
             mapping = aes(x = reference.class, y = predicted.class, fill = Classification)) +
  geom_tile(alpha = confusion.table$prop.rf) +
  geom_text(aes(label = paste(mean.rf.confusion,"\n(",sd.rf.confusion,")",sep="")), 
            vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(correct = scale_n[3], incorrect = scale_n[10])) +
  theme_minimal() + ylab("Predicted Genus")+
  theme(axis.title.x=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face="italic"),legend.position = "none",
        axis.text.y = element_text(face="italic", angle=90), 
        axis.title.y=element_text(size=8)) +  
  ylim(rev(levels(confusion.table$reference.class)))

confuse.svm<-ggplot(data = confusion.table, 
                   mapping = aes(x = reference.class, y = predicted.class, fill = Classification)) +
  geom_tile(alpha = confusion.table$prop.svm) +
  geom_text(aes(label = paste(mean.svm.confusion,"\n(",sd.svm.confusion,")",sep="")), 
            vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(correct = scale_n[3], incorrect = scale_n[10])) +
  theme_minimal()  + xlab("Reference Genus") + ylab("Predicted Genus")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(face="italic"),legend.position = "none",
        axis.text.y = element_text(face="italic", angle=90), 
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8))+
  ylim(rev(levels(confusion.table$reference.class)))

gc<-grid.arrange(confuse.lda,confuse.rf,confuse.svm, ncol=2,nrow=2)
ggsave("confusion_tables.pdf", gc,
         device = cairo_pdf, width = single.column.width,
       height=single.column.width, units="in",dpi=600)

ggsave("confusion_legend.pdf", confuse.svm + theme(legend.position = "right"), device = cairo_pdf, width = single.column.width,
       height=single.column.width, units="in",dpi=600)
# Examine misclassified images -----
#calculate baseline frequencies
count.slices<-as.factor(metadata.extant$Slice_from_Base) %>% summary()
freq.slices<-count.slices/nrow(metadata.extant)
plot.freq.slice<-data.frame(Frequency=freq.slices,
                            Slice=names(freq.slices))

count.specimens<-as.factor(metadata.extant$Specimen) %>% summary()
freq.specimens<-count.specimens/nrow(metadata.extant)

plot.freq.spec<-data.frame(Frequency=freq.specimens,
                           Specimen=names(freq.specimens))

#LDA
lda.freq<-lda.misclassify %>% unlist %>% as.factor() %>% summary() %>% as.data.frame
lda.freq$Specimen<-lda.freq %>% rownames %>% sub("(.*)_M3_([0-9]*)", "\\1",.)
lda.freq$Slice<-lda.freq %>% rownames %>% sub("(.*)_M3_([0-9]*)", "\\2",.)
colnames(lda.freq)[1]<-"Frequency"
lda.misclassify.all<-lda.freq[rep(row.names(lda.freq), lda.freq$Frequency), ]
lda.freq.slice<-lda.misclassify.all$Slice %>% as.factor %>% summary()/nrow(lda.misclassify.all)
lda.freq.specimen<-lda.misclassify.all$Specimen %>% as.factor %>% summary()/nrow(lda.misclassify.all)

plot.freq.spec$MF.lda<-0.0
plot.freq.spec$MF.lda[match(names(lda.freq.specimen),rownames(plot.freq.spec))]<-lda.freq.specimen
plot.freq.spec$RMF.lda<-plot.freq.spec$MF.lda/plot.freq.spec$Frequency

plot.freq.slice$MF.lda<-0.0
plot.freq.slice$MF.lda[match(names(lda.freq.slice),rownames(plot.freq.slice))]<-lda.freq.slice
plot.freq.slice$RMF.lda<-plot.freq.slice$MF.lda/plot.freq.slice$Frequency

#RF 
rf.freq<-rf.misclassify %>% unlist %>% as.factor() %>% summary() %>% as.data.frame
rf.freq$Specimen<-rf.freq %>% rownames %>% sub("(.*)_M3_([0-9]*)", "\\1",.)
rf.freq$Slice<-rf.freq %>% rownames %>% sub("(.*)_M3_([0-9]*)", "\\2",.)
colnames(rf.freq)[1]<-"Frequency"
rf.misclassify.all<-rf.freq[rep(row.names(rf.freq), rf.freq$Frequency), ]
rf.freq.slice<-rf.misclassify.all$Slice %>% as.factor %>% summary()/nrow(rf.misclassify.all)
rf.freq.specimen<-rf.misclassify.all$Specimen %>% as.factor %>% summary()/nrow(rf.misclassify.all)

plot.freq.spec$MF.rf<-0.0
plot.freq.spec$MF.rf[match(names(rf.freq.specimen),rownames(plot.freq.spec))]<-rf.freq.specimen
plot.freq.spec$RMF.rf<-plot.freq.spec$MF.rf/plot.freq.spec$Frequency

plot.freq.slice$MF.rf<-0.0
plot.freq.slice$MF.rf[match(names(rf.freq.slice),rownames(plot.freq.slice))]<-rf.freq.slice
plot.freq.slice$RMF.rf<-plot.freq.slice$MF.rf/plot.freq.slice$Frequency

#SVM
svm.freq<-svm.misclassify %>% unlist %>% as.factor() %>% summary() %>% as.data.frame
svm.freq$Specimen<-svm.freq %>% rownames %>% sub("(.*)_M3_([0-9]*)", "\\1",.)
svm.freq$Slice<-svm.freq %>% rownames %>% sub("(.*)_M3_([0-9]*)", "\\2",.)
colnames(svm.freq)[1]<-"Frequency"
svm.misclassify.all<-svm.freq[rep(row.names(svm.freq), svm.freq$Frequency), ]
svm.freq.slice<-svm.misclassify.all$Slice %>% as.factor %>% summary()/nrow(svm.misclassify.all)
svm.freq.specimen<-svm.misclassify.all$Specimen %>% as.factor %>% summary()/nrow(svm.misclassify.all)

plot.freq.spec$MF.svm<-0.0
plot.freq.spec$MF.svm[match(names(svm.freq.specimen),rownames(plot.freq.spec))]<-svm.freq.specimen
plot.freq.spec$RMF.svm<-plot.freq.spec$MF.svm/plot.freq.spec$Frequency

plot.freq.slice$MF.svm<-0.0
plot.freq.slice$MF.svm[match(names(svm.freq.slice),rownames(plot.freq.slice))]<-svm.freq.slice
plot.freq.slice$RMF.svm<-plot.freq.slice$MF.svm/plot.freq.slice$Frequency

# plot misclassification frequencies -------
plot.lda.spec<-ggplot(plot.freq.spec, aes(x=Specimen, y=RMF.lda)) +
  geom_hline(yintercept=1,lty=2) +
  geom_segment( aes(x=Specimen, xend=Specimen, y=0, yend=RMF.lda), color=scale_n[8]) +
  geom_point(size=3,color=scale_n[10],alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),axis.title.x=element_blank(),
    axis.ticks.y = element_blank(),axis.text.y=element_text(size=8))
plot.lda.slice<-ggplot(plot.freq.slice, aes(x=Slice, y=RMF.lda)) +
  geom_hline(yintercept=1,lty=2) +
  geom_segment( aes(x=Slice, xend=Slice, y=0, yend=RMF.lda), color=scale_n[8]) +
  geom_point(size=3,color=scale_n[10],alpha=1) +
  theme_light() + ylab("Relative Misclassification Frequency") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),axis.text.y=element_text(size=8),
    panel.border = element_blank(),axis.title.x=element_text(size=8),
    axis.ticks.y = element_blank())

plot.rf.spec<-ggplot(plot.freq.spec, aes(x=Specimen, y=RMF.rf)) +
  geom_hline(yintercept=1,lty=2) +
  geom_segment( aes(x=Specimen, xend=Specimen, y=0, yend=RMF.rf), color=scale_n[8]) +
  geom_point(size=3,color=scale_n[10],alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),axis.title.y=element_blank(),
    axis.title.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y = element_blank())
plot.rf.slice<-ggplot(plot.freq.slice, aes(x=Slice, y=RMF.rf)) +
  geom_hline(yintercept=1,lty=2) +
  geom_segment( aes(x=Slice, xend=Slice, y=0, yend=RMF.rf), color=scale_n[8]) +
  geom_point(size=3,color=scale_n[10],alpha=1) +
  theme_light() + ylab("Relative Misclassification Frequency") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),axis.title.y=element_blank(),
    axis.text.y=element_blank(),axis.title.x=element_text(size=8),
    axis.ticks.y = element_blank())
plot.svm.spec<-ggplot(plot.freq.spec, aes(x=Specimen, y=RMF.svm)) +
  geom_hline(yintercept=1,lty=2) +
  geom_segment( aes(x=Specimen, xend=Specimen, y=0, yend=RMF.svm), color=scale_n[8]) +
  geom_point(size=3,color=scale_n[10],alpha=1) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),axis.title.y=element_blank(),
    axis.text.y=element_blank(),axis.title.x=element_blank(),
    axis.ticks.y = element_blank())
plot.svm.slice<-ggplot(plot.freq.slice, aes(x=Slice, y=RMF.svm)) +
  geom_hline(yintercept=1,lty=2) +
  geom_segment( aes(x=Slice, xend=Slice, y=0, yend=RMF.svm), color=scale_n[8]) +
  geom_point(size=3,color=scale_n[10],alpha=1) +
  theme_light() + ylab("Relative Misclassification Frequency") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),axis.title.y=element_blank(),
    axis.text.y=element_blank(),axis.title.x=element_text(size=8),
    axis.ticks.y = element_blank())



g<-grid.arrange(plot.lda.spec,plot.rf.spec,plot.svm.spec,
             plot.lda.slice, plot.rf.slice, plot.svm.slice,
             ncol=3,nrow=2,widths=c(2.5,2,2),heights=c(3,2))
ggsave("misclassification_freq.pdf", g, device = cairo_pdf, width = double.column.width, 
       height = 4,units="in",dpi=600)
