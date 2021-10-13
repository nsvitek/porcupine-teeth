# Procrustes ANOVA -------------------
ext_gdf<-geomorph.data.frame(coords=lm.2d.extant,
                             # identity=metadata.extant$Identity,
                             specimen=metadata.extant$Specimen,
                             wear=metadata.extant$Slice_from_Base,
                             genus=metadata.extant$Genus,
                             Csize=metadata.extant$Centroid_Size)

fit.wear<-procD.lm(coords ~ wear, data = ext_gdf, print.progress = FALSE)
fit.wear.genus<-procD.lm(coords ~ wear + genus, data = ext_gdf, print.progress = FALSE)
fit.wear.by.genus<-procD.lm(coords ~ wear + genus + wear:genus, data = ext_gdf, print.progress = FALSE)

anova(fit.wear,fit.wear.genus)
anova(fit.wear.genus,fit.wear.by.genus)

fit.wear.by.genus.size<-procD.lm(coords ~ wear + genus + log(Csize) + wear:genus, data = ext_gdf, print.progress = FALSE)
fit.wear.by.genus.by.size<-procD.lm(coords ~ wear  + genus + log(Csize) + genus:log(Csize) + wear:genus, data = ext_gdf, print.progress = FALSE)

anova(fit.wear.by.genus,fit.wear.by.genus.size)
anova(fit.wear.by.genus.size,fit.wear.by.genus.by.size)

anova.result<-anova(fit.wear.by.genus.by.size)
anova.result$table %>% write.csv("procrustes_anova.csv")

#take a look at diagnostic plots
# plot(fit.wear.by.genus.by.size, type = "diagnostics", col=ptol_pal()(2)[factor(ext_gdf$genus)],pch=c(16,17)[factor(ext_gdf$genus)])
# plot(fit.wear.by.genus.by.size, type = "PC", col=ptol_pal()(9)[factor(ext_gdf$wear)],pch=c(16,17)[factor(ext_gdf$genus)])

#visualize regression.
# plot(fit.wear.by.genus.by.size, type = "regression", reg.type = "PredLine",
#      predictor = ext_gdf$wear,col=ptol_pal()(2)[factor(ext_gdf$genus)],pch=19)
cairo_pdf("ProcrustesANOVA_wear.pdf",width=single.column.width+1,height=single.column.width+1)
plot.me<-plot(fit.wear.by.genus.by.size, type = "regression", reg.type = "RegScore",
     predictor = ext_gdf$wear,col=ptol_pal()(2)[factor(ext_gdf$genus)],pch=c(16,17)[factor(ext_gdf$genus)],
     xlab="Wear (Slice Number)")
legend("topleft",legend=levels(factor(ext_gdf$genus)),pch=c(16,17),col=ptol_pal()(2))
dev.off()
# plot(fit.wear.by.genus.by.size, type = "regression", reg.type = "PredLine",
#      predictor = log(ext_gdf$Csize),col=ptol_pal()(2)[factor(ext_gdf$genus)],pch=19)
cairo_pdf("ProcrustesANOVA_size.pdf",width=single.column.width+1,height=single.column.width+1)
plot.me2<-plot(fit.wear.by.genus.by.size, type = "regression", reg.type = "RegScore",
     predictor = log(ext_gdf$Csize),col=ptol_pal()(2)[factor(ext_gdf$genus)],pch=c(16,17)[factor(ext_gdf$genus)],
     xlab="ln(Centroid Size)")
dev.off()

plot(fit.wear.by.genus.by.size, type = "regression", reg.type = "RegScore",
     predictor = as.numeric(factor(ext_gdf$genus)),col=ptol_pal()(2)[factor(ext_gdf$genus)],pch=c(16,17)[factor(ext_gdf$genus)],
     xlab="Genus")


#POST-REVIEW: check y-axis between the two plots:
pick.one<-which(log(metadata.extant$Centroid_Size)== min(log(metadata.extant$Centroid_Size)))
plot.me$RegScore[pick.one]
plot.me2$RegScore[pick.one]
#from manual: "the latter [RegScore, Drake and Klingenber 2008] calculates 
#a regression score as a 
#projection of data on normalized vector that expresses the covariation 
#between shape and the regression coefficients for size, conditioned on other 
#model effects"


 
# Intra-Genus Allometry -------
#just to check: does centroid size follow the prediction that Erethizon is larger than Coendou?
ggplot(metadata.extant,aes(x=Genus,y=Centroid_Size)) + scale_color_ptol() +
  geom_boxplot() + geom_jitter() + theme_minimal()

#within specimens, does centroid size changes with wear stage? Maybe.
#a lot of variation is between specimens, maybe not necessarily within specimens
ggplot(metadata.extant,aes(x=Specimen,y=Centroid_Size,shape=Genus,color=factor(Slice_from_Base))) + theme_minimal() +
  geom_point() + scale_color_discrete(ptol_pal()(8)) + theme(axis.text.x = element_text(angle = 90))

# Check with a second plot
#Is there a consistent profile of size by wear?
ggplot(metadata.extant,aes(x=Slice_from_Base,y=Centroid_Size,color=Genus,group=Specimen)) + theme_minimal() +
  geom_line()

#split out by genus
index.erethizon<-which(metadata.extant$Genus=="Erethizon")
index.coendou<-which(metadata.extant$Genus=="Coendou")
lm.2d.erethizon<-lm.2d.extant[index.erethizon,]
lm.2d.coendou<-lm.2d.extant[index.coendou,]
metadata.erethizon<-metadata.extant[index.erethizon,]
metadata.coendou<-metadata.extant[index.coendou,]


# index.low<-which(metadata.erethizon$Slice_from_Base<=2)
# index.high<-which(metadata.erethizon$Slice_from_Base>=3)
#Erethizon
erethizon_gdf<-geomorph.data.frame(coords=lm.2d.erethizon[,],
                                   specimen=metadata.erethizon$Specimen[],
                                   wear=metadata.erethizon$Slice_from_Base[],
                                   Csize=metadata.erethizon$Centroid_Size[])
fit.erethizon.size<-procD.lm(coords ~ log(Csize), data = erethizon_gdf, print.progress=FALSE)

anova(fit.erethizon.size)

fit.erethizon.all<-procD.lm(coords ~ wear + log(Csize), data = erethizon_gdf, print.progress=FALSE)
anova(fit.erethizon.all)
anova(fit.erethizon.all)$table %>% write.csv("procrustes_anova_erethizon.csv")


#wear stages are different shapes.
#but the relationship between size and shape? Seems more that a few larger specimens get a wackier shape
#at low levels of wear (levels 0-2)...not sure how biologically meaningful meaningful
plot(fit.erethizon.size, type = "regression", reg.type = "RegScore", predictor = log(erethizon_gdf$Csize),
     col=ptol_pal()(9)[factor(erethizon_gdf$wear)],pch=19)
legend('topleft',legend=levels(factor(erethizon_gdf$wear)),pch=19,col=ptol_pal()(9))

plot(fit.erethizon.size, type = "regression", reg.type = "RegScore", predictor = log(erethizon_gdf$Csize),
     col=ptol_pal()(12)[factor(erethizon_gdf$specimen)],pch=19)
legend('topleft',legend=levels(factor(erethizon_gdf$specimen)),pch=19,col=ptol_pal()(12))

coendou_gdf<-geomorph.data.frame(coords=lm.2d.coendou,
                                 specimen=metadata.coendou$Specimen,
                                 species=metadata.coendou$Species,
                                 wear=metadata.coendou$Slice_from_Base,
                                 Csize=metadata.coendou$Centroid_Size)
fit.coendou.size<-procD.lm(coords ~ log(Csize), data = coendou_gdf, print.progress=FALSE)
#size in and of itself not an important factor, but still a significant interaction with wear
anova(fit.coendou.size)

fit.coendou.all<-procD.lm(coords ~ wear + log(Csize) + species, data = coendou_gdf, print.progress=FALSE)
anova(fit.coendou.all)
anova(fit.coendou.all)$table %>% write.csv("procrustes_anova_coendou.csv")

#visually, interaction checks out
plot(fit.coendou.size, type = "regression", reg.type = "RegScore", predictor = log(coendou_gdf$Csize),
     col=factor(coendou_gdf$wear),pch=19)
plot(fit.coendou.size, type = "regression", reg.type = "RegScore", predictor = log(coendou_gdf$Csize),
     col=factor(metadata.coendou$Species),pch=19)

# visualize predicted shape ------

#plot shapes predicted by the model. Code adapted from Rachel Griffin's code http://www.randigriffin.com/2017/11/10/plotting-shape-changes-geomorph.html
# fit.wear.by.genus.by.size$coefficients[]
#row 1 = intercept, 2 = wear, 3 = genus [0 or 1], 4 = centroid size

modelled.shape<-function(model, coeff.value, coeff.row){
  coeff.means<-colMeans(model$X) #the average of each specimen's x values (ex: wear stage, centroid size)
  coeff.predict<-coeff.means #make a copy to alter with specific values of interest
  coeff.predict[coeff.row]<-coeff.value #insert altered value
  coeff.for.predicted.shape<-coeff.predict*model$coefficients
  predicted.shape<-matrix(colSums(coeff.for.predicted.shape),ncol(coeff.for.predicted.shape)/2, 2,byrow=TRUE) #reformat as single shape array
  #this is to check making sure code works, may also be useful if want build-in mean shape
  # coeff.for.mean.shape<-coeff.means*model$coefficients #coefficients by coordinate value
  # mean.shape<-matrix(colSums(coeff.for.mean.shape),ncol(coeff.for.mean.shape)/2, 2,byrow=TRUE) #reformat as single shape array
  # #check that mean shape works
  # mean.shape<-mshape(arrayspecs(lm.2d.extant,p=ncol(lm.2d.extant)/2, 2))
  # plotRefToTarget(mean.shape,shps.test,method="points") #these are the same
  return(predicted.shape)
}

#worth plotting mean wear by stage, then mean Coendou wear by stage, then mean erethizon wear by stage. 
#coendou only goes up to slice 7

wear.range<-c(0:7)
for(i in wear.range){
  avg.model.shape<-modelled.shape(fit.wear.by.genus.by.size,coeff.value=i,coeff.row=2)
  cairo_pdf(paste("predicted_shape_avg_",i,".pdf",sep=""))
  plot(avg.model.shape,
       xlab = "", ylab = "", asp = 1, xaxt = "n", yaxt = "n", bty="n", pch=21,bg="gray",cex=2.75,lwd=2.75)
  dev.off()

  erethizon.shape<-modelled.shape(fit.erethizon.all,coeff.value=i,coeff.row=2)
  coendou.shape<-modelled.shape(fit.coendou.all,coeff.value=i,coeff.row=2)

  cairo_pdf(paste("predicted_shape_erethizon_",i,".pdf",sep=""))
  plotRefToTarget(erethizon.shape,avg.model.shape,method="points",
                  gridPars=gridPar(pt.bg=scale_taxon[7], pt.size=2))
  dev.off()
  cairo_pdf(paste("predicted_shape_coendou_",i,".pdf",sep=""))
  plotRefToTarget(coendou.shape,avg.model.shape,method="points",
                  gridPars=gridPar(pt.bg=scale_taxon[1], pt.size=2))
  dev.off()
}


mean.shape<-mshape(arrayspecs(lm.2d.extant,p=ncol(lm.2d.extant)/2, 2))
#TPS, vector, points
size.range<-range(log(ext_gdf$Csize))

