# Load Dependencies -----
library(geomorph)
library(dplyr)
library(ggplot2)
# library(ggthemes) #ptol_pal
library(car) #modelling of ratios
library(caret)
library(inlmisc) #for more Paul Tol color options
library(reshape2) #for reformatting linear data to facet

scriptsdir <- "C://scripts/porcupine-teeth"
source(paste(scriptsdir,"/../observer-free-morphotype-characterization/find_repeatablePCs.R",sep=""))
source(paste(scriptsdir,"/calculate_error.R",sep=""))

iterations<-99 #bootstrap number, low for now, increase for final calcs

#for plotting
single.column.width<-3.27
double.column.width<-6.61
scale_n<-as.character(GetColors(10,scheme="sunset"))
scale_slice<-as.character(GetColors(9,scheme="discrete rainbow"))
scale_taxon<-as.character(GetColors(9,scheme="muted"))

# Data Input ------
setwd("D://Dropbox/Documents/research/mammals/porcupines/dental_variation/Pilot_Dataset_3_Digitized")

#read in Procrustes Aligned coordinates
lm.raw<-readland.tps("digitized_aligned.tps")

#read in metadata
metadata.raw<-read.csv("Digitized_Porcupine_Dataset_3.csv")

#Remove fossils that will be studied in another paper
lm.3d<-lm.raw[,,-which(metadata.raw$Specimen%in%c("UF-UF-223810","UF-UF-121740"))]
metadata<-metadata.raw[-which(metadata.raw$Specimen%in%c("UF-UF-223810","UF-UF-121740")),]

#make a single variable that identifies the "thing" being replicated
#(ex: the base of the m3 of FMNH-M-43289, not to be mixed up with the middle or top of the tooth)
metadata$Identity<-paste(metadata$Specimen,metadata$Molar,metadata$Slice_from_Base,sep="_")

#reformat/reproject shape data a couple of different ways
lm.2d<-two.d.array(lm.3d)

#write all results to their own folder
setwd("../output/")
sink(file="session_info.txt")
sessionInfo() %>% print() #for reporting package versions
sink() 

# # Error Analysis UNCOMMENT FOR FINAL ANALYSIS-------
#identity acts as marker of replicates: each slice should have a version from the left and right side
# error_gdf<-geomorph.data.frame(coords=lm.2d, identity=factor(metadata$Identity))
# 
# #Procrustes Regression
# errorANOVA<-procD.lm(coords~identity,data=error_gdf,iter=iterations)
# #Procrustes ANOVA
# errorANOVA$aov.table
# #error on landmarking due to size by keeping the same specimen but varying the size
# avg.number.replicates<-nrow(metadata)/(unique(c(metadata$Identity)) %>% length)
# #error3d and find repeatable PCs from homemade code
# err<-errorGM(errorANOVA$aov.table,r=avg.number.replicates,f1=1,f2=2)

# #Look at how error is distributed across PCs (Fruciano 2016)
# PCA.all<-prcomp(lm.2d, scale.=FALSE)
# jpeg(filename="error_pca.jpg")
# repPC<-find_repeatablePCs(PCA.all$x,factor(metadata$Identity),avg.number.replicates)
# title(paste("Overall Repeatability =", round(err$repeatability,3)))
# text(100,0.95,labels=paste(paste("# PCs > 0.9 =", max(which(repPC>=0.9)))))
# text(100,0.85,labels=paste(paste("# PCs > 0.8 =", max(which(repPC>=0.8)))))
# text(100,0.755,labels=paste(paste("# PCs > 0.6 =", max(which(repPC>=0.6)))))
# dev.off()

# to reduce error, average left and right sides --------
lm.2d.intermediate <- lm.2d %>% as.data.frame
lm.2d.intermediate$Identity<-metadata$Identity

lm.2d.avg<-lm.2d.intermediate %>% group_by(Identity) %>% summarise(across(starts_with("V"),mean)) #%>% select(-Identity)

# #check
# lm.array<-arrayspecs(lm.2d.avg,p=73,k=2)
# plot(lm.array[,,46])
# lm.2d.avg$Identity #before Identity was selected out of dataset

#to get metaflexid closure to work we're going to convert to numeric 
#because there's some disagreement between left and right slices
metadata$metaflexid<-0 #metaflexid closed
metadata$metaflexid[which(metadata$metaflexid_closed=="No")]<-1 #metaflexid open

#same for buccal posterolophid
metadata$posterolophid_curvature<-0 #convex
metadata$posterolophid_curvature[which(metadata$buccal_posterolophid=="Straight")]<-1 #straight
metadata$posterolophid_curvature[which(metadata$buccal_posterolophid=="Concave")]<-2 #concave

#take averages for each Identity
metadata.avg<-metadata %>% group_by(Identity,Specimen,Slice_from_Base,Genus,Species) %>% 
  summarise(across(where(is.numeric),mean))
metadata.avg$Taxon<-paste(metadata.avg$Genus,metadata.avg$Species)

# #check that everything is still in matching order
# cbind(metadata.avg$Identity,lm.2d.avg$Identity)

lm.2d.avg<-lm.2d.avg %>% select(-Identity) %>% as.matrix
# PCA -----------
PCA<-prcomp(lm.2d.avg, scale.=FALSE)
PCA.perc<-round(summary(PCA)$importance[2,]*100,1)
metadata.avg$PC1<-PCA$x[,1]
metadata.avg$PC2<-PCA$x[,2]

ggplot(data=metadata.avg,aes(x=PC1,y=PC2))+
  geom_point(size=2,aes(color = Taxon, shape=Taxon)) +
  theme_classic() +
  xlab(paste("PC 1 (",PCA.perc[1],"%)",sep="")) +
  ylab(paste("PC 2 (",PCA.perc[2],"%)",sep="")) + 
  scale_color_manual(values=scale_taxon) +
  scale_shape_manual(values=c(rep(16,6), 17, 15)) + 
  theme(legend.text=element_text(face="italic"))
ggsave("PCA_spp.pdf", device = cairo_pdf, width = 12, height = 8,units="cm",dpi=600)

ggplot(data=metadata.avg,aes(x=PC1,y=PC2))+
  geom_point(size=2,aes(color = factor(Slice_from_Base), shape=Taxon)) +
  theme_classic() +
  xlab(paste("PC 1 (",PCA.perc[1],"%)",sep="")) +
  ylab(paste("PC 2 (",PCA.perc[2],"%)",sep="")) + 
  scale_color_manual(name="Slice",values=scale_slice) +
  scale_shape_manual(values=c(rep(16,6), 17, 15))
ggsave("PCA_wear2.pdf", device = cairo_pdf, width = 12, height = 12,units="cm",dpi=600)

# Remove Fossil -----
lm.2d.extant<-lm.2d.avg[-which(metadata.avg$Species %in% c("kleini")),]
metadata.extant<-metadata.avg[-which(metadata.avg$Species %in% c("kleini")),]

#check for match
# dim(metadata.extant)
# dim(lm.2d.extant)

# # Procrustes ANOVA -----
# source(paste(scriptsdir,"Procrustes_ANOVA.R",sep="/"))
# Traditional Morphometrics -------
dim(metadata.extant)
head(metadata.extant)

metadata.extant<- metadata.extant %>% mutate("Anterofossettid Ratio" = anterofossettid_length/anterofossettid_width,
                           "Hypolophid Ratio" = max_length_hypolophid/max_length_mesoflexid,
                           "Ectolophid Ratio" = ectolophid_width/talonid_width,
                           "Posterolophid Evenness" = mid_posterolophid_length/max_posterolophid_length,
                           "Anterofossettid Angle" = anterofossettid_angle)

metadata.extant$Genus<-factor(metadata.extant$Genus)
metadata.extant$Slice_from_Base<-factor(metadata.extant$Slice_from_Base)

linear2plot<-metadata.extant %>% ungroup %>% 
  select(Genus, Slice_from_Base,"Anterofossettid Angle", "Anterofossettid Ratio","Hypolophid Ratio",
         "Ectolophid Ratio","Posterolophid Evenness") %>%
  melt(id=c("Genus","Slice_from_Base"))


ggplot(linear2plot, aes(x=Genus, y = value)) + 
  geom_boxplot() + 
  geom_jitter(aes(color=as.factor(Slice_from_Base)))  + 
  theme_minimal() + scale_color_manual(name="Slice",values=scale_slice) +
  # facet_grid() + 
  facet_wrap(vars(variable), scales="free_y", strip.position="top")+
  theme(axis.title.y = element_blank(), axis.text.x = element_text(face="italic"),
        strip.text.y.right = element_text(angle=0))
ggsave("linear_boxplots.pdf", device = cairo_pdf, width = double.column.width, 
       height = double.column.width,units="in",dpi=600)


# variables for plotting categorical
metadata.extant$metaflexid_closure<-"closed" #metaflexid closed
metadata.extant$metaflexid_closure[which(metadata.extant$metaflexid==1)]<-"open" #metaflexid open
metadata.extant$genus_state_metaflexid<-paste(metadata.extant$metaflexid_closure, metadata.extant$Genus)
count_metaflexid<-metadata.extant %>% 
  group_by(genus_state_metaflexid, Slice_from_Base, metaflexid_closure) %>% tally

ggplot(data = count_metaflexid, aes(x = genus_state_metaflexid, y = Slice_from_Base, fill = factor(n))) + 
  geom_tile() + theme_minimal() + 
  theme(axis.title.x = element_blank()) + ylab("Wear (Slice Number)") +
  scale_fill_manual( "Number of\nSpecimens", values=scale_n)
ggsave("metaflexid_open.pdf", device = cairo_pdf, width = single.column.width, 
       height = single.column.width,units="in",dpi=600)



?GetColors

fit <- lm(metaflexid ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2)

fit <- lm(anterofossettid_angle ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2)

fit <- lm(anterofossettid_ratio ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2)

fit <- lm(mesolophid_ratio ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2)

fit <- lm(ectolophid_ratio ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2)

fit <- lm(posterolophid_evenness ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2)

# linear.PCA<-metadata.extant %>% ungroup %>% select(anterofossettid_angle,mesolophid_ratio,
#                                                    ectolophid_ratio,posterolophid_evenness) %>%
#   prcomp(center = TRUE, scale. = TRUE)
# 
# plot(linear.PCA$x[,1:2],pch=c(21,22)[factor(metadata.extant$Genus)],bg = ptol_pal()(9)[metadata.extant$Slice_from_Base])
# 
# summary(linear.PCA)



# Crown Height ------
#in mm, how much height of the tooth is sampled?
#FIX THIS TO MORE ACCURATELY INCLUDE #of images between slices
sampled.height<-metadata.extant %>% group_by(Specimen,Genus,Species) %>% 
  summarise(mean.cs = mean(Centroid_Size),slices=max(Slice_from_Base),
            height.sampled.mm=max(Slice_from_Base)*mean(Size)*mean(Imgs_per_Slice))

#If larger tooth, still just more to sample
ggplot(sampled.height,aes(x=mean.cs,y=height.sampled.mm,color=Species,shape=Genus)) + theme_minimal() + 
  geom_point()

#but not necessarily any more hypsodont (neither are technically hypsodont)
ggplot(sampled.height,aes(x=mean.cs,y=height.sampled.mm/mean.cs,color=Species,shape=Genus)) + theme_minimal() + 
  geom_point()
