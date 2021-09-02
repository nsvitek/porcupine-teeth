
#plot the data
metadata.extant$Genus<-factor(metadata.extant$Genus)
metadata.extant$Slice_from_Base<-factor(metadata.extant$Slice_from_Base)

linear2plot<-metadata.extant %>% ungroup %>% 
  select(Genus, Slice_from_Base,"Anterofossettid Angle", "Anterofossettid Ratio","Hypolophid Ratio",
         "Ectolophid Ratio","Posterolophid Evenness") %>%
  melt(id=c("Genus","Slice_from_Base"))


ggplot(linear2plot, aes(x=Genus, y = value)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(aes(color=as.factor(Slice_from_Base)))  + 
  theme_minimal() + scale_color_manual(name="Slice",values=scale_slice) +
  # facet_grid() + 
  facet_wrap(vars(variable), scales="free_y", strip.position="top")+
  theme(axis.title.y = element_blank(), axis.text.x = element_text(face="italic"),
        strip.text.y.right = element_text(angle=0))
ggsave("linear_boxplots.pdf", device = cairo_pdf, width = double.column.width, 
       height = double.column.width,units="in",dpi=600)

# variables for plotting categorical variable
metadata.extant$"Metaflexid Closure"<-metadata.extant$metaflexid
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

#ANOVAs
fit <- lm(`Anterofossettid Angle` ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2) %>% write.csv("ANOVA_anterofossettid_angle.csv")

fit <- lm(`Anterofossettid Ratio` ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2) %>% write.csv("ANOVA_anterofossettid_ratio.csv")

fit <- lm(`Hypolophid Ratio` ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2)  %>% write.csv("ANOVA_hypolophid_ratio.csv")

fit <- lm(`Ectolophid Radio` ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2) %>% write.csv("ANOVA_ectolophid_ratio.csv")

fit <- lm(`Posterolophid Evenness` ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2) %>% write.csv("ANOVA_posterolophid.csv")

fit <- lm(metaflexid ~ Genus + Slice_from_Base + Genus:Slice_from_Base, data = metadata.extant)
Anova(fit, type=2) %>% write.csv("ANOVA_metaflexid.csv")

#calculate means and standard deviations
univariate.means<-dcast(linear2plot, Genus ~ variable, mean)
univariate.sds<-dcast(linear2plot, Genus ~ variable, sd)

rbind(univariate.means, univariate.sds) %>% write.csv("univariate_summary_stats.csv") 

# # Crown Height ------
# #in mm, how much height of the tooth is sampled?
# #FIX THIS TO MORE ACCURATELY INCLUDE #of images between slices
# sampled.height<-metadata.extant %>% group_by(Specimen,Genus,Species) %>% 
#   summarise(mean.cs = mean(Centroid_Size),slices=max(Slice_from_Base),
#             height.sampled.mm=max(Slice_from_Base)*mean(Size)*mean(Imgs_per_Slice))
# 
# #If larger tooth, still just more to sample
# ggplot(sampled.height,aes(x=mean.cs,y=height.sampled.mm,color=Species,shape=Genus)) + theme_minimal() + 
#   geom_point()
# 
# #but not necessarily any more hypsodont (neither are technically hypsodont)
# ggplot(sampled.height,aes(x=mean.cs,y=height.sampled.mm/mean.cs,color=Species,shape=Genus)) + theme_minimal() + 
#   geom_point()