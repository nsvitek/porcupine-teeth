library(ggfortify)
library(ggplot2)
library(car)
setwd("E:\\Pilot_Dataset_3_Digitized")

data_file <- 'Digitized_Measurement_Data.csv'

df <- read.csv(data_file, header = TRUE)

slice_selected <- 8
df <- df[-which(df$Slice_from_Base >= slice_selected),]

df$Genus_f <- factor(df$Genus)
df$Slice_from_Base <- factor(df$Slice_from_Base)
df$anterofossettid_ratio <- df$anterofossettid_width/df$anterofossettid_length
df$meso_hypo_ratio <- df$max_length_mesoflexid/df$max_length_hypolophid
df$ecto_talo_ratio <- df$ectolophid_width/df$talonid_width
df$posterolophid_ratio <- df$mid_posterolophid_length/df$max_posterolophid_length



###one way ANOVA
fit <- lm(anterofossettid_ratio ~ Genus_f, data = df)
anova(fit)
fit <- lm(meso_hypo_ratio ~ Genus_f, data = df)
anova(fit)
fit <- lm(ecto_talo_ratio ~ Genus_f, data = df)
anova(fit)
fit <- lm(posterolophid_ratio ~ Genus_f, data = df)
anova(fit)
fit <- lm(anterofossettid_angle ~ Genus_f, data = df)
anova(fit)
fit <- lm(hypoflexid_angle ~ Genus_f, data = df)
anova(fit)

###two way ANOVAs for unbalance design
fit <- lm(anterofossettid_ratio ~ Genus_f + Slice_from_Base + Genus_f:Slice_from_Base, data = df)
Anova(fit, type=3)
fit <- lm(meso_hypo_ratio ~ Genus_f + Slice_from_Base + Genus_f:Slice_from_Base, data = df)
Anova(fit, type=3)
fit <- lm(ecto_talo_ratio ~ Genus_f + Slice_from_Base + Genus_f:Slice_from_Base, data = df)
Anova(fit, type=3)
fit <- lm(posterolophid_ratio ~ Genus_f + Slice_from_Base + Genus_f:Slice_from_Base, data = df)
Anova(fit, type=3)
fit <- lm(anterofossettid_angle ~ Genus_f + Slice_from_Base + Genus_f:Slice_from_Base, data = df)
Anova(fit, type=3)
fit <- lm(hypoflexid_angle ~ Genus_f + Slice_from_Base + Genus_f:Slice_from_Base, data = df)
Anova(fit, type=3)

ggplot(df, aes(x=Genus_f, y=anterofossettid_ratio)) + 
  geom_point(aes(color=as.factor(Slice_from_Base)))  + 
  labs(color='Slice From Base') +
  labs(title="Anterofossettid Ratio",
       x ="Genus", y = "Anterofossettid Ratio") +
  theme_minimal()

ggplot(df, aes(x=Genus_f, y=meso_hypo_ratio)) +
  geom_point(aes(color=as.factor(Slice_from_Base)))  + 
  labs(color='Slice From Base') +
  labs(title="Meso-Hypo Ratio",
       x ="Genus", y = "Meso-Hypo Ratio") +
  theme_minimal()

ggplot(df, aes(x=Genus_f, y=ecto_talo_ratio)) + 
  geom_point(aes(color=as.factor(Slice_from_Base)))  + 
  labs(color='Slice From Base') +
  labs(title="Ecto-Talo Ratio",
       x ="Genus", y = "Ecto-Talo Ratio") +
  theme_minimal()

ggplot(df, aes(x=Genus_f, y=posterolophid_ratio)) + 
  geom_point(aes(color=as.factor(Slice_from_Base)))  + 
  labs(color='Slice From Base') +
  labs(title="Posterolophid Ratio",
       x ="Genus", y = "Posterolophid Ratio")+
  theme_minimal()

ggplot(df, aes(x=Genus_f, y=anterofossettid_angle)) + 
  geom_point(aes(color=as.factor(Slice_from_Base)))  + 
  labs(color='Slice From Base') +
  labs(title="Anterofossettid Angle",
       x ="Genus", y = "Anterofossettid Angle") +
  theme_minimal()

ggplot(df, aes(x=Genus_f, y=hypoflexid_angle)) + 
  geom_point(aes(color=as.factor(Slice_from_Base)))  + 
  labs(color='Slice From Base') +
  labs(title="Hypoflexid Angle",
       x ="Genus", y = "Hypoflexid Angle") +
  theme_minimal()

ggplot(df, aes(x=Genus_f, y=anterofossettid_ratio)) + geom_boxplot()
ggplot(df, aes(x=Genus_f, y=meso_hypo_ratio)) + geom_boxplot()
ggplot(df, aes(x=Genus_f, y=ecto_talo_ratio)) + geom_boxplot()
ggplot(df, aes(x=Genus_f, y=posterolophid_ratio)) + geom_boxplot()
ggplot(df, aes(x=Genus_f, y=anterofossettid_angle)) + geom_boxplot()
ggplot(df, aes(x=Genus_f, y=hypoflexid_angle)) + geom_boxplot()
