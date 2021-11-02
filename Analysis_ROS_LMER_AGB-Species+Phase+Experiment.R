# in data ####
library(doBy); library(plotrix); library(emmeans)
library(multcomp); library(car); library(lme4)
library(MuMIn); library(effects); library(lmerTest)
library(pbkrtest)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

df<-read.csv("ROS_data_all.csv")
df$Shelter<-as.factor(df$Shelter)

# select and run ####
df<-subset(df, Species == "Rye" & Experiment == "Double")
df$x<-(df$Ht_cm)

# leveneTest((x) ~ Phase * Type, center = mean, df)

m1<-lmer((x) ~ Phase * Type + (1|Shelter), REML = T, df)
round(r.squaredGLMM(m1),2)
round(anova(m1, ddf = "Kenward-Roger"),3)

# cld(emmeans(m1, ~ Phase:Type, tran = "response")) #transforms before pval
cld(emmeans(m1, ~ Phase, type = "response")) #transforms after pval calculation
r<-residuals(m1)
shapiro.test(r)
# hist(r)