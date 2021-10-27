library(doBy); library(plotrix); library(lme4)
library(lmerTest); library(multcomp); library(car)
library(MuMIn)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")

dat<-read.csv("ROS data all.csv"); dat<-subset(dat, Experiment == "Double")
df<-subset(dat, Species == "Rye")

leveneTest(log(AGB_g) ~ Phase * Type, center = mean, df)


m1<-lmer(log(AGB_g) ~ Phase * Type + (1|Shelter), REML = T, df)
r.squaredGLMM(m1)
anova(m1, ddf = "Kenward-Roger")

df$trt<-with(df, interaction(Phase,Type))
m1<-lmer(log(AGB_g) ~ trt + (1|Shelter), REML = T, df)
tuk1<-summary(glht(m1, linfct = mcp(trt = "Tukey")))
new1<-summary(tuk1, test = adjusted("single-step"))
cld(new1)
