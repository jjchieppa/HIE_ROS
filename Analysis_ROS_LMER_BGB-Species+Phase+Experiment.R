library(doBy); library(plotrix); library(lme4)
library(lmerTest); library(multcomp); library(car)
library(MuMIn)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")

dat<-read.csv("ROS data all.csv"); dat<-subset(dat, Experiment == "Short")
df<-subset(dat, Type == "Fes")

leveneTest(log(BGB_g) ~ Phase, center = mean, df)


m1<-lmer(log(BGB_g) ~ Phase + (1|Shelter), REML = T, df)
r.squaredGLMM(m1)
anova(m1, ddf = "Kenward-Roger")

tuk1<-summary(glht(m1, linfct = mcp(Phase = "Tukey")))
new1<-summary(tuk1, test = adjusted("single-step"))
cld(new1)
