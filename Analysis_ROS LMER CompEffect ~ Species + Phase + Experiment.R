library(car); library(multcomp); library(lme4)

setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

dat<-read.csv("ROS competition data all.csv")
df<-subset(dat, Experiment == "Double" & Species == "Rye")

cat("\f")

df$x<-(df$CompEffect)

leveneTest(x ~ Phase, center = mean, df)
m1<-lmer(x ~ Phase + (1|Shelter), df)

r.squaredGLMM(m1); anova(m1)
tuk1<-summary(glht(m1, linfct = mcp(Phase = "Tukey")))
new1<-summary(tuk1, test = adjusted("single-step"))
cld(new1)
