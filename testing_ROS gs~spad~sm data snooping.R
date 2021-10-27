library(effects); library(lubridate); library(doBy)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

spad<-read.csv("ROS data all.csv"); spad<-subset(spad, Type != "Mix")
gssm<-read.csv("ROS gssm raw data.csv")


gssm<-summaryBy(gs_mmol.m2.s + SM ~ Shelter * Side * Rep, FUN = max, na.rm = T, gssm)
spad<-summaryBy(SPAD ~ Experiment * Phase * Species * Shelter * Side * Rep, FUN = mean, na.rm = T, spad)

newdf<-merge(gssm, spad, by = c("Shelter","Side","Rep"))

df<-subset(newdf, Species == "Pha")
par(mfrow = c(1,3))

cat("\f")

plot(df$gs_mmol.m2.s.max ~ df$SM.max)
anova(lm(log(df$gs_mmol.m2.s.max) ~ log(df$SM.max)))

plot(df$SPAD.mean ~ df$SM.max)
anova(lm(df$SPAD.mean ~ df$SM.max))

plot(df$gs_mmol.m2.s.max ~ df$SPAD.mean)
anova(lm((df$gs_mmol.m2.s.max) ~ (df$SPAD.mean)))