library(piecewiseSEM); library(nlme)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

dat<-read.csv("ROS data all.csv")
dat<-subset(dat, Phase != "Control")
dat$inComp<-ifelse(dat$Type=="Mix","0","1"); dat$inComp<-as.integer(dat$inComp)
dat$trt<-interaction(dat$Phase, dat$inComp)

dat<-subset(dat, Experiment == "Double") #SLD
dat<-subset(dat, Species == "Pha") # FRP
dat$x<-(dat$SPAD)


dat<-dat[complete.cases(dat$x),]
par(mfrow = c(2,2), omi = c(0.1,0.1,0,0), mar = c(3,3,0.5,0.5))
plot(dat$x ~ dat$PhaseNum, xlim = c(-.5,1.5))
plot(dat$x ~ dat$inComp, xlim = c(-.5,1.5))
plot(dat$x ~ dat$trt, xlim = c(0,7))
plot(dat$AGB_g ~ dat$x)


my_sem<-psem(
  lme(x ~ PhaseNum * inComp, random = list(~1|Shelter), method = "ML", dat),
  lme(AGB_g ~ x, random = list(~1|Shelter), method = "ML", dat)
)
# cat("\f")

summary(my_sem, df)