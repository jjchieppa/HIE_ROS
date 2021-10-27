library(piecewiseSEM); library(nlme)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

dat<-read.csv("ROS competition data all.csv")
# dat<-subset(dat, Phase != "Control")
# dat$inComp<-ifelse(dat$Type=="Mix","0","1"); dat$inComp<-as.integer(dat$inComp)
# dat$Phase<-ifelse(dat$Phase=="Resistance","0","1"); dat$Phase<-as.integer(dat$Phase)

dat<-subset(dat, Experiment == "Long")
dat$x<-dat$SPAD
# dat<-subset(dat, Species == "Pha")

dat<-dat[complete.cases(dat$x),]

my_sem<-psem(
  lme(x ~ PhaseNum, random = ~1|Species, method = "ML", dat),
  lme(CompEffect ~ x, random = ~1|Species, method = "ML", dat)
)

# my_sem<-psem(
#   lme(x ~ PhaseNum, random = ~1|Experiment, method = "ML", dat),
#   lme(CompEffect ~ x, random = ~1|Experiment, method = "ML", dat)
# )

# my_sem<-psem(
#   lm(x ~ PhaseNum, dat),
#   lm(CompEffect ~ x, dat)
# )
# cat("\f")

summary(my_sem, df, standarize.type = "latent.linear")
