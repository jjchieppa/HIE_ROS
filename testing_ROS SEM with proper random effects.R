  library(piecewiseSEM); library(nlme)
# setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

dat<-read.csv("ROS competition data all.csv")
dat<-subset(dat, Phase != "Control")
dat$CompEff_standard<-dat$CompEff_standard+1.590966

# dat<-subset(dat, Experiment == "Double") #SLD
# dat<-subset(dat, Species == "Fes") #FRP
dat$x<-(dat$Ht_mm)
dat$y<-(dat$CompEff_standard)

dat<-dat[complete.cases(dat$x),]
par(mfrow = c(1,2), omi = c(0.1,0.1,0,0), mar = c(3,3,0.5,0.5))
plot(dat$x ~ dat$PhaseNum, xlim = c(-.5,2.5))
plot(dat$y ~ dat$x)

# lme(x ~ PhaseNum, random = list(~1|Experiment, ~1|Zone, ~1|Rep), method = "ML", dat)

# Important traits within an experiment
my_sem<-psem(
  lme(x ~ PhaseNum, random = list(~1|Experiment, ~1|Species, ~1|Shelter), method = "ML", dat),
  lme(y ~ x, random = list(~1|Experiment, ~1|Species, ~1|Shelter), method = "ML", dat)
)
#   
# Important traits for species across all experiments
# my_sem<-psem(
#   lme(x ~ PhaseNum, random = list(~1|Experiment,~1|Shelter), method = "ML", dat),
#   lme(CompEffect ~ x, random = list(~1|Experiment,~1|Shelter), method = "ML", dat)
# )

# Important traits for all species within all experiments
# my_sem<-psem(
#   lme(x ~ PhaseNum, random = list(~1|Shelter, ~1|Species), method = "ML", dat),
#   lme(CompEffect ~ x, random = list(~1|Shelter, ~1|Species), method = "ML", dat)
# )

# Important traits for species across all experiments
# my_sem<-psem(
#   lme(x ~ PhaseNum, random = list(~1|Shelter), method = "ML", dat),
#   lme(CompEffect ~ x, random = list(~1|Shelter), method = "ML", dat)
# )

  # cat("\f")
  
  summary(my_sem, df, standarize.type = "latent.linear")
