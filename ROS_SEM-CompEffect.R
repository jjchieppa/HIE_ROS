library(piecewiseSEM); library(nlme)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

df<-read.csv("ROS competition data all.csv")
df<-subset(df, Phase != "Control")

df<-subset(df, Experiment == "Double")
df$x<-df$Ht_mm
# df<-subset(df, Species == "Rye")

df<-df[complete.cases(df$x),]

# all_dro<-psem(
#   lme(x ~ PhaseNum, random = ~1|Experiment, method = "ML", df),
#   lme(CompEffect ~ x, random = ~1|Experiment, method = "ML", df)
# )
all_spp<-psem(
  lme(x ~ PhaseNum, random = ~1|Species, method = "ML", df),
  lme(CompEffect ~ x, random = ~1|Species, method = "ML", df)
)



# cat("\f")
# summary(all_dro, df, standarize.type = "latent.linear")
summary(all_spp, df, standarize.type = "latent.linear")

par(mfrow = c(1,2))
plot(x ~ PhaseNum, df); m<-lm(x~PhaseNum,df); abline(m)
plot(CompEffect ~ x,df); m<-lm(CompEffect~x,df); abline(m)

