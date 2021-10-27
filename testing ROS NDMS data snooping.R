library(car); library(multcomp); library(vegan)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

par(mfrow = c(1,2))

dat<-read.csv("ROS data all.csv")
df<-subset(dat, Experiment == "Short" & Type == "Mix")
df<-subset(df, Species == "Pha")
df<-df[complete.cases(df[,c(8,10,13,14,15,16)])]
# df$trt<-interaction(df$Species, df$Phase)
new<-df[,c(10,13,14,15,16)]
m1<-metaMDS(new, trymax = 40, k = 2); plot(m1, type = "n")
points(m1, display = "sites", pch = as.numeric(df$Phase), col = as.numeric(df$Phase))
grp<-sort(unique(df$Phase))
for(i in seq(grp)) {
  ordiellipse(m1, df$Phase, kind = "se", conf = 0.95, label = TRUE,
              font = 2, cex = 1.2, col = i, show.groups = grp[i])
}

dat<-read.csv("ROS data all.csv")
df<-subset(dat, Experiment == "Short" & Type != "Mix")
df<-subset(df, Species == "Pha")
df<-df[complete.cases(df[,c(8,10,13,14,15,16)])]
# df$trt<-interaction(df$Species, df$Phase)
new<-df[,c(10,13,14,15,16)]

m1<-metaMDS(new, trymax = 40, k = 2); plot(m1, type = "n")
points(m1, display = "sites", pch = as.numeric(df$Phase), col = as.numeric(df$Phase))
grp<-sort(unique(df$Phase))
for(i in seq(grp)) {
  ordiellipse(m1, df$Phase, kind = "se", conf = 0.95, label = TRUE,
              font = 2, cex = 1.2, col = i, show.groups = grp[i])
}