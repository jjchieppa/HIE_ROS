library(doBy)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

dat<-read.csv("ROS competition data all.csv")
dat<-summaryBy(CompEffect ~ Species * Phase * Experiment, FUN = c(mean, std.error), na.rm = T, dat)

xx<-c(-500,500); yy<-c(-500,500)
par(mfrow = c(3,3), mar = c(1,0,0,0), omi = c(1.5,1,0.5,0.1))

###############################################################
##############################################################################################################################
###############################################################

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(2, at = seq(-1,1,0.25))
df<-subset(dat, Experiment == "Short" & Species == "Fes" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Short" & Species == "Fes" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Short" & Species == "Fes" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)


plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Short" & Species == "Pha" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Short" & Species == "Pha" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Short" & Species == "Pha" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)


plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Short" & Species == "Rye" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Short" & Species == "Rye" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Short" & Species == "Rye" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)

###############################################################
##############################################################################################################################
###############################################################

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Long" & Species == "Fes" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Long" & Species == "Fes" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Long" & Species == "Fes" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)


plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Long" & Species == "Pha" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Long" & Species == "Pha" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Long" & Species == "Pha" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)


plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Long" & Species == "Rye" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Long" & Species == "Rye" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Long" & Species == "Rye" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)

###############################################################
##############################################################################################################################
###############################################################

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Double" & Species == "Fes" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Double" & Species == "Fes" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Double" & Species == "Fes" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)


plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Double" & Species == "Pha" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Double" & Species == "Pha" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Double" & Species == "Pha" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)


plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.25,0.75), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
df<-subset(dat, Experiment == "Double" & Species == "Rye" & Phase == "Resistance")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Double" & Species == "Rye" & Phase == "Recovery")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
df<-subset(dat, Experiment == "Double" & Species == "Rye" & Phase == "Control")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)


