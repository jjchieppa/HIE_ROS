# libs ####

library(doBy); library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# start ####

# tiff(file = "ROS_BGB-Species+Phase_Experiment.tiff", height = 10, width = 12, res = 600, units = "in", compression = "zip+p")
xx<-c(-500,500); yy<-c(-500,500)

par(mfrow = c(3,4), mar = c(1,0,0,0), omi = c(1.5,1,0.5,0.5))

# a fes short ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Short")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")
df<-subset(sin, Species == "Fes" & Phase == "Resistance")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Fes" & Phase == "Recovery")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

mtext(side = 3, expression(italic("F. arundinacea")), cex = 1.5)
axis(2, at = seq(-150,0,30), labels = c(150,120,90,60,30,0), cex.axis = 1.75, las = 2)
text(0.1, -5, "a)", cex = 2)

# b pha short ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Short")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")
df<-subset(sin, Species == "Pha" & Phase == "Resistance")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Pha" & Phase == "Recovery")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

mtext(side = 3, expression(italic("P. aquatica")), cex = 1.5)
text(0.1, -5, "b)", cex = 2)

# c lol short ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Short")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")
df<-subset(sin, Species == "Rye" & Phase == "Resistance")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Rye" & Phase == "Recovery")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

mtext(side = 3, expression(italic("L. perenne")), cex = 1.5)
text(0.1, -5, "c)", cex = 2)

# d mix short ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Short")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(mix, Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")
df<-subset(mix, Species == "Rye" & Phase == "Resistance")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(mix, Species == "Rye" & Phase == "Recovery")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")



mtext(side = 3, "Mixture Total", cex = 1.5)
mtext(side = 4, "Short", cex = 1.5, padj = 1)

text(0.1, -5, "d)", cex = 2)

# e fes long ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Long")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")
df<-subset(sin, Species == "Fes" & Phase == "Resistance")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Fes" & Phase == "Recovery")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

axis(2, at = seq(-150,0,30), labels = c(150,120,90,60,30,0), cex.axis = 1.75, las = 2)

text(0.1, -5, "e)", cex = 2)

# f pha long ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Long")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")
df<-subset(sin, Species == "Pha" & Phase == "Resistance")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Pha" & Phase == "Recovery")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

text(0.1, -5, "f)", cex = 2)

# g lol long ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Long")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")
df<-subset(sin, Species == "Rye" & Phase == "Resistance")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Rye" & Phase == "Recovery")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

text(0.1, -5, "g)", cex = 2)

# h mix long ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Long")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(mix, Species == "Rye" & Phase == "Resistance")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(mix, Species == "Rye" & Phase == "Recovery")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")
df<-subset(mix, Species == "Rye" & Phase == "Control")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")

mtext(side = 4, "Prolonged", cex = 1.5, padj = 1)

text(0.1, -5, "viii)", cex = 2)

# i fes repeated ####











plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Double")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Fes" & Phase == "Resistance")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Fes" & Phase == "Recovery")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")
df<-subset(sin, Species == "Fes" & Phase == "Control")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

axis(1, at = c(1,2,3), labels = c("Resistance","Recovery","Well-Watered"), cex.axis = 1.75, las = 2)
axis(2, at = seq(-150,0,30), labels = c(150,120,90,60,30,0), cex.axis = 1.75, las = 2)

text(0.1, -5, "ix)", cex = 2)

# j pha repeated ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Double")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Pha" & Phase == "Resistance")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Pha" & Phase == "Recovery")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")
df<-subset(sin, Species == "Pha" & Phase == "Control")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

axis(1, at = c(1,2,3), labels = c("Resistance","Recovery","Well-Watered"), cex.axis = 1.75, las = 2)

text(0.1, -5, "x)", cex = 2)

# k lol repeated ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Double")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(sin, Species == "Rye" & Phase == "Resistance")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(sin, Species == "Rye" & Phase == "Recovery")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")
df<-subset(sin, Species == "Rye" & Phase == "Control")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "c")

axis(1, at = c(1,2,3), labels = c("Resistance","Recovery","Well-Watered"), cex.axis = 1.75, las = 2)

text(0.1, -5, "xi)", cex = 2)

# l mix repeated ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-150,0), xaxt = "n", xlab = "", yaxt = "n", ylab = "")

dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Double")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix); mix$BGB_g.mean<-(mix$BGB_g.mean*-1); mix$BGB_g.std.error<-(mix$BGB_g.std.error*-1)
sin<-summaryBy(BGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin); sin$BGB_g.mean<-(sin$BGB_g.mean*-1); sin$BGB_g.std.error<-(sin$BGB_g.std.error*-1)

df<-subset(mix, Species == "Rye" & Phase == "Resistance")
rect(0.75, 0, 1.25, df$BGB_g.mean, col = "white")
ablineclip(v = 1, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(1, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "a")
df<-subset(mix, Species == "Rye" & Phase == "Recovery")
rect(1.75, 0, 2.25, df$BGB_g.mean, col = "grey40")
ablineclip(v = 2, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(2, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")
df<-subset(mix, Species == "Rye" & Phase == "Control")
rect(2.75, 0, 3.25, df$BGB_g.mean, col = "black")
ablineclip(v = 3, lwd = 1.5, y1 = (df$BGB_g.mean-df$BGB_g.std.error), y2 = (df$BGB_g.mean+df$BGB_g.std.error))
text(3, as.numeric((df$BGB_g.mean+df$BGB_g.std.error)-8), cex = 1.75, "b")

axis(1, at = c(1,2,3), labels = c("Resistance","Recovery","Well-Watered"), cex.axis = 1.75, las = 2)
mtext(side = 4, "Repeated", cex = 1.5, padj = 1)
mtext(side = 2, "Belowground Biomass (g)", cex = 1.5, padj = -3, outer = T)

text(0.1, -5, "xii)", cex = 2)

# off ####
# dev.off()
