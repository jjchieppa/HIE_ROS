# libs ####

library(doBy); library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# start ####

tiff(file = "ROS_ABG-Species+Phase+Experiment.tiff", height = 10, width = 12, res = 400, units = "in", compression = "zip+p")

xx<-c(-500,500); yy<-c(-500,500)

par(mfrow = c(3,3), mar = c(1,0,0,0), omi = c(1.5,1,0.5,0.5))

# a fes short ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Short")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(AGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix)
sin<-summaryBy(AGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin)


df<-subset(sin, Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")
df<-subset(mix, Species == "Fes" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")

df<-subset(sin, Species == "Fes" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")
df<-subset(mix, Species == "Fes" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "ab")

df<-subset(sin, Species == "Fes" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")
df<-subset(mix, Species == "Fes" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")


axis(1, at = c(1.5,4.5,7.5), labels = F)
axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
mtext(side = 3, expression(italic("F. arundinacea")), cex = 1.5)
text(0.1, 65, "a)", cex = 2)
legend("topleft", c("Without Competition","With Competition"), pch = 15, pt.cex = 3, col = c("grey","black"), cex = 1.5)


# b Pha short ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


df<-subset(sin, Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")
df<-subset(mix, Species == "Pha" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")

df<-subset(sin, Species == "Pha" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")
df<-subset(mix, Species == "Pha" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Pha" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Pha" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")


axis(1, at = c(1.5,4.5,7.5), labels = F)
# axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
mtext(side = 3, expression(italic("P. aquatica")), cex = 1.5)
text(0.1, 90, "b)", cex = 2)

# c lol short ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


df<-subset(sin, Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")
df<-subset(mix, Species == "Rye" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")

df<-subset(sin, Species == "Rye" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")
df<-subset(sin, Species == "Rye" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Rye" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Rye" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")


axis(1, at = c(1.5,4.5,7.5), labels = F)
# axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
mtext(side = 3, expression(italic("L. perenne")), cex = 1.5)
mtext(side = 4, "Short", cex = 1.5, padj = 1)
text(0.1, 90, "c)", cex = 2)

# d fes long ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Long")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(AGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix)
sin<-summaryBy(AGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin)


df<-subset(sin, Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "e")
df<-subset(mix, Species == "Fes" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")

df<-subset(sin, Species == "Fes" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Fes" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Fes" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "bc")
df<-subset(mix, Species == "Fes" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "cd")


axis(1, at = c(1.5,4.5,7.5), labels = F)
axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
text(0.1, 90, "d)", cex = 2)

# e pha long ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


df<-subset(sin, Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1.25, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)), cex = 1.75, "d")
df<-subset(mix, Species == "Pha" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")

df<-subset(sin, Species == "Pha" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Pha" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Pha" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")
df<-subset(mix, Species == "Pha" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")


axis(1, at = c(1.5,4.5,7.5), labels = F)
# axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
text(0.1, 90, "e)", cex = 2)

# f lol long ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


df<-subset(sin, Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")
df<-subset(mix, Species == "Rye" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")

df<-subset(sin, Species == "Rye" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Rye" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Rye" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Rye" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")


axis(1, at = c(1.5,4.5,7.5), labels = F)
# axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
mtext(side = 4, "Prolonged", cex = 1.5, padj = 1)
text(0.1, 90, "f)", cex = 2)

# g fes dou ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


dat<-read.csv("ROS_data_all.csv"); dat<-subset(dat, Experiment == "Double")
mix<-subset(dat, Type == "Mix")
sin<-subset(dat, Type != "Mix")
mix<-summaryBy(AGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, mix)
sin<-summaryBy(AGB_g ~ Phase * Species, FUN = c(mean,std.error), na.rm = T, sin)


df<-subset(sin, Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "e")
df<-subset(mix, Species == "Fes" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")

df<-subset(sin, Species == "Fes" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Fes" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Fes" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")
df<-subset(mix, Species == "Fes" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")


axis(1, at = c(1.5,4.5,7.5), labels = c("Well-watered","Resistance","Recovery"), cex.axis = 1.75, las = 2)
axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
text(0.1, 90, "g)", cex = 2)

# h pha dou ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


df<-subset(sin, Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")
df<-subset(mix, Species == "Pha" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "bc")

df<-subset(sin, Species == "Pha" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")
df<-subset(mix, Species == "Pha" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Pha" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "d")
df<-subset(mix, Species == "Pha" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")


axis(1, at = c(1.5,4.5,7.5), labels = c("Well-watered","Resistance","Recovery"), cex.axis = 1.75, las = 2)
# axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
text(0.1, 90, "h)", cex = 2)

# i rye dou ####

plot(xx ~ yy, xlim = c(0,9), ylim = c(0,95), xaxt = "n", xlab = "", yaxt = "n", ylab = "")


df<-subset(sin, Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 1, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(1.25, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+3), cex = 1.75, "d")
df<-subset(mix, Species == "Rye" & Phase == "Control")
rect(1.75, 0, 2.25, df$AGB_g.mean, col = "black")
ablineclip(v = 2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(2, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "bc")

df<-subset(sin, Species == "Rye" & Phase == "Resistance")
rect(3.75, 0, 4.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 4, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(4, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")
df<-subset(mix, Species == "Rye" & Phase == "Resistance")
rect(4.75, 0, 5.25, df$AGB_g.mean, col = "black")
ablineclip(v = 5, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(5, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "a")

df<-subset(sin, Species == "Rye" & Phase == "Recovery")
rect(6.75, 0, 7.25, df$AGB_g.mean, col = "grey")
ablineclip(v = 7, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(7, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "c")
df<-subset(mix, Species == "Rye" & Phase == "Recovery")
rect(7.75, 0, 8.25, df$AGB_g.mean, col = "black")
ablineclip(v = 8, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
text(8, as.numeric((df$AGB_g.mean+df$AGB_g.std.error)+8), cex = 1.75, "b")


axis(1, at = c(1.5,4.5,7.5), labels = c("Well-watered","Resistance","Recovery"), cex.axis = 1.75, las = 2)
# axis(2, at = seq(0,90,15), cex.axis = 1.75, las = 2)
mtext(side = 4, "Repeated", cex = 1.5, padj = 1)
text(0.1, 90, "i)", cex = 2)

mtext(side = 2, "Aboveground Biomass (g)", cex = 1.5, padj = -3, outer = T)

# off ####

dev.off()