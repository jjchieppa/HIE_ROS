# in data ####
library(doBy); library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

dat<-read.csv("ROS_competition_data_all.csv")
dat<-summaryBy(CompEffect ~ Species * Phase * Experiment, FUN = c(mean, std.error), na.rm = T, dat)
# start ####
tiff(file = "ROS_CompEffects-Species+Phase+Experiment.tiff", height = 6.5, width = 10, res = 400, units = "in", compression = "zip+p")

xx<-c(-500,500); yy<-c(-500,500)
par(mfrow = c(3,3), mar = c(1,0,0,0), omi = c(1.05,1,0.5,0.5))

# a fes short ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 1)


axis(1, at = seq(1,3,1), labels = F)
axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
mtext(side = 3, expression(italic("F. arundinacea")), cex = 1.5)
df<-subset(dat, Experiment == "Short" & Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Short" & Species == "Fes" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "a")
df<-subset(dat, Experiment == "Short" & Species == "Fes" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "b")
text(0.1, 0.75, "a)", cex = 2)

mtext(side = 2, " Competitive Effect (log ratio)", outer = T, cex = 1.15, padj = -5.25)
mtext(side = 2, "(i.e. More positive indicates greater negative effects from competition)", outer = T, cex = 1, padj = -4.25)

# b pha short ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = F)
# axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
mtext(side = 3, expression(italic("P. aquatica")), cex = 1.5)
df<-subset(dat, Experiment == "Short" & Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Short" & Species == "Pha" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "a")
df<-subset(dat, Experiment == "Short" & Species == "Pha" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "c")
text(0.1, 0.75, "b)", cex = 2)

# c lol short ####
plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = F)
# axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
mtext(side = 3, expression(italic("L. perenne")), cex = 1.5)
df<-subset(dat, Experiment == "Short" & Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Short" & Species == "Rye" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "a")
df<-subset(dat, Experiment == "Short" & Species == "Rye" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "c")
text(0.1, 0.75, "c)", cex = 2)

mtext(side = 4, "Short", cex = 1.5, padj = 1)

# d fes long ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = F)
axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
df<-subset(dat, Experiment == "Long" & Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Long" & Species == "Fes" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Long" & Species == "Fes" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "a")
text(0.1, 0.75, "d)", cex = 2)

# e pha long ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = F)
# axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
df<-subset(dat, Experiment == "Long" & Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Long" & Species == "Pha" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "a")
df<-subset(dat, Experiment == "Long" & Species == "Pha" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "b")
text(0.1, 0.75, "e)", cex = 2)

# f pha long ####


plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = F)
# axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
df<-subset(dat, Experiment == "Long" & Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Long" & Species == "Rye" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Long" & Species == "Rye" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "a")
text(0.1, 0.75, "f)", cex = 2)

mtext(side = 4, "Prolonged", cex = 1.5, padj = 1)

# g fes double ####

plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = c("Well-Watered","Resistance","Recovery"), cex.axis = 1.5, las = 2)
axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
df<-subset(dat, Experiment == "Double" & Species == "Fes" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "a")
df<-subset(dat, Experiment == "Double" & Species == "Fes" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Double" & Species == "Fes" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "a")
text(0.1, 0.75, "g)", cex = 2)

# h pha double ####
plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = c("Well-Watered","Resistance","Recovery"), cex.axis = 1.5, las = 2)
# axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
df<-subset(dat, Experiment == "Double" & Species == "Pha" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "a")
df<-subset(dat, Experiment == "Double" & Species == "Pha" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "b")
df<-subset(dat, Experiment == "Double" & Species == "Pha" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "a")
text(0.1, 0.75, "h)", cex = 2)

# i lol double ####
plot(xx ~ yy, xlim = c(0,4), ylim = c(-0.35,0.85), xaxt = "n", xlab = "", yaxt = "n", ylab = ""); abline(h = 0, lty = 3)
axis(1, at = seq(1,3,1), labels = c("Well-Watered","Resistance","Recovery"), cex.axis = 1.5, las = 2)
# axis(2, at = seq(-1,1,0.25), cex.axis = 1.25, las = 2)
df<-subset(dat, Experiment == "Double" & Species == "Rye" & Phase == "Control")
rect(0.75, 0, 1.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 1, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(1, 0.8, cex = 1.75, "c")
df<-subset(dat, Experiment == "Double" & Species == "Rye" & Phase == "Resistance")
rect(1.75, 0, 2.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 2, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(2, 0.8, cex = 1.75, "a")
df<-subset(dat, Experiment == "Double" & Species == "Rye" & Phase == "Recovery")
rect(2.75, 0, 3.25, as.numeric(df$CompEffect.mean))
ablineclip(v = 3, y1 = as.numeric(df$CompEffect.mean-df$CompEffect.std.error), y2 = as.numeric(df$CompEffect.mean+df$CompEffect.std.error), lwd = 2)
text(3, 0.8, cex = 1.75, "b")
text(0.1, 0.75, "i)", cex = 2)

mtext(side = 4, "Repeated", cex = 1.5, padj = 1)

# off ####
dev.off()
