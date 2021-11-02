# in data ####
library(doBy); library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

df<-read.csv("ROS_data_all.csv")
sin<-subset(df, PotType == "FesFes" | PotType == "RyeRye" | PotType == "PhaPha")
mix<-subset(df, PotType == "FesMix" | PotType == "RyeMix" | PotType == "PhaMix")
rm(df)
source<-read.csv("ROS_data_all.csv")

sin<-summaryBy(SLA_mm2.mg + LDMC_mg.g + Ht_cm ~ Experiment + Phase + Species, FUN = c(mean, std.error), na.rm = T, sin)
mix<-summaryBy(SLA_mm2.mg + LDMC_mg.g + Ht_cm ~ Experiment + Phase + Species, FUN = c(mean, std.error), na.rm = T, mix)

# start ####

tiff(file = "ROS_LeafTraits-Species+Phase+Experiment.tiff", height = 6.5, width = 10, res = 400, units = "in", compression = "zip+p")

par(mfrow = c(3,3), mar = c(1,0,1,0), omi = c(0.75,0.8,0.1,1.4))

xx<-c(-500,500); yy<-xx
# a) fes sla Short ####

yl<-10
yu<-35.0
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

axis(2, at = seq(0,40,5), las = 2, cex.axis = 1.2)

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "a)", bty = "n", cex = 1.2)

mtext(side = 3, "Short-term")

mtext(side = 2, cex = 1.2, expression(SLA~(mm^2~g^-1)), padj = -1.8)

text(01, 28, "c", cex = 1.2)
text(01, 20, "c", cex = 1.2)

text(02, 20, "b", cex = 1.2)
text(02, 12, "a", cex = 1.2)

text(03, 23, "c", cex = 1.2)
text(03, 15, "b", cex = 1.2)


# a) pha sla Short ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

text(05, 33, "bc", cex = 1.2)
text(05, 26, "bc", cex = 1.2)

text(06, 34, "c", cex = 1.2)
text(06, 12, "a", cex = 1.2)

text(07, 31, "bc", cex = 1.2)
text(07, 25, "b", cex = 1.2)

# a) lol sla Short ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

text(09, 28, "abc", cex = 1.2)
text(09, 20, "abc", cex = 1.2)

text(10, 28, "ab", cex = 1.2)
text(10, 20, "b", cex = 1.2)

text(11, 17, "c", cex = 1.2)
text(11, 32, "a", cex = 1.2)


# b) fes sla Long ####

yl<-10
yu<-35.0
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "b)", bty = "n", cex = 1.2)

mtext(side = 3, "Prolonged")

text(01, 17, "b", cex = 1.2)
text(01, 26, "b", cex = 1.2)

text(02, 12, "a", cex = 1.2)
text(02, 25, "b", cex = 1.2)

text(03, 13, "a", cex = 1.2)
text(03, 25, "b", cex = 1.2)


# b) pha sla Long ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

text(05, 32, "b", cex = 1.2)
text(05, 26, "b", cex = 1.2)

text(06, 20, "a", cex = 1.2)
text(06, 32, "b", cex = 1.2)

text(07, 31, "b", cex = 1.2)
text(07, 26, "b", cex = 1.2)

# b) lol sla Long ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

text(09, 26, "a", cex = 1.2)
text(09, 21, "a", cex = 1.2)

text(10, 21, "a", cex = 1.2)
text(10, 31, "b", cex = 1.2)

text(11, 32, "b", cex = 1.2)
text(11, 26, "b", cex = 1.2)



# c) fes sla Double ####

yl<-10
yu<-35.0
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "c)", bty = "n", cex = 1.2, adj = 1.5)

mtext(side = 3, "Repeated")

text(01, 20, "B", cex = 1.2)
text(02, 15, "A", cex = 1.2)
text(03, 15, "A", cex = 1.2)

# inset ####

par(new=T)
par(mar = c(8.5,4,1.6,13.5))

plot(yy ~ xx, xlim = c(0,3), ylim = c(18,23), axes = F, xlab = "", ylab = ""); box()
axis(2, at = seq(15,26,2), cex.axis = 1, las = 2)

hold<-summaryBy(SLA_mm2.mg ~ Species + Type + Experiment, FUN = c(mean, std.error), na.rm = T, source)
dum<-subset(hold, Experiment == "Double" & Species == "Fes" & Type == "Fes")
par(new=T)
plotCI(01, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 1, col = "black",
       xlim = c(0,3), ylim = c(18,23), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(hold, Experiment == "Double" & Species == "Fes" & Type == "Mix")
par(new=T)
plotCI(02, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 16, col = "black",
       xlim = c(0,3), ylim = c(18,23), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
text(1.5, 22.6, "*", cex = 2)

par(mar = c(1,0,1,0))

# c) pha sla Double ####



dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

text(05, 33, "b", cex = 1.2)
text(05, 26, "b", cex = 1.2)

text(06, 34, "b", cex = 1.2)
text(06, 20, "a", cex = 1.2)

text(07, 22, "a", cex = 1.2)
text(07, 31, "b", cex = 1.2)

# c) lol sla Double ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$SLA_mm2.mg.mean, dum$SLA_mm2.mg.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()


text(09, 26, "ab", cex = 1.2)
text(09, 19, "a", cex = 1.2)

text(10, 21, "ab", cex = 1.2)
text(10, 31, "c", cex = 1.2)

text(11, 24, "bc", cex = 1.2)
text(11, 29, "bc", cex = 1.2)


# d) fes ldmc Short ####

yl<-100
yu<-400
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

axis(2, at = seq(0,500,50), las = 2, cex.axis = 1.2)

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "d)", bty = "n", cex = 1.2)

mtext(side = 2, cex = 1.2, expression(LDMC~(g~g^-1)), padj = -1.8)

text(01, 200, "a", cex = 1.2)
text(01, 150, "a", cex = 1.2)

text(02, 310, "c", cex = 1.2)
text(02, 220, "b", cex = 1.2)

text(03, 220, "b", cex = 1.2)
text(03, 270, "bc", cex = 1.2)


# d) pha ldmc Short ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

text(05, 230, "ab", cex = 1.2)
text(05, 180, "ab", cex = 1.2)

text(06, 355, "c", cex = 1.2)
text(06, 150, "a", cex = 1.2)

text(07, 180, "b", cex = 1.2)
text(07, 230, "b", cex = 1.2)

# d) lol ldmc Short ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

text(09, 160, "a", cex = 1.2)
text(09, 240, "ab", cex = 1.2)

text(10, 160, "a", cex = 1.2)
text(10, 330, "d", cex = 1.2)

text(11, 300, "cd", cex = 1.2)
text(11, 220, "bc", cex = 1.2)

# e) fes ldmc Long ####

yl<-100
yu<-400
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "e)", bty = "n", cex = 1.2)

text(01, 150, "a", cex = 1.2)
text(01, 210, "ab", cex = 1.2)

text(02, 290, "d", cex = 1.2)
text(02, 185, "c", cex = 1.2)

text(03, 240, "bc", cex = 1.2)
text(03, 155, "ab", cex = 1.2)

# e) pha ldmc Long ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

text(05, 175, "ab", cex = 1.2)
text(05, 230, "ab", cex = 1.2)

text(06, 200, "c", cex = 1.2)
text(06, 260, "c", cex = 1.2)

text(07, 155, "a", cex = 1.2)
text(07, 250, "bc", cex = 1.2)

# e) lol ldmc Long ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

# f) fes ldmc Double ####

yl<-100
yu<-400
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "f)", bty = "n", cex = 1.2)

text(01, 150, "a", cex = 1.2)
text(01, 200, "a", cex = 1.2)

text(02, 250, "b", cex = 1.2)
text(02, 170, "ab", cex = 1.2)

text(03, 250, "b", cex = 1.2)
text(03, 165, "a", cex = 1.2)

# f) pha ldmc Double ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

# inset ####

par(new=T)
par(mar = c(8.5,9.5,1.6,7.5))

plot(yy ~ xx, xlim = c(0,3), ylim = c(180,230), axes = F, xlab = "", ylab = ""); box()
axis(2, at = seq(180,230,20), cex.axis = 1, las = 2)

hold<-summaryBy(LDMC_mg.g ~ Species + Type + Experiment, FUN = c(mean, std.error), na.rm = T, source)
dum<-subset(hold, Experiment == "Double" & Species == "Pha" & Type == "Pha")
par(new=T)
plotCI(01, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 1, col = "black",
       xlim = c(0,3), ylim = c(180,230), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(hold, Experiment == "Double" & Species == "Pha" & Type == "Mix")
par(new=T)
plotCI(02, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 16, col = "black",
       xlim = c(0,3), ylim = c(180,230), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
text(1.5, 225, "*", cex = 2)

par(mar = c(1,0,1,0))

# f) lol ldmc Double ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$LDMC_mg.g.mean, dum$LDMC_mg.g.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

# g) fes ht Short ####

yl<-0
yu<-350
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = c("Well-watered","Resistance","Recovery"), las = 2)
axis(1, at = c(5,6,7), labels = c("Well-watered","Resistance","Recovery"), las = 2)
axis(1, at = c(9,10,11), labels = c("Well-watered","Resistance","Recovery"), las = 2)
abline(v = c(4,8), lty = 2, col = "grey")

axis(2, at = seq(0,400,50), las = 2, cex.axis = 1.2)

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "g)", bty = "n", cex = 1.2)

mtext(side = 2, cex = 1.2, expression(Height~(mm)), padj = -1.8)

text(01, 130, "A", cex = 1.2)
# text(01, 125, "c", cex = 1.2)

text(02, 80, "A", cex = 1.2)
# text(02, 070, "ab", cex = 1.2)

text(03, 100, "B", cex = 1.2)
# text(03, 020, "a", cex = 1.2)

# g) pha ht Short ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

text(05, 215, "C", cex = 1.2)
# text(05, 145, "c", cex = 1.2)

# text(06, 005, "a", cex = 1.2)
text(06, 070, "A", cex = 1.2)

text(07, 110, "B", cex = 1.2)
# text(07, 015, "ab", cex = 1.2)


# g) lol ht Short ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

text(09, 150, "B", cex = 1.2)
text(10, 110, "A", cex = 1.2)
text(11, 110, "B", cex = 1.2)

# h) fes ht Long ####

yl<-0
yu<-350
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = c("Well-watered","Resistance","Recovery"), las = 2)
axis(1, at = c(5,6,7), labels = c("Well-watered","Resistance","Recovery"), las = 2)
axis(1, at = c(9,10,11), labels = c("Well-watered","Resistance","Recovery"), las = 2)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "h)", bty = "n", cex = 1.2)

text(01, 150, "A", cex = 1.2)
text(02, 110, "B", cex = 1.2)
text(03, 130, "AB", cex = 1.2)

# h) pha ht Long ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

# text(05, 150, "ab", cex = 1.2)
text(05, 235, "B", cex = 1.2)

text(06, 110, "A", cex = 1.2)
# text(06, 005, "a", cex = 1.2)

text(07, 250, "B", cex = 1.2)
# text(07, 070, "a", cex = 1.2)


# h) lol ht Long ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

text(09, 060, "abc", cex = 1.2)
text(09, 130, "abc", cex = 1.2)

text(10, 100, "b", cex = 1.2)
text(10, 005, "a", cex = 1.2)

text(11, 140, "c", cex = 1.2)
text(11, 60, "c", cex = 1.2)



# i) fes ht Double ####

yl<-0
yu<-350
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = c("Well-watered","Resistance","Recovery"), las = 2)
axis(1, at = c(5,6,7), labels = c("Well-watered","Resistance","Recovery"), las = 2)
axis(1, at = c(9,10,11), labels = c("Well-watered","Resistance","Recovery"), las = 2)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "i)", bty = "n", cex = 1.2)

text(01, 060, "bc", cex = 1.2)
text(01, 130, "c", cex = 1.2)

text(02, 90, "ab", cex = 1.2)
text(02, 005, "a", cex = 1.2)

text(03, 130, "c", cex = 1.2)
text(03, 040, "bc", cex = 1.2)



# i) pha ht Double ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

text(05, 250, "C", cex = 1.2)
text(06, 80, "A", cex = 1.2)
text(07, 200, "B", cex = 1.2)

# i) lol ht Double ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$Ht_cm.mean, dum$Ht_cm.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()

text(09, 150, "B", cex = 1.2)
text(10, 100, "A", cex = 1.2)
text(11, 140, "B", cex = 1.2)

# off ####

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 66, 16.5, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab="",ylab="")
legend("top", pch = c(NA,1,0,2,NA,NA,16,15,17), horiz = F, xpd = T, inset = c(0,0), pt.cex = 2, cex = 1.2,
       c("No Competition",
         expression(italic(Festuca)),
         expression(italic(Phalaris)),
         expression(italic(Lolium)),
         "",
         "With Competition",
         expression(italic(Festuca)),
         expression(italic(Phalaris)),
         expression(italic(Lolium))))

dev.off()

