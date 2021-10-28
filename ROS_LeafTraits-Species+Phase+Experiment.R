# in data ####
library(doBy); library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

df<-read.csv("ROS_data_all.csv")
sin<-subset(df, PotType == "FesFes" | PotType == "RyeRye" | PotType == "PhaPha")
mix<-subset(df, PotType == "FesMix" | PotType == "RyeMix" | PotType == "PhaMix")
rm(df)

sin<-summaryBy(SLA_mm2.mg + LDMC_mg.g + Ht_mm ~ Experiment + Phase + Species, FUN = c(mean, std.error), na.rm = T, sin)
mix<-summaryBy(SLA_mm2.mg + LDMC_mg.g + Ht_mm ~ Experiment + Phase + Species, FUN = c(mean, std.error), na.rm = T, mix)

# start ####

tiff(file = "ROS_LeafTraits-Species+Phase+Experiment.tiff", height = 6.5, width = 10, res = 400, units = "in", compression = "zip+p")

par(mfrow = c(3,3), mar = c(1,0,1,0), omi = c(0.5,0.5,0.1,0.1))

xx<-c(-500,500); yy<-xx
# a) fes sla Short ####

yl<-10
yu<-40
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



# b) fes sla Long ####

yl<-10
yu<-40
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





# c) fes sla Double ####

yl<-10
yu<-40
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

legend("topleft", "c)", bty = "n", cex = 1.2)

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
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

axis(2, at = seq(0,400,50), las = 2, cex.axis = 1.2)

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(01, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(02, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(03, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "g)", bty = "n", cex = 1.2)

# g) pha ht Short ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(05, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(06, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(07, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)


# g) lol ht Short ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Short")
par(new=T)
plotCI(09, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
par(new=T)
plotCI(10, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
par(new=T)
plotCI(11, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()



# h) fes ht Long ####

yl<-0
yu<-350
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(01, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(02, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(03, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "h)", bty = "n", cex = 1.2)

# h) pha ht Long ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(05, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(06, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(07, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)


# h) lol ht Long ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Long")
par(new=T)
plotCI(09, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
par(new=T)
plotCI(10, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
par(new=T)
plotCI(11, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()





# i) fes ht Double ####

yl<-0
yu<-350
xl<-0
xu<-12

plot(yy ~ xx, xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "")
axis(1, at = c(1,2,3), labels = F)
axis(1, at = c(5,6,7), labels = F)
axis(1, at = c(9,10,11), labels = F)
abline(v = c(4,8), lty = 2, col = "grey")

dum<-subset(sin, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(01, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(02, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 1, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(03, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 16, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

legend("topleft", "i)", bty = "n", cex = 1.2)

# i) pha ht Double ####

dum<-subset(sin, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(05, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(06, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 0, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(07, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 15, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)


# i) lol ht Double ####

dum<-subset(sin, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Control" & Experiment == "Double")
par(new=T)
plotCI(09, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "dodgerblue4",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
par(new=T)
plotCI(10, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "firebrick",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)

dum<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 2, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
dum<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
par(new=T)
plotCI(11, dum$Ht_mm.mean, dum$Ht_mm.std.error, err="y", pch = 17, col = "orange3",
       xlim = c(xl,xu), ylim = c(yl,yu), axes = F, xlab = "", ylab = "", sfrac = 0, cex = 1.3)
box()


# off ####

dev.off()

