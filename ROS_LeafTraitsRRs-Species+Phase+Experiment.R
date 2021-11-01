# in data ####
library(doBy); library(plotrix); library(SingleCaseES)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

df<-read.csv("ROS_data_all.csv")
sin<-subset(df, PotType == "FesFes" | PotType == "RyeRye" | PotType == "PhaPha")
mix<-subset(df, PotType == "FesMix" | PotType == "RyeMix" | PotType == "PhaMix")
rm(df)
source<-read.csv("ROS_data_all.csv")

# start ####

tiff(file = "ROS_LeafTraitsRRs-Species+Phase+Experiment.tiff", height = 6.5, width = 10, res = 400, units = "in", compression = "zip+p")

par(mfrow = c(3,3), mar = c(2,1,1,1), omi = c(1,1,0.5,0.5))

xx<-c(-500,500); yy<-xx

lx = -0.75
ux = 0.75
ly = 0
uy = 9

# a SLA_mm2.mg Short ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")

# b SLA_mm2.mg Long ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")x

# c SLA_mm2.mg Double ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")

# d LDMC_mg.g Short ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")

# e LDMC_mg.g Long ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")x
# f LDMC_mg.g Double ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")
# reset pars ####
lx = -0.75
ux = 2.25
ly = 0
uy = 9
# g Ht_mm Short ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.5,2,0.5))
# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")

# h Ht_mm Long ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.5,2,0.5))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")x
# i Ht_mm Double ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.5,2,0.5))

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_mm # traits
x2$x<-x2$Ht_mm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")
# off ####

dev.off()
