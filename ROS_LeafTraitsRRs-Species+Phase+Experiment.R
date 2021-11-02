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

par(mfrow = c(3,3), mar = c(2,1,1,1), omi = c(0.5,0.7,0.6,0.1))

xx<-c(-500,500); yy<-xx

lx = -0.75
ux = 0.75
ly = 0
uy = 9

# a SLA_mm2.mg Short ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2,
     labels = c(expression(italic(Festuca)),
                expression(italic(Phalaris)),
                expression(italic(Lolium))))
mtext(side = 3, "Short")
legend("topleft", "a)", bty = "n", cex = 1.2)

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
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "black")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "black")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "black")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "black")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "black")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "black")

# b SLA_mm2.mg Long ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2, labels = F)
mtext(side = 3, "Prolonged")
legend("topleft", "b)", bty = "n", cex = 1.2)

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
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "black")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "black")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "black")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "black")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "black")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "black")

# c SLA_mm2.mg Double ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2, labels = F)
mtext(side = 3, "Repeated")
legend("topleft", "c)", bty = "n", cex = 1.2)

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
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "black")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "black")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "black")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "black")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$SLA_mm2.mg # traits
x2$x<-x2$SLA_mm2.mg # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "black")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "black")


# d LDMC_mg.g Short ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2,
     labels = c(expression(italic(Festuca)),
                expression(italic(Phalaris)),
                expression(italic(Lolium))))
legend("topleft", "d)", bty = "n", cex = 1.2)

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "black")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "black")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "black")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "black")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "black")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "black")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "black")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "black")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "black")

# e LDMC_mg.g Long ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.75,0.75,0.25))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2, labels = F)
legend("topleft", "e)", bty = "n", cex = 1.2)

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
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "black")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "black")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "black")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
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
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2, labels = F)
legend("topleft", "f)", bty = "n", cex = 1.2)

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

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$LDMC_mg.g # traits
x2$x<-x2$LDMC_mg.g # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
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

# g Ht_cm Short ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.5,2,0.5))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2,
     labels = c(expression(italic(Festuca)),
                expression(italic(Phalaris)),
                expression(italic(Lolium))))
legend("topleft", "g)", bty = "n", cex = 1.2)

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Short")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Short")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")

# h Ht_cm Long ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.5,2,0.5))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2, labels = F)
legend("topleft", "h)", bty = "n", cex = 1.2)

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "grey70")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "grey70")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "grey70")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "black")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Long")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Long")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "black")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "black")x

# i Ht_cm Double ####

plot(yy ~ xx, axes = F, xlim = c(lx,ux), ylim = c(ly,uy), xlab = "", ylab = ""); box()
abline(v = 0, lty = 2, col = "grey")
axis(1, at = seq(-0.5,2,0.5))
axis(2, at = c(7.5,4.5,1.5), cex.axis = 1.2, las = 2, labels = F)
legend("topleft", "i)", bty = "n", cex = 1.2)

# fes
x1<-subset(sin, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 8, cex = 1.2, pch = 1, col = "black")
ablineclip(h = 8, x1 = est.l, x2 = est.u, col = "black")

x1<-subset(mix, Species == "Fes" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Fes" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 7, cex = 1.2, pch = 16, col = "black")
ablineclip(h = 7, x1 = est.l, x2 = est.u, col = "black")

# Pha
x1<-subset(sin, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 5, cex = 1.2, pch = 0, col = "grey70")
ablineclip(h = 5, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Pha" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Pha" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 4, cex = 1.2, pch = 15, col = "grey70")
ablineclip(h = 4, x1 = est.l, x2 = est.u, col = "grey70")

# Rye
x1<-subset(sin, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(sin, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 2, cex = 1.2, pch = 2, col = "grey70")
ablineclip(h = 2, x1 = est.l, x2 = est.u, col = "grey70")

x1<-subset(mix, Species == "Rye" & Phase == "Resistance" & Experiment == "Double")
x2<-subset(mix, Species == "Rye" & Phase == "Recovery" & Experiment == "Double")
x1$x<-x1$Ht_cm # traits
x2$x<-x2$Ht_cm # traits
est.x<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[2])
est.l<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[4])
est.u<-as.numeric(LRRd(A_data = x1$x, B_data = x2$x, bias_correct = TRUE)[5])
points(x = est.x, y = 1, cex = 1.2, pch = 17, col = "grey70")
ablineclip(h = 1, x1 = est.l, x2 = est.u, col = "grey70")


# off ####

mtext(side = 1, outer = T, cex = 1.2, "Log Response Ratio", padj = 1.5)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 8, 1, 5), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab="",ylab="")
legend("top", pch = c(NA,1,0,2,NA,16,15,17), horiz = T, xpd = T, inset = c(0,0), pt.cex = 2, cex = 1,
       c("No Competition",
         expression(italic(Festuca)),
         expression(italic(Phalaris)),
         expression(italic(Lolium)),
         "With Competition",
         expression(italic(Festuca)),
         expression(italic(Phalaris)),
         expression(italic(Lolium))))


dev.off()
