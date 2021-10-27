library(doBy); library(plotrix)
setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

dat<-read.csv("ROS data all.csv")
dat<-subset(dat, Experiment == "Long")

mix_all<-subset(dat, Type == "Mix")
sin_all<-subset(dat, Type != "Mix")

mix<-summaryBy(AGB_g + BGB_g ~ Species * Phase, FUN = c(mean, std.error), na.rm = T, mix_all)
mixt<-summaryBy(AGB_g + BGB_g ~ Rep * Phase, FUN = sum, na.rm = T, mix_all)
mixt<-summaryBy(AGB_g.sum ~ Phase, FUN = c(mean, std.error), na.rm = T, mixt)
names(mixt)[2]<-"ABG_g.mean"
names(mixt)[3]<-"ABG_g.std.error"

sin<-summaryBy(AGB_g + BGB_g ~ Species * Phase, FUN = c(mean, std.error), na.rm = T, sin_all)
sint<-summaryBy(AGB_g + BGB_g ~ Rep * Phase, FUN = sum, na.rm = T, sin_all)
sint<-summaryBy(AGB_g.sum ~ Phase, FUN = c(mean, std.error), na.rm = T, sint)
names(sint)[2]<-"ABG_g.mean"
names(sint)[3]<-"ABG_g.std.error"

# rm(dat, mix_all, sin_all)

tiff(file = "Figure_ROS BIOMASS LONG AGB + BGB.tiff", height = 10, width = 12, res = 600, units = "in", compression = "zip+p")


# par(mar = c(4,7,1,1))

x<-c(-500,500); y<-c(-500,500)

plot(x ~ y, xaxt = "n", yaxt = "n", pch = 1, xlab = "", ylab = "", xlim = c(1,26.25), ylim = c(-225, 150))
abline(v = 15.5, lty = 2, lwd = 2, col = "grey20")
axis(2, seq(-250,175,25), labels = c(250,225,200,175,150,125,100,75,50,25,0,25,50,75,100,125,150,175))
mtext(side = 2, "Belowground", padj = -3, cex = 1.5, adj = 0.35)
mtext(side = 2, "Aboveground", padj = -3, cex = 1.5, adj = 0.9)
mtext(side = 2, "Dry Biomass (g)", padj = -4.5, cex = 1.85, adj = 0.625)

# axis(1, at = seq(0,100,0.5), las = 2) # for lines

# Aboveground biomass for mixtures
df<-subset(mix, Species == "Rye" & Phase == "Resistance"); v1<-1; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mix, Species == "Fes" & Phase == "Resistance"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mix, Species == "Pha" & Phase == "Resistance"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mixt, Phase == "Resistance"); v1<-v2+.25; v2<-v1+1
rect(v1, 0, v2, df$ABG_g.mean, col = "grey80", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$ABG_g.mean-df$ABG_g.std.error), y2 = (df$ABG_g.mean+df$ABG_g.std.error))

df<-subset(mix, Species == "Rye" & Phase == "Recovery"); v1<-v2+1; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mix, Species == "Fes" & Phase == "Recovery"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mix, Species == "Pha" & Phase == "Recovery"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mixt, Phase == "Recovery"); v1<-v2+.25; v2<-v1+1
rect(v1, 0, v2, df$ABG_g.mean, col = "grey80", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$ABG_g.mean-df$ABG_g.std.error), y2 = (df$ABG_g.mean+df$ABG_g.std.error))

df<-subset(mix, Species == "Rye" & Phase == "Control"); v1<-v2+1; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mix, Species == "Fes" & Phase == "Control"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mix, Species == "Pha" & Phase == "Control"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(mixt, Phase == "Control"); v1<-v2+.25; v2<-v1+1
rect(v1, 0, v2, df$ABG_g.mean, col = "grey80", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$ABG_g.mean-df$ABG_g.std.error), y2 = (df$ABG_g.mean+df$ABG_g.std.error))

# Aboveground biomass for singles
df<-subset(sin, Species == "Rye" & Phase == "Resistance"); v1<-16; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(sin, Species == "Fes" & Phase == "Resistance"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(sin, Species == "Pha" & Phase == "Resistance"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))


df<-subset(sin, Species == "Rye" & Phase == "Recovery"); v1<-v2+1; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(sin, Species == "Fes" & Phase == "Recovery"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(sin, Species == "Pha" & Phase == "Recovery"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))


df<-subset(sin, Species == "Rye" & Phase == "Control"); v1<-v2+1; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(sin, Species == "Fes" & Phase == "Control"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))
df<-subset(sin, Species == "Pha" & Phase == "Control"); v1<-v2+.25; v2<-v1+0.75
rect(v1, 0, v2, df$AGB_g.mean, density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5, y1 = (df$AGB_g.mean-df$AGB_g.std.error), y2 = (df$AGB_g.mean+df$AGB_g.std.error))

# BGB data

# Belowground biomass for mixtures
mixroot<-summaryBy(BGB_g ~ Phase, FUN = c(mean, std.error), na.rm = T, mix_all)

df<-subset(mixroot, Phase == "Resistance"); v1<-4; v2<-v1+1
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), col = "grey80", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))

df<-subset(mixroot, Phase == "Recovery"); v1<-v2+4; v2<-v1+1
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), col = "grey80", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))

df<-subset(mixroot, Phase == "Control"); v1<-v2+4; v2<-v1+1
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), col = "grey80", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))

# Belowground biomass for singles

df<-subset(sin, Species == "Rye" & Phase == "Resistance"); v1<-v2+1; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))
df<-subset(sin, Species == "Fes" & Phase == "Resistance"); v1<-v2+.25; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))
df<-subset(sin, Species == "Pha" & Phase == "Resistance"); v1<-v2+.25; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))


df<-subset(sin, Species == "Rye" & Phase == "Recovery"); v1<-v2+1; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))
df<-subset(sin, Species == "Fes" & Phase == "Recovery"); v1<-v2+.25; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))
df<-subset(sin, Species == "Pha" & Phase == "Recovery"); v1<-v2+.25; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))


df<-subset(sin, Species == "Rye" & Phase == "Control"); v1<-v2+1; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 00, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))
df<-subset(sin, Species == "Fes" & Phase == "Control"); v1<-v2+.25; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 10, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))
df<-subset(sin, Species == "Pha" & Phase == "Control"); v1<-v2+.25; v2<-v1+.75
rect(v1, 0, v2, as.numeric(df$BGB_g.mean*-1), density = 30, col = "black", lty = 1, border = 1)
ablineclip(v = (v1+v2)/2, lwd = 1.5,
           y1 = as.numeric((df$BGB_g.mean-df$BGB_g.std.error)*-1),
           y2 = as.numeric((df$BGB_g.mean+df$BGB_g.std.error)*-1))


# Labels
axis(1, at = c(3,8,13,17.25,21.25,24.75), labels = c("Resistance","Recovery","Ambient","Resistance","Recovery","Ambient"),
     cex.axis = 1.5, padj = -50, tck = F)
axis(1, at = c(7.75, 23.25), labels = c("Mixed Species", "Single Species"), cex.axis = 1.5)
abline(h = 0, lty = 1, col = "black", lwd = 2)
abline(v = c(5.5,10.5,19.25,23), lty = 2, lwd = 1, col = "grey80")

# Legend
legend("bottomleft", cex = 1.75, fill = c("white","white","white","grey80"),
       c(expression(italic("L. perenne")),
         expression(italic("F. arundinacea")),
         expression(italic("P. aquatica")),
         "Total"))

legend("bottomleft", cex = 1.75, density = c(0, 10, 30, 0), bg = NA,
       c(expression(italic("L. perenne")),
         expression(italic("F. arundinacea")),
         expression(italic("P. aquatica")),
         "Total"))
dev.off()
