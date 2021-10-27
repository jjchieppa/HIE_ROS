setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
library(lubridate); library(chron); library(reshape)
library(devtools); library(doBy); library(plotrix)
install_bitbucket("remkoduursma/HIEv"); library(HIEv)
setToken("TBLyLbiGFy8foqw8Qq4N")
#
# # generate start and end dates
start_date = '2018-05-20'
end_date = '2018-11-13'

ROS1_swc = downloadTOA5(filename='ROS_AUTO_S01_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS2_swc = downloadTOA5(filename='ROS_AUTO_S02_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS3_swc = downloadTOA5(filename='ROS_AUTO_S03_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS4_swc = downloadTOA5(filename='ROS_AUTO_S04_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS5_swc = downloadTOA5(filename='ROS_AUTO_S05_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS6_swc = downloadTOA5(filename='ROS_AUTO_S06_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)

ROS1<-summaryBy(ROS1_A_F2_Avg+ROS1_A_M3_Avg+ROS1_A_P4_Avg+ROS1_A_R3_Avg+ROS1_S_F2_Avg
                +ROS1_S_M1_Avg+ROS1_S_P1_Avg+ROS1_S_R2_Avg
                ~ Date, FUN = mean, na.rm = T, ROS1_swc)
ROS2<-summaryBy(ROS2_A_F2_Avg+ROS2_A_M1_Avg+ROS2_A_P1_Avg+ROS2_A_R1_Avg+ROS2_L_F4_Avg
                +ROS2_L_M3_Avg+ROS2_L_P3_Avg+ROS2_L_R1_Avg
                ~ Date, FUN = mean, na.rm = T, ROS2_swc)
ROS3<-summaryBy(ROS3_A_F3_Avg+ROS3_A_M3_Avg+ROS3_A_P1_Avg+ROS3_A_R4_Avg+ROS3_X_F3_Avg
                +ROS3_X_M2_Avg+ROS3_X_P4_Avg+ROS3_X_R2_Avg
                ~ Date, FUN = mean, na.rm = T, ROS3_swc)
ROS4<-summaryBy(ROS4_L_F4_Avg+ROS4_L_M2_Avg+ROS4_L_P2_Avg+ROS4_L_R2_Avg+ROS4_S_F3_Avg
                +ROS4_S_M4_Avg+ROS4_S_P3_Avg+ROS4_S_R4_Avg
                ~ Date, FUN = mean, na.rm = T, ROS4_swc)
ROS5<-summaryBy(ROS5_L_F4_Avg+ROS5_L_M1_Avg+ROS5_L_P1_Avg+ROS5_L_R2_Avg+ROS5_X_F1_Avg
                +ROS5_X_M1_Avg+ROS5_X_P2_Avg+ROS5_X_R2_Avg
                ~ Date, FUN = mean, na.rm = T, ROS5_swc)
ROS6<-summaryBy(ROS6_S_F4_Avg+ROS6_S_M4_Avg+ROS6_S_P3_Avg+ROS6_S_R3_Avg+ROS6_X_F2_Avg
                +ROS6_X_M2_Avg+ROS6_X_P2_Avg+ROS6_X_R2_Avg
                ~ Date, FUN = mean, na.rm = T, ROS6_swc)
##########################################################
gs<-read.csv("ROS gs data all.csv")
gs$Date<-as.Date(gs$Date, format = "%d/%m/%Y")

gs<-summaryBy(gs_mmol.m2.s ~ Shelter+Side+Rep+sRep+Date, FUN = max, na.rm = T, gs)
trt<-read.csv("ROS trt assignments.csv")
gs<-merge(gs, trt, by = c("Shelter","Side","Rep","sRep"))
names(gs)[6]<-"gs"
# gs<-subset(gs, Date > "2018-6-20")

con<-subset(gs, Treatment == "Control")
con<-summaryBy(gs ~ Species * Date * Shelter, FUN = max, na.rm = T, con)
all<-subset(gs, Treatment != "Control")
all<-summaryBy(gs ~ Species * Date * Treatment * Count, FUN = max, na.rm = T, all)

crye<-subset(con, Species == "Rye")
cfes<-subset(con, Species == "Fes")
cpha<-subset(con, Species == "Phal")

drye<-subset(all, Species == "Rye")
dfes<-subset(all, Species == "Fes")
dpha<-subset(all, Species == "Phal")

srye<-subset(all, Species == "Rye" & Treatment == "Short")
sfes<-subset(all, Species == "Fes" & Treatment == "Short")
spha<-subset(all, Species == "Phal" & Treatment == "Short")

lrye<-subset(all, Species == "Rye" & Treatment == "Long")
lfes<-subset(all, Species == "Fes" & Treatment == "Long")
lpha<-subset(all, Species == "Phal" & Treatment == "Long")

xrye<-subset(all, Species == "Rye" & Treatment == "Double")
xfes<-subset(all, Species == "Fes" & Treatment == "Double")
xpha<-subset(all, Species == "Phal" & Treatment == "Double")

rm(gs, trt, all, con)

tiff(file = "Visual_ROS gs over time (prolonged).tiff", height = 8, width = 12, res = 600,units = "in", compression = "zip+p")

par(mfrow = c(3,1), mar = c(0, 0, 0, 0), cex.lab = 1.3, omi = c(1, 1, 0.5, 1))

plot(gs.max ~ Date, lrye, pch = "", ylim = c(0,850), xaxt = "n", yaxt = "n")
axis(2, at = seq(0,750,150), cex.axis = 1.5, las = 2)
points(gs.max ~ Date, crye, pch = 1, cex = 2, col = "black")
points(gs.max ~ Date, lrye, pch = 1, cex = 2, col = "red")
abline(v = as.Date("2018-6-20"), lty = 3)
abline(v = as.Date("2018-10-18"), lty = 3)
abline(v = as.Date("2018-11-1"), lty = 3)
abline(v = as.Date("2018-11-14"), lty = 3)
abline(h = 25, lty = 2, col = "grey")
legend("top", expression(italic("L. perenne")), bty = "n", cex = 1.5)

par(new = T)
points(as.numeric(ROS3$ROS3_A_P1_Avg.mean*5000) ~ ROS1$Date, type = "l", lty = 1) #Amb
points(as.numeric(ROS1$ROS1_S_F2_Avg.mean*5000) ~ ROS1$Date, type = "l", lty = 1, col = "red") #short
axis(4, at = seq(0,750,150), labels = c("0","3","6","9","12","15"), cex.axis = 1.5, las = 2)

plot(gs.max ~ Date, lrye, pch = "", ylim = c(0,850), xaxt = "n", yaxt = "n")
axis(2, at = seq(0,750,150), cex.axis = 1.5, las = 2)
points(gs.max ~ Date, cfes, pch = 1, cex = 2, col = "black")
points(gs.max ~ Date, lfes, pch = 1, cex = 2, col = "red")
abline(v = as.Date("2018-6-20"), lty = 3)
abline(v = as.Date("2018-10-18"), lty = 3)
abline(v = as.Date("2018-11-1"), lty = 3)
abline(v = as.Date("2018-11-14"), lty = 3)
abline(h = 25, lty = 2, col = "grey")
legend("top", expression(italic("F. arundinaceae")), bty = "n", cex = 1.5)

par(new = T)
points(as.numeric(ROS3$ROS3_A_P1_Avg.mean*5000) ~ ROS1$Date, type = "l", lty = 1) #Amb
points(as.numeric(ROS1$ROS1_S_F2_Avg.mean*5000) ~ ROS1$Date, type = "l", lty = 1, col = "red") #short
axis(4, at = seq(0,750,150), labels = c("0","3","6","9","12","15"), cex.axis = 1.5, las = 2)

plot(gs.max ~ Date, lrye, pch = "", ylim = c(0,850), xaxt = "n", yaxt = "n")
axis(2, at = seq(0,750,150), cex.axis = 1.5, las = 2)
points(gs.max ~ Date, cpha, pch = 1, cex = 2, col = "black")
points(gs.max ~ Date, lpha, pch = 1, cex = 2, col = "red")
abline(v = as.Date("2018-6-20"), lty = 3)
abline(v = as.Date("2018-10-18"), lty = 3)
abline(v = as.Date("2018-11-1"), lty = 3)
abline(v = as.Date("2018-11-14"), lty = 3)
abline(h = 25, lty = 2, col = "grey")
legend("top", expression(italic("P. aquatica")), bty = "n", cex = 1.5)

par(new = T)
points(as.numeric(ROS3$ROS3_A_P1_Avg.mean*5000) ~ ROS1$Date, type = "l", lty = 1) #Amb
points(as.numeric(ROS1$ROS1_S_F2_Avg.mean*5000) ~ ROS1$Date, type = "l", lty = 1, col = "red") #short
axis(4, at = seq(0,750,150), labels = c("0","3","6","9","12","15"), cex.axis = 1.5, las = 2)

axis.Date(1, lrye$Date, at = seq(as.Date("2018-4-20"), as.Date(max(lrye$Date)), "weeks"),
          cex.axis = 1.5, padj = 0.5, format = "%d/%m", las = 2)
abline(v = as.Date("2018-6-20"), lty = 3)

mtext(side = 1, outer = T, "Date (DD-MM)", cex = 1.5, padj = 4)
mtext(side = 2, outer = T, "Stomatal Conductance", cex = 1.5, padj = -3)
mtext(side = 3, outer = T, "Prolong Drought", cex = 1.5, padj = 0)
mtext(side = 4, outer = T, "Soil Moisture (%)", cex = 1.5, padj = 3)

dev.off()