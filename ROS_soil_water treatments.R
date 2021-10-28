setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
tiff(file = "ROS soil water treatments.tiff",
height = 6.5, width = 10, res = 400, units = "in", compression = "zip+p")
##################################
weeks<-c(-2:17)
#31 days to go from 17.5% to 10% (2.42/day)
#38 days to go from 25% to %7 (4.74%/day)
#13 days to go from 15% to 5% (7.69%/day)
#CWMT would be 4.33%/day....so let's say 2.5%/day
#so per week, let's say 5
x<-11.75
y<-12.25
ambient<-c(x,y,x,y,
           x,y,x,y,
           x,y,x,y,
           x,y,x,y,
           x,y,x,y)
d1<-c(x,y,x,y-5,
      y-10,y-y,y-y,y+2,
      x,y,x,y,
      x,y,x,y,
      x,y,x,y)
d2<-c(x,y,x,y-5,
      y-10,y-y,y-y,y-y,
      0,0,0,0,
      0,0,0,y+2,
      x,y,x,y)
d3<-c(x,y,x,y-5,
      y-10,y-y,y-y,y+2,
      x,y,x,y-5,
      y-10,y-y,y-y,y+2,
      x,y,x,y)
df<-data.frame(weeks,ambient,d1,d2,d3)

x<-c(-500,-500)
y<-c(-500,-500)

par(mfrow = c(4,1), mar = c(0, 0, 0, 0), omi = c(0.5, 0.5, 0.1, 0.1))
plot(y ~ x, xlim = c(-2,16), ylim = c(0,15),
     xlab = "", xaxt = "n",
     ylab = "", yaxt = "n")
dat<-subset(df, weeks <= 14)
points(dat$ambient ~ dat$weeks, type = "l", lty = 3, col = "grey60", lwd = 2)
axis(2, at = c(0,4,8,12), las = 2, cex.axis = 1.25)
# legend("bottomleft", "Control", cex = 1.7, bty = "n")
abline(v = c(0,5,14), col = "grey20")
text(x = -.15, y = 8, srt = 90, "Commencement", cex = 1.1)
text(x = 4.85, y = 8, srt = 90, "Short control harvest", cex = 1.1)
text(x = 13.65, y = 8, srt = 90, "Prolonged control", cex = 1.1); text(x = 13.85, y = 8, srt = 90, "harvest", cex = 1.1)
text(x = 14.15, y = 8, srt = 90, "Repeated control", cex = 1.1); text(x = 14.30, y = 8, srt = 90, "harvest", cex = 1.1)
legend("topleft", "a. Control", cex = 1.3, bty = "n")

###########################################

plot(y ~ x, xlim = c(-2,16), ylim = c(0,15),
     xlab = "", xaxt = "n",
     ylab = "", yaxt = "n")
axis(2, at = c(0,4,8,12), las = 2, cex.axis = 1.25)
dat<-subset(df, weeks <= 6)
points(dat$d1 ~ dat$weeks, type = "l", lty = 3, col = "grey60", lwd = 2)
# legend("bottomleft", "Short", cex = 1.7, bty = "n")
abline(v = c(0, 2, 4, 6), col = "grey20")
text(x = -.15, y = 8, srt = 90, "Commencement", cex = 1.1)
text(x = 1.85, y = 8, srt = 90, "1st species to 0 gs", cex = 1.1)
text(x = 3, y = 3, "2 weeks later", cex = 1.1)
text(x = 3.85, y = 8, srt = 90, "Resistance harvests", cex = 1.1)
text(x = 5, y = 3, "2 weeks later", cex = 1.1)
text(x = 6.15, y = 8, srt = 90, "Recovery harvests", cex = 1.1)
legend("topleft", "b. Short", cex = 1.3, bty = "n")

###########################################

plot(y ~ x, xlim = c(-2,16), ylim = c(0,15),
     xlab = "", xaxt = "n",
     ylab = "", yaxt = "n")
axis(2, at = c(0,4,8,12), las = 2, cex.axis = 1.25)
dat<-subset(df, weeks <= 16)
points(dat$d2 ~ dat$weeks, type = "l", lty = 3, col = "grey60", lwd = 2)
# legend("bottomleft", "Prolonged", cex = 1.7, bty = "n")
abline(v = c(0, 10, 12, 16), col = "grey20")
text(x = -.15, y = 8, srt = 90, "Commencement", cex = 1.1)
text(x = 9.85, y = 8, srt = 90, "3rd species to 0 gs", cex = 1.1)
text(x = 11, y = 3, "2 weeks later", cex = 1.1)
text(x = 11.85, y = 8, srt = 90, "Resistance harvests", cex = 1.1)
text(x = 14, y = 3, "4 weeks later", cex = 1.1)
text(x = 16.15, y = 8, srt = 90, "Recovery harvests", cex = 1.1)
legend("topleft", "c. Prolonged", cex = 1.3, bty = "n")

###########################################

plot(y ~ x, xlim = c(-2,16), ylim = c(0,15),
     xlab = "", xaxt = "n",
     ylab = "", yaxt = "n")
axis(2, at = c(0,4,8,12), las = 2, cex.axis = 1.25)
dat<-subset(df, weeks <= 16)
points(dat$d3 ~ dat$weeks, type = "l", lty = 3, col = "grey60", lwd = 2)
# legend("bottomleft", "Repeated", cex = 1.7, bty = "n")
abline(v = c(0, 2, 4, 10, 12, 16), col = "grey20")
text(x = -.15, y = 8, srt = 90, "Commencement", cex = 1.1)
text(x = 1.85, y = 8, srt = 90, "1st species to 0 gs", cex = 1.1)
text(x = 3, y = 3, "2 weeks later", cex = 1.1)
text(x = 3.85, y = 8, srt = 90, "Re-watering", cex = 1.1)
text(x = 9.85, y = 8, srt = 90, "1st species to 0 gs", cex = 1.1)
text(x = 11, y = 3, "2 weeks later", cex = 1.1)
text(x = 11.85, y = 8, srt = 90, "Resistance harvests", cex = 1.1)
text(x = 14, y = 3, "4 weeks later", cex = 1.1)
text(x = 16.15, y = 8, srt = 90, "Recovery harvests", cex = 1.1)
legend("topleft", "d. Repeated", cex = 1.3, bty = "n")


axis(1, at = c(-2,0,2,4,6,8,10,12,14,16), labels = c("n-2","n","n+2","n+4","n+6","n+8","n+10","2+12","n+14","n+16"), cex.axis = 1.5)
mtext(side = 1, "Weeks", cex = 1.05, padj = 2.5, outer = T)
mtext(side = 2, "Soil Moisture (%)", cex = 1.05, padj = -2.5, outer = T)
dev.off()
