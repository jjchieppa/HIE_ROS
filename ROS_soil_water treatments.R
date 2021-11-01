
rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# in data ####

weeks1<-c(-2,-1,0,1,2,3,4,4.1, 6,7,8,9,10,11,12,    13,14,15,16,17)
weeks2<-c(-2,-1,0,1,2,3,4,   5,6,7,8,9,10,11,12,12.1,14,15,16,17)
weeks3<-c(-2,-1,0,1,2,3,4,4.1, 6,7,8,9,10,11,12,12.1,  14,15,16,17)
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
      y-10,y-y,y-y,y+.3,
      x,y,x,y,
      x,y,x,y,
      x,y,x,y)
d2<-c(x,y,x,y-5,
      y-10,y-y,y-y,y-y,
      0,0,0,0,
      0,0,0,y+.3,
      x,y,x,y)
d3<-c(x,y,x,y-5,
      y-10,y-y,y-y,y+.3,
      x,y,x,y-5,
      y-10,y-y,y-y,y+.3,
      x,y,x,y)
df<-data.frame(weeks1,weeks2,weeks3,ambient,d1,d2,d3)

x<-c(-500,-500)
y<-c(-500,-500)

# start ####

tiff(file = "ROS soil water treatments.tiff", height = 8, width = 10, res = 400, units = "in", compression = "zip+p")

par(mfrow = c(3,1), mar = c(1, 0, 1, 0), omi = c(0.5, 0.5, 0.1, 0.1))

# a short ####

plot(y ~ x, xlim = c(-2,16), ylim = c(0,15),
     xlab = "", xaxt = "n",
     ylab = "", yaxt = "n")
axis(2, at = c(0,4,8,12), las = 2, cex.axis = 1.25)

dat<-subset(df, weeks1 <= 6)
points(dat$ambient ~ dat$weeks1, type = "l", lty = 1, col = "dodgerblue4", lwd = 4)
dat<-subset(df, weeks1 <= 0)
points(dat$d1 ~ dat$weeks1, type = "l", lty = 1, col = "darkgrey", lwd = 2)
dat<-subset(df, weeks1 <= 4 & weeks1 >= 0)
points(dat$d1 ~ dat$weeks1, type = "l", lty = 1, col = "firebrick", lwd = 2)
dat<-subset(df, weeks1 >= 4 & weeks1 <= 6)
points(dat$d1 ~ dat$weeks1, type = "l", lty = 1, col = "orange3", lwd = 2)

abline(v = c(0, 2, 4, 6), col = "grey20", lty = 2)
text(x = -0.2, y = 7, srt = 90, "Commencement", cex = 1.1)
text(x = 1.8, y = 7.2, srt = 90, "1st species to 0 gs", cex = 1.1)
text(x = 3, y = 2.2, "2 weeks later", cex = 1.1)
text(x = 3.8, y = 7, srt = 90, "Resistance harvests", cex = 1.1)
text(x = 5, y = 2.2, "2 weeks later", cex = 1.1)
text(x = 6.2, y = 7, srt = 90, "Recovery harvests", cex = 1.1)

arrows(x0 = 5, x1 = 5, y0 = 9.5, y1 = 11.5, lwd = 2, length = 0.15, col = "dodgerblue4")

legend("topleft", "a) Short", cex = 1.2, bty = "n")

axis(1, at = c(-2,0,2,4,6,8,10,12,14,16), labels = F)

legend("right", bty = "n", c("Well-watered","Pre-Treatment","Resistance Phase", "Recovery Phase"),
       lty = c(1,1,1,1), lwd = c(5,3,3,3), col = c("dodgerblue4","darkgrey","firebrick","orange3"), cex = 1.5)

# b prolonged ####

plot(y ~ x, xlim = c(-2,16), ylim = c(0,15),
     xlab = "", xaxt = "n",
     ylab = "", yaxt = "n")
axis(2, at = c(0,4,8,12), las = 2, cex.axis = 1.25)

dat<-subset(df, weeks2 <= 16)
points(dat$ambient ~ dat$weeks2, type = "l", lty = 1, col = "dodgerblue4", lwd = 4)
dat<-subset(df, weeks2 <= 0)
points(dat$d2 ~ dat$weeks2, type = "l", lty = 1, col = "darkgrey", lwd = 2)
dat<-subset(df, weeks2 <= 12 & weeks2 >= 0)
points(dat$d2 ~ dat$weeks2, type = "l", lty = 1, col = "firebrick", lwd = 2)
dat<-subset(df, weeks2 >= 12 & weeks2 <= 16)
points(dat$d2 ~ dat$weeks2, type = "l", lty = 1, col = "orange3", lwd = 2)

arrows(x0 = 14, x1 = 14, y0 = 9, y1 = 11, lwd = 2, length = 0.15, col = "dodgerblue4")

abline(v = c(0, 10, 12, 16), col = "grey20", lty = 2)
text(x = -0.2, y = 7, srt = 90, "Commencement", cex = 1.1)
text(x = 9.8, y = 7, srt = 90, "All species to 0 gs", cex = 1.1)
text(x = 11, y = 2, "2 weeks later", cex = 1.1)
text(x = 11.8, y = 7, srt = 90, "Resistance harvests", cex = 1.1)
text(x = 14, y = 2, "4 weeks later", cex = 1.1)
text(x = 16.2, y = 7, srt = 90, "Recovery harvests", cex = 1.1)
legend("topleft", "b) Prolonged", cex = 1.2, bty = "n")

axis(1, at = c(-2,0,2,4,6,8,10,12,14,16), labels = F)

# c repeated ####

plot(y ~ x, xlim = c(-2,16), ylim = c(0,15),
     xlab = "", xaxt = "n",
     ylab = "", yaxt = "n")
axis(2, at = c(0,4,8,12), las = 2, cex.axis = 1.25)

dat<-subset(df, weeks3 <= 16)
points(dat$ambient ~ dat$weeks3, type = "l", lty = 1, col = "dodgerblue4", lwd = 4)

dat<-subset(df, weeks3 <= 0)
points(dat$d3 ~ dat$weeks3, type = "l", lty = 1, col = "darkgrey", lwd = 2)
dat<-subset(df, weeks3 <= 4 & weeks3 >= 0)
points(dat$d3 ~ dat$weeks3, type = "l", lty = 1, col = "firebrick", lwd = 2)
dat<-subset(df, weeks3 <= 6 & weeks3 >= 4)
points(dat$d3 ~ dat$weeks3, type = "l", lty = 1, col = "forestgreen", lwd = 2)
dat<-subset(df, weeks3 <= 12 & weeks3 >= 6)
points(dat$d3 ~ dat$weeks3, type = "l", lty = 1, col = "firebrick", lwd = 2)
dat<-subset(df, weeks3 >= 12 )
points(dat$d3 ~ dat$weeks3, type = "l", lty = 1, col = "orange3", lwd = 2)

arrows(x0 = 14, x1 = 14, y0 = 9, y1 = 11, lwd = 2, length = 0.15, col = "dodgerblue4")

abline(v = c(0, 2, 4, 10, 12, 16), col = "grey20", lty = 2)
text(x = -.15, y = 8, srt = 90, "Commencement", cex = 1.1)
text(x = 1.85, y = 8, srt = 90, "1st species to 0 gs", cex = 1.1)
text(x = 3, y = 3, "2 weeks later", cex = 1.1)
text(x = 3.85, y = 8, srt = 90, "Re-watering", cex = 1.1)
text(x = 9.85, y = 8, srt = 90, "1st species to 0 gs", cex = 1.1)
text(x = 11, y = 3, "2 weeks later", cex = 1.1)
text(x = 11.85, y = 8, srt = 90, "Resistance harvests", cex = 1.1)
text(x = 14, y = 3, "4 weeks later", cex = 1.1)
text(x = 16.15, y = 8, srt = 90, "Recovery harvests", cex = 1.1)
legend("topleft", "c) Repeated", cex = 1.2, bty = "n")

legend("bottom", bty = "n", c("Recovery period"), lty = 1, lwd = 3, col = "forestgreen", cex = 1.5)

axis(1, at = c(-2,0,2,4,6,8,10,12,14,16), labels = c("n-2","n","n+2","n+4","n+6","n+8","n+10","n+12","n+14","n+16"), cex.axis = 1.5)
mtext(side = 1, "Weeks", cex = 1.05, padj = 2.5, outer = T)
mtext(side = 2, "Soil Moisture (%)", cex = 1.05, padj = -2.5, outer = T)

# off ####
dev.off()
