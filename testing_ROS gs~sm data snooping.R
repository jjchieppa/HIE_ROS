setwd("F:/Dropbox backup/HIE PhD/Data/Thesis analyses/ROS/Formal Analysis/Manuscript Analysis")
# rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
library(lubridate); library(chron); library(reshape)
library(devtools); library(doBy); library(plotrix)
install_bitbucket("remkoduursma/HIEv"); library(HIEv)
library(reshape)
setToken("TBLyLbiGFy8foqw8Qq4N")
#
# # generate start and end dates
start_date = '2018-05-20'
end_date = '2018-12-30'

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

df<-merge(ROS1, ROS2, by = "Date", all = T)
df<-merge(df, ROS3, by = "Date", all = T)
df<-merge(df, ROS4, by = "Date", all = T)
df<-merge(df, ROS5, by = "Date", all = T)
df<-merge(df, ROS6, by = "Date", all = T)

df<-subset(df, Date == "2018-09-12")
df<-melt(df, id = "Date"); df
