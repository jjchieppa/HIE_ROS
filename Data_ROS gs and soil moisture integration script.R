# start ####
rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

library(lubridate)
library(chron); library(reshape)
library(devtools); library(doBy); library(plotrix)
library(HIEv)
library(stringr)
install_bitbucket("remkoduursma/HIEv"); 
setToken("TBLyLbiGFy8foqw8Qq4N")

# HIEv download ####

# # # generate start and end dates
start_date = '2018-06-20'; end_date = '2018-10-31'
ROS1_swc = downloadTOA5(filename='ROS_AUTO_S01_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS2_swc = downloadTOA5(filename='ROS_AUTO_S02_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS3_swc = downloadTOA5(filename='ROS_AUTO_S03_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS4_swc = downloadTOA5(filename='ROS_AUTO_S04_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS5_swc = downloadTOA5(filename='ROS_AUTO_S05_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)
ROS6_swc = downloadTOA5(filename='ROS_AUTO_S06_SOILVARS', startDate=start_date, endDate=end_date, keepFiles=FALSE)

ROS1<-summaryBy(ROS1_A_F2_Avg+ROS1_A_P4_Avg+ROS1_A_R3_Avg+ROS1_S_F2_Avg+ROS1_S_P1_Avg+ROS1_S_R2_Avg
                ~ Date, FUN = median, na.rm = T, ROS1_swc)
ROS2<-summaryBy(ROS2_A_F2_Avg+ROS2_A_P1_Avg+ROS2_A_R1_Avg+ROS2_L_F4_Avg+ROS2_L_P3_Avg+ROS2_L_R1_Avg
                ~ Date, FUN = median, na.rm = T, ROS2_swc)
ROS3<-summaryBy(ROS3_A_F3_Avg+ROS3_A_P1_Avg+ROS3_A_R4_Avg+ROS3_X_F3_Avg+ROS3_X_P4_Avg+ROS3_X_R2_Avg
                ~ Date, FUN = median, na.rm = T, ROS3_swc)
ROS4<-summaryBy(ROS4_L_F4_Avg+ROS4_L_P2_Avg+ROS4_L_R2_Avg+ROS4_S_F3_Avg+ROS4_S_P3_Avg+ROS4_S_R4_Avg
                ~ Date, FUN = median, na.rm = T, ROS4_swc)
ROS5<-summaryBy(ROS5_L_F4_Avg+ROS5_L_P1_Avg+ROS5_L_R2_Avg+ROS5_X_F1_Avg+ROS5_X_P2_Avg+ROS5_X_R2_Avg
                ~ Date, FUN = median, na.rm = T, ROS5_swc)
ROS6<-summaryBy(ROS6_S_F4_Avg+ROS6_S_P3_Avg+ROS6_S_R3_Avg+ROS6_X_F2_Avg+ROS6_X_P2_Avg+ROS6_X_R2_Avg
                ~ Date, FUN = median, na.rm = T, ROS6_swc)

############################################################################
############################################################################ Soil moisture data prep

c1<-ROS1[,1:4]
c2<-ROS2[,1:4]
c3<-ROS3[,1:4]
s1<-ROS2[,c(1,5:7)]
s2<-ROS4[,1:4]
s3<-ROS6[,1:4]
l1<-ROS1[,c(1,5:7)]
l2<-ROS4[,c(1,5:7)]
l3<-ROS6[,1:4]
x1<-ROS3[,c(1,5:7)]
x2<-ROS5[,c(1,5:7)]
x3<-ROS6[,c(1,5:7)]

rm(ROS1, ROS2, ROS3, ROS4, ROS5, ROS6, ROS1_swc, ROS2_swc, ROS3_swc, ROS4_swc, ROS5_swc, ROS6_swc)

con<-merge(c1, c2, by = "Date", all = T); con<-merge(con, c3, by = "Date", all = T)
sho<-merge(s1, s2, by = "Date", all = T); sho<-merge(sho, s3, by = "Date", all = T)
lon<-merge(l1, l2, by = "Date", all = T); lon<-merge(lon, l3, by = "Date", all = T)
dou<-merge(x1, x2, by = "Date", all = T); dou<-merge(dou, x3, by = "Date", all = T)

rm(list = ls()[!ls() %in% c("con","sho","lon","dou")])

names(con)[2]<-"1c-Fes"
names(con)[3]<-"1c-Phal"
names(con)[4]<-"1c-Rye"
names(con)[5]<-"2c-Fes"
names(con)[6]<-"2c-Phal"
names(con)[7]<-"2c-Rye"
names(con)[8]<-"3c-Fes"
names(con)[9]<-"3c-Phal"
names(con)[10]<-"3c-Rye"
con<-melt(con, id = c("Date"))
names(con)[2]<-"UID"
names(con)[3]<-"SoilM"
# con<-summaryBy(SoilM ~ Date * Spp, FUN = c(median, std.error), na.rm = T, con)

names(sho)[2]<-"1s-Fes"
names(sho)[3]<-"1s-Phal"
names(sho)[4]<-"1s-Rye"
names(sho)[5]<-"2s-Fes"
names(sho)[6]<-"2s-Phal"
names(sho)[7]<-"2s-Rye"
names(sho)[8]<-"3s-Fes"
names(sho)[9]<-"3s-Phal"
names(sho)[10]<-"3s-Rye"
sho<-melt(sho, id = c("Date"))
names(sho)[2]<-"UID"
names(sho)[3]<-"SoilM"
# sho<-summaryBy(SoilM ~ Date * Spp, FUN = c(median, std.error), na.rm = T, sho)

names(lon)[2]<-"1l-Fes"
names(lon)[3]<-"1l-Phal"
names(lon)[4]<-"1l-Rye"
names(lon)[5]<-"2l-Fes"
names(lon)[6]<-"2l-Phal"
names(lon)[7]<-"2l-Rye"
names(lon)[8]<-"3l-Fes"
names(lon)[9]<-"3l-Phal"
names(lon)[10]<-"3l-Rye"
lon<-melt(lon, id = c("Date"))
names(lon)[2]<-"UID"
names(lon)[3]<-"SoilM"
# lon<-summaryBy(SoilM ~ Date * Spp, FUN = c(median, std.error), na.rm = T, lon)

names(dou)[2]<-"1x-Fes"
names(dou)[3]<-"1x-Phal"
names(dou)[4]<-"1x-Rye"
names(dou)[5]<-"2x-Fes"
names(dou)[6]<-"2x-Phal"
names(dou)[7]<-"2x-Rye"
names(dou)[8]<-"3x-Fes"
names(dou)[9]<-"3x-Phal"
names(dou)[10]<-"3x-Rye"
dou<-melt(dou, id = c("Date"))
names(dou)[2]<-"UID"
names(dou)[3]<-"SoilM"
# dou<-summaryBy(SoilM ~ Date * Spp, FUN = c(median, std.error), na.rm = T, dou)

conSM<-con; rm(con)
droSM<-rbind(sho,lon,dou)
rm(sho, lon, dou)

cfesSM<-subset(conSM, UID == "1c-Fes" | UID == "2c-Fes" | UID == "3c-Fes")
cphaSM<-subset(conSM, UID == "1c-Phal" | UID == "2c-Phal" | UID == "3c-Phal")
cryeSM<-subset(conSM, UID == "1c-Rye" | UID == "2c-Rye" | UID == "3c-Rye")

dfesSM<-subset(droSM, UID == "1s-Fes" | UID == "2s-Fes" | UID == "3s-Fes" |
                 UID == "1l-Fes" | UID == "2l-Fes" | UID == "3l-Fes" |
                 UID == "1x-Fes" | UID == "2x-Fes" | UID == "3x-Fes")
dphaSM<-subset(droSM, UID == "1s-Phal" | UID == "2s-Phal" | UID == "3s-Phal" |
                 UID == "1l-Phal" | UID == "2l-Phal" | UID == "3l-Phal" |
                 UID == "1x-Phal" | UID == "2x-Phal" | UID == "3x-Phal")
dryeSM<-subset(droSM, UID == "1s-Rye" | UID == "2s-Rye" | UID == "3s-Rye" |
                 UID == "1l-Rye" | UID == "2l-Rye" | UID == "3l-Rye" |
                 UID == "1x-Rye" | UID == "2x-Rye" | UID == "3x-Rye")

rm(conSM, droSM)


############################################################################
############################################################################ Conductance data prep

gs<-read.csv("ROS gs data all.csv")
gs<-summaryBy(gs_mmol.m2.s+Temp_C+RH_leaf ~ Shelter+Side+Rep+sRep+Date, FUN = max, na.rm = T, gs)
trt<-read.csv("ROS trt assignments.csv")
gs<-merge(gs, trt, by = c("Shelter","Side","Rep","sRep"))
names(gs)[6]<-"gs"
names(gs)[7]<-"Temp.C"
names(gs)[8]<-"RH.leaf"
rm(trt)
gs$Date<-as.Date(gs$Date, format = "%d/%m/%Y")
gs<-subset(gs, Spp == "Fes" | Spp == "Rye" | Spp == "Phal")
gs$SVP.Pa<-610.7*(10^(((7.5*gs$Temp.C)/(237.3+gs$Temp.C))))
gs$VPD<-((100-gs$RH.leaf)/100)*gs$SVP.Pa
gs$VPD.kpa<-gs$VPD/1000

gs<-summaryBy(gs + VPD.kpa ~ Date * UID * Treatment * Spp * Shelter * Rep * sRep * Side, FUN = c(max), na.rm = T, gs)
# names(gs)[5]<-"gs"
# names(gs)[6]<-"VPD.kpa"

conGS<-subset(gs, Treatment == "Control")
droGS<-subset(gs, Treatment != " Control")
rm(gs)

cfesGS<-subset(conGS, Spp == "Fes")
cphaGS<-subset(conGS, Spp == "Phal")
cryeGS<-subset(conGS, Spp == "Rye")
dfesGS<-subset(droGS, Spp == "Fes" )
dphaGS<-subset(droGS, Spp == "Phal")
dryeGS<-subset(droGS, Spp == "Rye")

rm(conGS, droGS)

cfes<-merge(cfesGS, cfesSM, by = c("Date","UID"), all = F)
dfes<-merge(dfesGS, dfesSM, by = c("Date","UID"), all = F)
cpha<-merge(cphaGS, cphaSM, by = c("Date","UID"), all = F)
dpha<-merge(dphaGS, dphaSM, by = c("Date","UID"), all = F)
crye<-merge(cryeGS, cryeSM, by = c("Date","UID"), all = F)
drye<-merge(dryeGS, dryeSM, by = c("Date","UID"), all = F)

rm(list = ls()[!ls() %in% c("cfes","dfes","cpha","dpha","crye","drye")])

df<-rbind(cfes, cpha, crye, dfes, dpha, drye)

write.csv(df, "ROS gs and soil moisture combined.csv")
