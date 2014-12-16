## Is it possible to use year 2 plot data in year 1 data
## plot size should remain constant across years.

## read in data
library(gdata)
library(foreign)
library(plyr)

## SET WORKING DIRECTORY
#wdpath = "c:\\Users\\morle001\\Dropbox\\IPOP micro"
wdpath = "D:\\Dijk158\\Dropbox\\Michiel research\\IPOP micro"

setwd(wdpath)

## year 2: contains variable for plot area (GPS)
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2AGRDTA\\", sep="")
AG_SEC2A<-read.dta(paste(filepath,"AG_SEC2A.dta", sep=""),convert.factors = TRUE)

## Get 2008 household id from sec B in order to merge with year 1 data 
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2HH1DTA\\", sep="")
ID08<-read.dta(paste(filepath,"HH_SEC_B.dta", sep=""),convert.factors = TRUE)[,1:2]
ID08 <- unique(ID08)

## merge AG_SEC2A and ID08 by "y2_hhid"
plotYear2 <- merge(AG_SEC2A, ID08, by = "y2_hhid")

# Select only plot size data
plotYear2<-plotYear2[,c(1:4,7,9)]
names(plotYear2)[c(3,5)]<-c("sizeFarm_2", "sizeGPS_2")

## year 1 data frame containing household ID, plot size and plot number
filepath<-paste(wdpath, "\\Data\\Tanzania\\2008_09\\Stata\\TZNPS1AGDTA_E\\", sep="")
## Is the area variable right??
AG1_SEC2A<-read.dta(paste(filepath,"SEC_2A.dta", sep=""),convert.factors = TRUE)

# Select relevant data for year 1
plotYear1<-AG1_SEC2A
names(plotYear1)[c(3,4)]<-c("sizeFarm_1", "sizeGPS_1")

## merge year 1 and year 2  data frames by HH ID and plot num
fullData <- merge(plotYear2, plotYear1, by.x = c("hhid_2008", "y1plotnum"), by.y = c("hhid", "plotnum"))
# Only look at smaller plots
good <- which(fullData$sizeGPS_1<2.5 & fullData$sizeGPS_2<2.5 )
good <- which(fullData$sizeFarm_1<10 & fullData$sizeFarm_2<10 )
fullData <- fullData[good,]

ggplot(data=fullData)+geom_point(aes(x=sizeGPS_1, y=sizeGPS_2))+
  geom_smooth(aes(x=sizeGPS_1, y=sizeGPS_2),method="lm")

ggplot(data=fullData)+geom_point(aes(x=sizeFarm_1, y=sizeFarm_2))+
  geom_smooth(aes(x=sizeFarm_1, y=sizeFarm_2),method="lm")

model1 = lm(fullData$sizeGPS_1 ~ fullData$sizeGPS_2) 
summary(model1)

# Compare household characteristics between years
# Load HH data Y2
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2HH1DTA\\", sep="")
HH_SEC_B<-read.dta(paste(filepath,"HH_SEC_B.dta", sep=""),convert.factors = TRUE)
HHYear2<-HH_SEC_B[,c(1,2,3,5,7,9)]
names(HHYear2)[c(4,5,6)]<-c("Birth_y2", "Age_y2", "indidy1")

# Load HH data Y1
filepath<-paste(wdpath, "\\Data\\Tanzania\\2008_09\\Stata\\TZNPS1HHDTA_E\\", sep="")
HH_SEC_B1<-read.dta(paste(filepath, "SEC_B_C_D_E1_F_G1_U.dta", sep=""),convert.factors = FALSE)
HHYear1<-HH_SEC_B1[,c(1,2,4,6)]
names(HHYear1)[c(2,3,4)]<-c("indidy1","Birth_y1", "Age_y1")

# Merge HH data
HHfulldata<-merge(HHYear1, HHYear2, by.x=c("hhid", "indidy1"), by.y=c("hhid_2008", "indidy1"))
HHfulldata$Age_y1c<-HHfulldata$Age_y1+2

ggplot(data=HHfulldata)+geom_point(aes(x=Birth_y1, y=Birth_y2))+
  geom_smooth(aes(x=Birth_y1, y=Birth_y2),method="lm")

ggplot(data=HHfulldata)+geom_point(aes(x=Age_y1c, y=Age_y2))+
  geom_smooth(aes(x=Age_y1c, y=Age_y2),method="lm")
