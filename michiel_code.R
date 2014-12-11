# IPOP 2271000300 IPOP 3 (new code in 2014-228250008)
# R code to prepare LSMS-ISA data for yield and adoption analysis

# TO DO
# Add soil and rain stress variables
# add rain stress variable from community survey
# Check for other control factors (see Burke)
# Link CAN use to share of maize on plot. Suspician that for these plots Maize might not be key crop.
# Distinguish between buyers and seller of Maize (see Sheahan)
# Focus on two Maize regions (Southern Highlands and around Arusha) and compare results.
# Add interaction N and irrigation (see Jayne and Rashid, p. 552)

# VERSION CONTROL
#version <-Sys.Date() # set to today's date
#plots<-paste("Analysis/",version, "Graph", sep=" ") # create plot folder
#dir.create(plots)
#if (!file.exists(plots)) {
#  dir.create(plots)
#}

# Load relevant community survey data
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2COMDTA\\", sep="")
y2commlink<-read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2COMDTA/y2commlink.dta" ,convert.factors = TRUE) # in order to ensure that id_01 is numeric
Com<-read.dta(paste(filepath,"y2commlink.dta", sep=""),convert.factors = TRUE)
Com<-Com[,c(1,2)]
names(Com)<-c("y2_hhid","Regions")

Com<-merge(Com, y2commlink, by="y2_hhid") # note that not for all EAs community data is available - see overview document
Com<-Com[complete.cases(Com),] # For about 600 plots the geodata is NA
Com<-merge(Com,HH.Geovariables_Y2,by="y2_hhid")
Com$District_id<-factor(with(Com, paste(Regions, id_02, sep="_")))
EAs<-unique(na.omit(Com[,c(2:9)])) # list of all EAs plus geocoordinates, district and community ids.

filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2HH2DTA\\", sep="")
HH_SEC_N<-read.dta(paste(filepath,"HH_SEC_N.dta", sep=""),convert.factors = FALSE)
# Number of agricultural assets defined as: (1) small tools: 428, 429, 431, 435, 440, 441, 442, 443, 446, 447, 452; (2) 444, 445, 449 (see p. 40 HH Quest.)
HHAssets<-HH_SEC_N[complete.cases(HH_SEC_N),]
HHAssets$Tools<-recode(HHAssets$itemcode ,"c(428, 429, 431, 435, 440, 441, 442, 443, 446, 447, 452)='Small'; c(444, 445, 449)='Large'; else=NA")
HHAssets<-dcast(HHAssets, y2_hhid~Tools, sum, value.var="hh_n01_2")

# Create fertilizer variables for plot database
Fert<-merge(Fert, Plot.Geovariables_Y2[,c(1,3,4)], by=c("y2_hhid", "plotnum"), all.x=TRUE)
Fert2<-Fert %>%
  group_by(y2_hhid, plotnum) %>%      
  select(y2_hhid, plotnum, N, P, K) %>%
  summarise(N=plus(N), P=plus(P), K=plus(K)) %>%
  arrange(desc(N))
Fert2[is.na(Fert2)]<-0
Plot2<-merge(Plot2, Fert2, by=c("y2_hhid", "plotnum"))
#InorgFertkg<-rowSums(cbind(AG_SEC3A$ag3a_47,AG_SEC3A$ag3a_54), na.rm=TRUE)
#InorgFertkg<-cbind(AG_SEC3A[,c(1,2)], InorgFertkg)

Plot2<-merge(Plot2, areas_tza_y2_imputed, by=c("y2_hhid", "plotnum"), all.x=TRUE)

# from the data prep 2008 original output section
## calling summary on OutputI.calc reveals that only 1757 plots have Maize on them 
summary(factor(output.maize$zaocode))
output.maize <- ddply(output.maize, .(region, hhid, plotnum), transform,
                      maize.false = "Maize" %in% zaocode)
output.maize <- output.maize[output.maize$maize.false,]

#original output code from year 2 
OutputI<-Plot[c("y2_hhid", 'plotnum', "zaocode", "Outputkg")]
OutputI$Source<-"CropQ"
OutputI$ID<-paste(OutputI$y2_hhid, OutputI$plotnum, OutputI$zaocode)
OutputIb<-Plot3[c("y2_hhid", 'plotnum', "zaocode", "Outputkg")]
OutputIb$Source<-"FruitQ"
OutputIb$ID<-paste(OutputIb$y2_hhid, OutputIb$plotnum, OutputIb$zaocode)
OutputI.tot<-rbind(OutputI, OutputIb)
#check for duplicates in crop and permanent crop files
check<-ddply(OutputI.tot,.(y2_hhid, plotnum, zaocode, ID), summarize, count=length(zaocode))
check<-check[check$count>1 & !is.na(check$zaocode),]
check2<-subset(OutputI.tot, ID %in% check$ID & zaocode %in% c("Banana", "Passion fruit"))
OutputI<-subset(OutputI, !(ID %in% check2$ID)) # remove duplicates
OutputI.tot<-rbind(OutputI, OutputIb)
OutputI.tot<-OutputI.tot[!is.na(OutputI.tot$zaocode),]
OutputI.tot<-OutputI.tot[!is.na(OutputI.tot$Outputkg),]
rm(check, check2, OutputI, OutputIb)
# Select only plots where maize is grown and remove values with Outputkg=0 or missing
OutputI.tot<-ddply(OutputI.tot,.(y2_hhid, plotnum),  transform, Maize=any(zaocode %in% c("Maize")))
OutputI.tot<-OutputI.tot[OutputI.tot$Maize,]
OutputI.tot<-subset(OutputI.tot, Outputkg!=0 & !is.na(Outputkg))
# Check how many crops per plot are grown
CropCount<-OutputI.tot
CropCount$Count<-1
CropCount$zaocode<-factor(CropCount$zaocode)
CropCount<-dcast(CropCount[,c(1:3,8)], ...~zaocode, value.var="Count")
CropCount<-data.frame(freq=colSums(!is.na(CropCount[,c(3:61)])))
# Add number of crops on plot
OutputI.tot<-ddply(OutputI.tot,.(y2_hhid, plotnum),  transform, NumbofCrops=sum(!is.na(Outputkg)))
MoreThan7Crops<-OutputI.tot[OutputI.tot$NumbofCrops>7,]
table(OutputI.tot[!duplicated(OutputI.tot[,c(1,2)]),]$NumbofCrops)
# Merge national and district price data with Output 
OutputI.tot<-merge(OutputI.tot, Com[,c(1:6)], by=c("y2_hhid")) # Note: about 1000 observations do not have geocodes
OutputI.tot<-merge(OutputI.tot, CropCodes[,c(1,3,4)], by.x=c("zaocode"), by.y=c("CropName"))
OutputI.tot.District<-merge(OutputI.tot, Prices.District[,c(1,3,4)], by=c("Regions", "itemname"))
OutputI.tot.National<-merge(OutputI.tot, Prices.District[,c(1,3,4)], by=c("Regions", "itemname"), all.x=TRUE)
OutputI.tot.National<-OutputI.tot.National[!complete.cases(OutputI.tot.National),]
OutputI.tot.National$Price<-NULL
OutputI.tot.National<-merge(OutputI.tot.National, Prices.National[,c(2,3)], by=c("itemname"), all.x=TRUE)
OutputI.tot<-rbind(OutputI.tot.National, OutputI.tot.District)

# Calculate Yield output corrected for multicropping
OutputI.calc<-OutputI.tot
OutputI.calc<-merge(OutputI.calc, CropCodes[,c(1,6)], by.x="zaocode", by.y="CropName")
OutputI.calc$value<-OutputI.calc$Price*OutputI.calc$Outputkg
OutputI.calc<-ddply(OutputI.calc,.(Regions,y2_hhid, plotnum), 
                    summarize, PlotValue=sum(value), MaizePrice=Price[zaocode=="Maize"], MaizeValue=MaizePrice*Outputkg[zaocode=="Maize"],
                    OutputkgOld=Outputkg[zaocode=="Maize"], OutputkgNew=PlotValue/MaizePrice, 
                    MaizeShare=MaizeValue/PlotValue*100, NumbofCrops=unique(NumbofCrops), Beans=any(zaocode=="Beans"), CashCrop=any(CashCrop=="YES"))
OutputI.calc$Multicropping<-ifelse(OutputI.calc$NumbofCrops>1,"Multicropping", "Singlecropping")
OutputI.calc$Regions<-NULL # Otherwise there will be a duplication later.
#check<-subset(OutputI.calc, MaizeShare>=25 & CashCrop)
#check<-subset(OutputI.calc, MaizeShare>=25 & NumbofCrops>7)

# imputed plot areas from year 2 code
# Load imputed plot size data from WB
filepath<-paste(wdpath, "\\Data\\Plot_size\\", sep="")
areas_tza_y2_imputed<-read.dta(paste(filepath,"areas_tza_y2_imputed.dta", sep=""),convert.factors = TRUE)
names(areas_tza_y2_imputed)[1]<-"y2_hhid"



#EA.offsets.TZA<-read.dta(paste(filepath,"EA.offsets.dta", sep=""),convert.factors = TRUE)