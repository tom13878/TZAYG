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

# INSTALL PACKAGES AND SET WORKING DIRECTORY
library(foreign)
library(stringr)
library(gdata)


# SET WORKING DIRECTORY
wdpath<-"D:\\Dijk158\\Dropbox\\Michiel research\\Micro_IPOP"
#wdpath<-"W:/LEI/Internationaal Beleid  (IB)/Projecten/2271000300 IPOP 3 (new code in 2014-228250008)/Micro analysis/Tanzania analysis"
setwd(wdpath)

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# VERSION CONTROL
#version <-Sys.Date() # set to today's date
#plots<-paste("Analysis/",version, "Graph", sep=" ") # create plot folder
#dir.create(plots)
#if (!file.exists(plots)) {
#  dir.create(plots)
#}

# FUNCTIONS
source("Analysis\\Functions\\plus.R") 
source("Analysis\\Functions\\missing.plot.R") 
source("Analysis\\Functions\\multi.merge.R")

# TANZANIA
# LOAD DATA
# Load relevant plot data
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2AGRDTA\\", sep="")
AG_SEC2A<-read.dta(paste(filepath,"AG_SEC2A.dta", sep=""),convert.factors = TRUE)
AG_SEC2B<-read.dta(paste(filepath,"AG_SEC2B.dta", sep=""),convert.factors = TRUE)
AG_SEC3A<-read.dta(paste(filepath,"AG_SEC3A.dta", sep=""),convert.factors = FALSE) # Error with stata labels, probably one missing see?
AG_SEC3B<-read.dta(paste(filepath,"AG_SEC3B.dta", sep=""),convert.factors = FALSE) # Error with stata labels, probably one missing see?
AG_SEC4A<-read.dta(paste(filepath,"AG_SEC4A.dta", sep=""),convert.factors = TRUE) 
AG_SEC4B<-read.dta(paste(filepath,"AG_SEC4B.dta", sep=""),convert.factors = TRUE) 
AG_SEC6A<-read.dta(paste(filepath,"AG_SEC6A.dta", sep=""),convert.factors = FALSE) # Error with stata labels, probably one missing see?
AG_SEC6B<-read.dta(paste(filepath,"AG_SEC6B.dta", sep=""),convert.factors = FALSE) # Error with stata labels, probably one missing see?
AG_SEC11<-read.dta(paste(filepath,"AG_SEC11.dta", sep=""),convert.factors = TRUE)
rm(filepath)

# Load imputed plot size data from WB
filepath<-paste(wdpath, "\\Data\\Plot_size\\", sep="")
areas_tza_y2_imputed<-read.dta(paste(filepath,"areas_tza_y2_imputed.dta", sep=""),convert.factors = TRUE)
names(areas_tza_y2_imputed)[1]<-"y2_hhid"
rm(filepath)

# Load relevant household data
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2HH1DTA\\", sep="")
HH_SEC_A<-read.dta(paste(filepath,"HH_SEC_A.dta", sep=""),convert.factors = TRUE)
HH_SEC_B<-read.dta(paste(filepath,"HH_SEC_B.dta", sep=""),convert.factors = TRUE)
HH_SEC_C<-read.dta(paste(filepath,"HH_SEC_C.dta", sep=""),convert.factors = TRUE)
HH_SEC_E1<-read.dta(paste(filepath,"HH_SEC_E1.dta", sep=""),convert.factors = FALSE)
rm(filepath)
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2HH2DTA\\", sep="")
HH_SEC_N<-read.dta(paste(filepath,"HH_SEC_N.dta", sep=""),convert.factors = FALSE)
rm(filepath)

# Load relevant geo data
# NB in rare cases convert.factors=TRUE gives an error because of missing factors (see ?read.dta)
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2GEODTA\\", sep="")
HH.Geovariables_Y2<-read.dta(paste(filepath,"HH.Geovariables_Y2.dta", sep=""),convert.factors = TRUE)
Plot.Geovariables_Y2<-read.dta(paste(filepath,"Plot.Geovariables_Y2.dta", sep=""),convert.factors = TRUE)
#EA.offsets.TZA<-read.dta(paste(filepath,"EA.offsets.dta", sep=""),convert.factors = TRUE)
rm(filepath)

# Load relevant community survey data
filepath<-paste(wdpath, "\\Data\\Tanzania\\2010_11\\Stata\\TZNPS2COMDTA\\", sep="")
y2commlink<-read.dta(paste(filepath,"y2commlink.dta", sep=""),convert.factors = FALSE) # in order to ensure that id_01 is numeric
Com<-read.dta(paste(filepath,"y2commlink.dta", sep=""),convert.factors = TRUE)
Com<-Com[,c(1,2)]
names(Com)<-c("y2_hhid","Regions")
#COMSEC_CJ<-read.dta(paste(filepath,"COMSEC_CJ.dta", sep=""),convert.factors = TRUE) # in order to ensure that id_01 is numeric. Note that also other labels are missing.
COMSEC_CJ<-read.dta(paste(filepath,"COMSEC_CJ.dta", sep=""),convert.factors = TRUE)
rm(filepath)

# `````````````````````````````````````````````````````````````````````````````
# A. community data
# `````````````````````````````````````````````````````````````````````````````
# PREPARE DATA
# Create community indicators and link geospatial data
Com<-merge(Com, y2commlink, by="y2_hhid") # note that not for all EAs community data is available - see overview document
Com<-Com[complete.cases(Com),] # For about 600 plots the geodata is NA
Com<-merge(Com,HH.Geovariables_Y2,by="y2_hhid")
Com$District_id<-factor(with(Com, paste(Regions, id_02, sep="_")))
EAs<-unique(na.omit(Com[,c(2:9)])) # list of all EAs plus geocoordinates, district and community ids.
names(EAs)[names(EAs)=="lat_modified"]<-"lat"
names(EAs)[names(EAs)=="lon_modified"]<-"lon"
# Group regions into zones (from Demographic and Health Survey (DHS))
EAs$Zone[EAs$Regions %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
EAs$Zone[EAs$Regions %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
EAs$Zone[EAs$Regions %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
EAs$Zone[EAs$Regions %in% c("Singida","Dodoma")] <- "Central"
EAs$Zone[EAs$Regions %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
EAs$Zone[EAs$Regions %in% c("Pwani","Morogoro", "Dar es Salaam")] <- "Eastern"
EAs$Zone[EAs$Regions %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
EAs$Zone[EAs$Regions %in% c("Kaskazini Unguja", "Kusini Unguja", "Mjini Magharibi", "Kaskazini Pemba", "Kusini Pemba")] <- "Zanzibar"
write.csv(EAs, "Analysis\\Cleaned_data\\EAs.csv", row.names=FALSE)		

# Packages
library("rgdal")
library("raster")
library(RColorBrewer)

# AEZ 16
filepath<-paste(wdpath, "/Data/Spatial/AEZ_CODE_2/", sep="")
AEZ16<-readGDAL(paste(filepath,"AEZ_CODE.asc",sep=""))
AEZ16<-raster(AEZ16)
plot(AEZ16) # note many values for AEZ are missing so figure is not correct.
xy<-cbind(EAs$lon, EAs$lat) # Set coordinates to extract
AEZ<-data.frame(AEZ16Code=extract(AEZ16, xy)) # extract from raster
EAs<-cbind(EAs, AEZ)

# Check NA values
plot(AEZ16, col=brewer.pal(11,"Spectral"), xlim=c(29.3,40.4), ylim=c(-11.65,-1.04), axes=TRUE)
plot(AEZ16, col=brewer.pal(11,"Spectral"), xlim=c(37,40.4), ylim=c(-10,-4), axes=TRUE)
check<-EAs[is.na(EAs$AEZ16Code),]
points(check$lon, check$lat, col='black', pch=20, cex=0.75)
# NA values are located on coast line or on islands so match is not perfect.
# All values will be recoded to 313: Tropic - warm / subhumid
EAs$AEZ16Code[is.na(EAs$AEZ16Code)]<-313
AEZ16Code<-read.csv(paste(filepath, "AEZ16Code.csv", sep=""))
EAs<-merge(EAs, AEZ16Code[,c(1:2)], by=c("AEZ16Code"))
length(unique(HH.Geovariables_Y2$land03))
# Land03 is probably the same as AEZ16Code but misses some values (mainly islands around Zanzibar)

# `````````````````````````````````````````````````````````````````````````````
# B. Household level data
# `````````````````````````````````````````````````````````````````````````````
# Create HH level indicators
# Number of agricultural assets defined as: (1) small tools: 428, 429, 431, 435, 440, 441, 442, 443, 446, 447, 452; (2) 444, 445, 449 (see p. 40 HH Quest.)
HHAssets<-HH_SEC_N[complete.cases(HH_SEC_N),]
HHAssets$Tools<-recode(HHAssets$itemcode ,"c(428, 429, 431, 435, 440, 441, 442, 443, 446, 447, 452)='Small'; c(444, 445, 449)='Large'; else=NA")
HHAssets<-dcast(HHAssets, y2_hhid~Tools, sum, value.var="hh_n01_2")

# Create HH indicators (age and educ head, size, capital, etc)
HH<-merge(HH_SEC_C, HH_SEC_B[c(1:8)], by=c("y2_hhid","indidy2"))
HH<-merge(HH, HH_SEC_E1[c(1,2,9)], by=c("y2_hhid","indidy2"))
HH<-subset(HH, hh_b05=="HEAD")
HH$Hsex<-factor(HH$hh_b02)
HH$Hage<-HH$hh_b04
HH$hh_c08[HH$hh_c08==9999]<-NA # set year of leaving school to NA if it is unknown (9999)
HH$hh_c08[HH$hh_c08==1697]<-1997 # correct one typo
HH$Hyeduc<-with(HH, 
     ifelse(hh_c03=="No", 0,
            ifelse(hh_c05=="Yes", hh_b04-hh_c04,(hh_b04-(2010-hh_c08))-hh_c04))) # assume 2010 is year age was asked
Table<-cbind(Freq=table(HH$Hyeduc), Cumul=cumsum(table(HH$Hyeduc)),relative=prop.table(table(HH$Hyeduc)))
HH[which(HH$Hyeduc<0),]<-NA # set negative values to NA 
HH$Hprioc[HH$hh_e06 %in% c(5,6)]<-"Agriprime"
HH$Hprioc[HH$hh_e06 %in% c(1:4)]<-"Nonagriprime"
HH$Hprioc<-factor(HH$Hprioc)
HH<-HH[c("y2_hhid","indidy2","Hsex","Hage","Hyeduc","Hprioc")]
# Compute capital stock per HH
HH.cap<-AG_SEC11
HH.cap<-ddply(HH.cap,.(y2_hhid),summarize, capownsh=plus(ag11_01*ag11_02), caprentsh=plus(ag11_07*ag11_09))
HH.cap$caprentsh[is.na(HH.cap$caprentsh)]<-0 # set missing values to 0
# Compute HH.size
HH.size<-ddply(HH_SEC_B,.(y2_hhid), summarize, HHsize=length(indidy2))
# Number of plots and if plot data is missing
HH.plotnumb<-ddply(AG_SEC3A,.(y2_hhid), summarize, Plots=sum(!is.na(plotnum)), Plotmissing=factor(missing.plot(zaocode)))
# Compute total size (GPS) of plots owned by HH
#HH.totalplotsize<-ddply(areas_tza_y2_imputed,.(y2_hhid), summarize, 
#                  TotalPlotSize=plus(area_gps_mi_50), MeanPlotSize=mean(area_gps_mi_50,na.rm=TRUE))

# Merge HH files 
HH<-merge(HH, HH.size, by=c("y2_hhid"))
HH<-merge(HH, HH.cap, by=c("y2_hhid"), all.x=TRUE)
HH<-merge(HH, HH.plotnumb, by=c("y2_hhid"), all.x=TRUE)
#HH<-merge(HH, HH.totalplotsize, by=c("y2_hhid"), all.x=TRUE)
rm(HH.size,HH.cap, HH.plotnumb)

# `````````````````````````````````````````````````````````````````````````````
# C. plot level input - output data
# `````````````````````````````````````````````````````````````````````````````
# Create plot level input and output variables
# start with most disaggregated level by plot and crop.
Plot<-AG_SEC4A
CropCodes<-read.xls("Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet=1)
Plot$zaocode<-factor(Plot$zaocode,
                 levels = CropCodes$zaocode,
                 labels = CropCodes$CropName)
Plot$Fullarea<-Plot$ag4a_01 # if entire area is planted with crop ZAOCODE
Plot$Cropshare<-Plot$ag4a_02 # share of crop on plot
levels(Plot$Cropshare)<-c(levels(Plot$Cropshare), 1)
Plot$Cropshare[Plot$Fullarea=="YES"]<-1
Plot$Plantproblem<-Plot$ag4a_03
levels(Plot$Plantproblem)<-c(levels(Plot$Plantproblem), "Full area planted")
Plot$Plantproblem[Plot$ag4a_01=="YES"]<-"Full area planted"
Plot$InterCrop<-Plot$ag4a_04 # intercropped, is not the same as dividing the plot into two or more parts!
Plot$Harvproblem<-Plot$ag4a_10
levels(Plot$Harvproblem)<-c(levels(Plot$Harvproblem), "Full area harvested")
Plot$Harvproblem[Plot$ag4a_09=="NO"]<-"Full area harvested"
Plot$Outputkg<-Plot$ag4a_15 
Plot$Outputton<-Plot$ag4a_15/1000
Plot$Outputsh<-Plot$ag4a_16
Plot$Seedssh<-Plot$ag4a_21
Plot$Seedssh[Plot$ag4a_19=="NO"]<-0 #Value set to zero if no seeds purchased. Note that onw harvested seeds are not valued!
Plot$Seedtype<-factor(Plot$ag4a_23, labels=c("Traditional", "Improved"))
Plot$Seedtype<-Plot$ag4a_23
levels(Plot$Seedtype)<-c(levels(Plot$Seedtype), "None purchased")
Plot$Seedtype[Plot$ag4a_19=="NO"]<-"None purchased"
Plot$Harvestcomp<-Plot$ag4a_12
Plot<-Plot[,c(1:3,30:40)]

# Permanent tree crops
Plot3<-AG_SEC6A
Plot3$zaocode<-factor(Plot3$zaocode,
                      levels = CropCodes$zaocode,
                      labels = CropCodes$CropName)
Plot3$Outputkg<-Plot3$ag6a_08
Plot3$Tree<-"YES"
Plot3<-Plot3[,-c(4:16)]

# `````````````````````````````````````````````````````````````````````````````
# D. plot level indicators data
# `````````````````````````````````````````````````````````````````````````````
# Plot level indicators
require(dplyr)
# source wikipedia 0.40468564224 ha=1 acre
AG_SEC2A3A<-merge(AG_SEC2A, AG_SEC3A, by=c("y2_hhid", "plotnum"))
AG_SEC2A3A<-merge(AG_SEC2A3A, Plot.Geovariables_Y2, by=c("y2_hhid","plotnum"), all.x=TRUE) # GPS data missing for many plots!)
AG_SEC2A3A$ag3a_14[AG_SEC2A3A$ag3a_14==3]<-NA # correction for one value that is 3 (impossible)

Plot2<-transmute(AG_SEC2A3A,
                 y2_hhid,
                 plotnum,
                 Areafha=ag2a_04*0.40468564224, 
                 Areagpsha=ag2a_09*0.40468564224, 
                 Plotdisthkm=as.numeric(dist01), 
                 Maincrop=factor(zaocode, levels = CropCodes$zaocode, labels = CropCodes$CropName), 
                 Soil=factor(ag3a_09, labels=c("Sandy", "Loam", "Clay", " Other")), 
                 Soilq=factor(ag3a_10, labels=c("Good", "Average", "Bad")),
                 Eroscont=factor(ag3a_14, labels=c("YES", "NO")), 
                 Erosion=factor(ag3a_12, labels=c("YES", "NO")), 
                 Slope=factor(ag3a_16, labels=c("Flat bottom", "Flat top", "Slightly sloped", "Very steep")), 
                 Irrigation=factor(ag3a_17, labels=c("YES", "NO")), 
                 Ownership=factor(ag3a_24, labels=c("Owned", "Used free of charge", "Rented in", "Shared-rent", "shared-own")),
                 Landtitle=factor(ag3a_27, labels=c("YES", "NO")),
                 Cultivate=factor(ag3a_38,  labels=c("YES", "NO")),
                 Orgfert=factor(ag3a_39,labels=c("YES", "NO")),
                 Orgfertkg=ag3a_40,
                 Orgfertsh=ag3a_43, # note that this is only a subset of total organic inputs!
                 Inorgfert=factor(ag3a_45, labels=c("YES", "NO")), 
                 Pest=factor(ag3a_58, labels=c("YES", "NO")),
                 Pesttype=factor(ag3a_59, labels=c("Pesticide", "Herbicide", "Fungicide", "Other")),
                 Pestsh=ag3a_61, 
                 Pestkg=ifelse(ag3a_60_2=="MILLILITRE", ag3a_60_1/1000,
                          ifelse(ag3a_60_2=="LITRE",ag3a_60_1*1,
                                 ag3a_60_1)), # Pesticide is liters and kg. It is assumed that 1l=1k, need to be improved!!!
                 Famlabdays=apply(cbind(ag3a_70_1,ag3a_70_2,ag3a_70_3,ag3a_70_4,ag3a_70_5,ag3a_70_6,
                                          ag3a_70_13,ag3a_70_14,ag3a_70_15,ag3a_70_16,ag3a_70_17,ag3a_70_18,
                                          ag3a_70_37,ag3a_70_37,ag3a_70_39,ag3a_70_40,ag3a_70_41,ag3a_70_42,
                                          ag3a_70_25,ag3a_70_26,ag3a_70_27,ag3a_70_28,ag3a_70_29,ag3a_70_30),
                                    1,plus),
                 Hirlabdays=apply(cbind(ag3a_72_1, ag3a_72_2, ag3a_72_21,
                                          ag3a_72_4, ag3a_72_5, ag3a_72_51,
                                          ag3a_72_61, ag3a_72_62, ag3a_72_63,
                                          ag3a_72_7, ag3a_72_8, ag3a_72_81),
                                    1,plus))
                  
TotalInputsSh<-rowSums(cbind(Plot2$Orgfertsh,Plot2$Inorgfertsh,Plot2$Pestsh), na.rm=TRUE) 
Plot2<-cbind(Plot2,TotalInputsSh)

# Set NA to zero
Plot2$TotalInputsSh[is.na(Plot2$TotalInputsSh)]<-0
Plot2$Orgfertkg[is.na(Plot2$Orgfertkg)]<-0 
Plot2$Orgfertsh[is.na(Plot2$Orgfertsh)]<-0 
Plot2$Pestsh[is.na(Plot2$Pestsh)]<-0
Plot2$Pestkg[is.na(Plot2$Pestkg)]<-0
Plot2$Famlabdays[is.na(Plot2$Famlabdays)]<-0
Plot2$Hirlabdays[is.na(Plot2$Hirlabdays)]<-0

# `````````````````````````````````````````````````````````````````````````````
# E. Fertilizer database
# `````````````````````````````````````````````````````````````````````````````
# Create new database with all fertilizer information
# Compute fertilizer content using composition in Sheahan and Barett, p. 78
Fert_comp<-read.xls("Data/Other/Fert_comp.xlsx", sheet=2)
Fert_comp$P_share<-as.numeric(Fert_comp$P_share)
Fert_comp$K_share<-as.numeric(Fert_comp$K_share)

# Create database
Fert.type1<-AG_SEC3A %>% 
              transmute(y2_hhid, plotnum, InorgFertType=factor(ag3a_46,labels=c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")),
              InorgFertkg=ag3a_47, Voucher=factor(ag3a_48, labels=c("YES", "NO")), InorgFertsh=ag3a_49, FertType="Type1") %>%
            arrange(InorgFertType)
Fert.type2<-AG_SEC3A %>% 
              transmute(y2_hhid, plotnum, InorgFertType=factor(ag3a_53,labels=c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP")),
              InorgFertkg=ag3a_54, Voucher=factor(ag3a_55, labels=c("YES", "NO")), InorgFertsh=ag3a_56, FertType="Type2") %>%
            arrange(InorgFertType)
Fert<-rbind(Fert.type1, Fert.type2)
Fert<-Fert %>% arrange(InorgFertType, y2_hhid, plotnum)
Fert<-merge(Fert, Fert_comp[c("Fert_type2", "N_share", "P_share", "K_share")], by.x=c("InorgFertType"),by.y=c("Fert_type2"), all.x=TRUE)
Fert<-mutate(Fert, N=InorgFertkg*(N_share/100), P=InorgFertkg*(P_share/100), K=InorgFertkg*(K_share/100), UnitPricekg=InorgFertsh/InorgFertkg,
             UnitPrice50kg=UnitPricekg*50, NPricekg=(100/N_share)*UnitPricekg)
Fert<-merge(Fert, Plot.Geovariables_Y2[,c(1,3,4)], by=c("y2_hhid", "plotnum"), all.x=TRUE)
Fert<-merge(Fert, Com[,c(1,7,10:14)], by=c("y2_hhid"), all.x=TRUE)
Fert<-merge(Fert, EAs[,c(2,7,10,11)], by=c("ea_id"), all.x=TRUE)
Fert$Source<-"Y2"
write.csv(Fert, "Analysis\\Cleaned_data\\FertY2.csv", row.names = FALSE)

# Create fertilizer variables for plot database
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



# `````````````````````````````````````````````````````````````````````````````
# F. price data + winsoring on prices afterwards for section G.
# `````````````````````````````````````````````````````````````````````````````
# Create price database
# Standardise prices
Prices<-COMSEC_CJ
Prices$PricesEA<-with(Prices,  
ifelse(cm_j01a=="Grams", cm_j01c/(cm_j01b/1000),
  ifelse (cm_j01a=="Kilograms", cm_j01c/(cm_j01b),
    ifelse(cm_j01a=="Litre", cm_j01c/(cm_j01b),
      ifelse(cm_j01a=="Millilitre", cm_j01c/(cm_j01b/1000),
        ifelse(cm_j01a=="Pieces", cm_j01c/(cm_j01b),cm_j01c/cm_j01b))))))

Prices$PricesEAT<-with(Prices,  
ifelse(cm_j02a=="Grams", cm_j02c/(cm_j02b/1000),
  ifelse (cm_j02a=="Kilograms", cm_j02c/(cm_j02b),
    ifelse(cm_j02a=="Litre", cm_j02c/(cm_j02b),
      ifelse(cm_j02a=="Millilitre", cm_j02c/(cm_j02b/1000),
        ifelse(cm_j02a=="Pieces", cm_j02c/(cm_j02b),cm_j02c/cm_j02b))))))


# Determine number of price observations per region: village, town and village plus town
PriceInfo<-ddply(Prices,.(id_01,itemname), summarize, 
                 District_EA=sum(!is.na(PricesEA)), District_T=sum(!is.na(PricesEAT)),District_TOT=sum(District_EA,District_T))

# Compute average price by District and item
Prices.District<-Prices[,c(1:6, 13)] # compute price only using EA prices to be consistent with 2008 data
names(Prices.District)[1] <- "Regions"
Prices.District<-melt(Prices.District, id=1:6)
Prices.District<-Prices.District[complete.cases(Prices.District),]
Prices.District<-ddply(Prices.District,.(Regions, itemid, itemname), summarize, Price=mean(value,na.rm=TRUE), NumberObs=length(value))
# Select only prices based on more than five observations
Prices.District<-Prices.District[Prices.District$NumberObs>5,]
# Compute average national prices by item
Prices.National<-Prices[,c(1:6,13)]
names(Prices.National)[1]<-"Regions"
Prices.National<-melt(Prices.National, id=1:6)
Prices.National<-Prices.National[complete.cases(Prices.National),]
Prices.National<-ddply(Prices.National,.(itemid, itemname), summarize, Price=mean(value,na.rm=TRUE), NumberObs=length(value))

# `````````````````````````````````````````````````````````````````````````````
# H. output data
# `````````````````````````````````````````````````````````````````````````````
# Compute output index following Sheahan
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


