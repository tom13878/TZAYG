# TZA 2008 data preperation - year1
# 1. set working directory - may need to change this
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")

# 2. set some options
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) # ensures that character data that is loaded (e.g. csv) is not turned into factors
options(digits = 4)

# 3. install required packages
x <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "rgdal", "raster", "plyr")
lapply(x, library, character.only = TRUE)
library(dplyr)

# 4. source functions
source("./Analysis/Functions/plus.R") 
source("./Analysis/Functions/missing.plot.R") 
source("./Analysis/Functions/multi.merge.R")

# A. Create community data and zone variable
#    1. read in household questionnaire section A1 for hhid, region, dir
#    2. read in HH.Geovariables file which contains data on household location (GIS)
#    3. select variables from household questionnaire and merge with geo variables
#    4. Group regions into zones (from Demographic and Health Survey (DHS))
HQSECA1 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1HHDTA_E/SEC_A_T.dta", convert.factors = TRUE)
geo <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1_consdta/HH.Geovariables_Y1.dta", convert.factors = TRUE)
ComInd <- select(HQSECA1, hhid, region)
ComInd <- merge(ComInd, geo, by = "hhid")  
EAs <- select(ComInd, hhid, region, lat = lat_modified, lon = lon_modified) 
EAs$Zone[EAs$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
EAs$Zone[EAs$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
EAs$Zone[EAs$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
EAs$Zone[EAs$region %in% c("Singida","Dodoma")] <- "Central"
EAs$Zone[EAs$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
EAs$Zone[EAs$region %in% c("Pwani","Morogoro", "Dar es Salaam")] <- "Eastern"
EAs$Zone[EAs$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
EAs$Zone[EAs$region %in% c("Kaskazini Unguja", "Kusini Unguja", "Mjini Magharibi", "Kaskazini Pemba", "Kusini Pemba")] <- "Zanzibar"
write.csv(EAs, "./Analysis/TZA_2008/EAsY1.csv")

# B. create data on the household characteristics
#    1. read in data from household questionnaire sections B, C, D, E1, F, G1, and U (all one file)
#    2. read in data from agricultural questionnaiore section 11 
#    3. read in data from agricultural questionnaire section 3A
#    4. select variables on the head of the household, their age and sex and the overall size of the HH
#    5. calcualte capital stock, number of plots and plotmissing variables
#    6. merge household data together.
HQSECBU <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta", convert.factors = TRUE)
AQSEC11 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_11_ALL.dta",convert.factors = TRUE)
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta", convert.factors = FALSE)
hh_char <- select(HQSECBU, hhid, member = sbmemno, status = sbq5, sex = sbq2, age = sbq4)
hh_char <- ddply(hh_char, .(hhid), transform, size = length(member))
hh_char <- filter(hh_char, status == "HEAD")
hh_cap <- ddply(AQSEC11, .(hhid), summarize, capownsh = plus(s11q1 * s11q2), caprentsh = plus(s11q7 * s11q9))
hh_cap$caprentsh[is.na(hh_cap$caprentsh)] <- 0 
hh_plots <- ddply(AQSEC3A, .(hhid), summarize, Plots = sum(!is.na(plotnum)), Plotmissing = factor(missing.plot(s3aq5code)))
hh_var_list <- list(hh_char, HH_cap, hh_plots) ## may need to add HH1.totalPlotsize
hh_total <- multi.merge(hh_var_list, "hhid")

# C. input and output variables at the plot level
#    1. read in data from agricultural questionnaire section 4A
#    2. read in data from agricultural questionnaire section 6A
#    3. select desired variables from section 4A and alter the values and levels of some variables
#    4. select desired variables from section 6A
AQSEC4A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_4A.dta", convert.factors = TRUE)
AQSEC6A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_6A.dta", convert.factors = TRUE)
PlotIO <- transmute(AQSEC4A, hhid, plotnum, zaocode, totalPlot = s4aq3, interCrop = factor(s4aq6, labels = c("YES", "NO")),
                    Outputkg = s4aq15, Outputton = s4aq15/1000, Outputsh = s4aq16, Seedssh = s4aq20, Seedtype = s4aq22,
                    Harvestcomp = s4aq12)
PlotIO$Seedssh[PlotIO$s4aq19 == "NO"] <- 0 
levels(PlotIO$Seedtype) <- c(levels(PlotIO$Seedtype), "None purchased")
PlotIO$Seedtype[PlotIO$ag4a_19 == "NO"] <- "None purchased"
PlotTree <- select(AQSEC6A, hhid, plotnum, zaocode, Outputkg = s6aq8)

# D. Plot level indicators
#    1. read in agricultural questionnaire sections 2A and 3A
#    2. read in plot geovariables file and EAs file stored 
AQSEC2A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_2A.dta", convert.factors = TRUE)
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta", convert.factors = FALSE)
EAs <- read.csv("./Analysis/TZA_2008/EAsY1.csv")
plot_geo <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1_consdta/Plot.Geovariables_Y1_revised2.dta", convert.factors = TRUE)
PlotData <- merge(AQSEC2A, AQSEC3A, by = c("hhid", "plotnum"))
PlotData <- merge(PlotData, select(EAs, hhid, region, Zone), by = "hhid")
PlotInd <- merge(PlotData, Plot_geo, by = c("hhid", "plotnum"), all.x = TRUE)
summary(PlotInd)
PlotInd$s3aq25[PlotInd$s3aq25 == 5] <- NA
PlotInd$s3aq49[PlotInd$s3aq49 == 0] <- NA
# 2. calculate the total amount of labour spent on plot
#    and create variables and sort units and set NAs to 0 for some variables
bad <- grep("s3aq61_id", names(PlotInd))
PlotInd <- PlotInd[, -bad]
Famlabdays <- apply(select(PlotInd, s3aq61_1:s3aq61_36), 1, plus)
Hirlabdays <- apply(select(PlotInd, s3aq63_1:s3aq63_9), 1, plus)
PlotInd <- cbind(PlotInd, Famlabdays)
PlotInd <- cbind(PlotInd, Hirlabdays)

PlotInd <- transmute(PlotInd, hhid, plotnum, region, ea, Zone, Hirlabdays, Famlabdays, Areafha = s2aq4 * 0.40468564224, Plotdisthkm = as.numeric(dist01), Areagpsha = area * 0.40468564224,  Soil = factor(s3aq7, labels = c("Sandy", "Loam", "Clay", " Other")),
                     Soilq = factor(s3aq8, labels = c("Good", "Average", "Bad")), Eroscont = factor(s3aq12, labels=c("YES", "NO")),
                     Erosion = factor(s3aq10, labels = c("YES", "NO")), Irrigation = factor(s3aq15, labels = c("YES", "NO")),
                     Slope = factor(s3aq14, labels = c("Flat bottom", "Flat top", "Slightly sloped", "Very steep")),
                     Ownership = factor(s3aq22, labels = c("Owned", "Used free of charge", "Rented in", "Shared-rent", "shared-own")),
                     Landtitle = factor(s3aq25, labels = c("YES", "NO")), Cultivate = factor(s3aq36,  labels = c("YES", "NO")),
                     Pest = factor(s3aq49, labels = c("YES", "NO")), Pestkg = s3aq51_amount, PestUnit = factor(s3aq51_measure, labels = c("KG", "LITRE", "MILLILITRE")),
                     Pesttype = factor(s3aq50, labels = c("Pesticide", "Herbicide", "Fungicide", "Other")),
                     Pestsh = s3aq52, Orgfert = factor(s3aq37,labels = c("YES", "NO")), Orgfertkg = s3aq38, Orgfertsh = s3aq41)

PlotInd$Pestkg <- with(PlotInd,
                       ifelse(PestUnit == "MILLILITRE", Pestkg/1000,
                              ifelse(PestUnit == "LITRE",Pestkg*1,
                                     Pestkg))) ## check this one again

PlotInd$Pestkg[is.na(PlotInd$Pestkg)] <- 0
PlotInd$Pestsh[is.na(PlotInd$Pestsh)] <- 0
PlotInd$Orgfertkg[is.na(PlotInd$Orgfertkg)] <- 0
PlotInd$Orgfertsh[is.na(PlotInd$Orgfertsh)] <- 0
PlotInd$Famlabdays[is.na(PlotInd$Famlabdays)] <- 0
PlotInd$Hirlabdays[is.na(PlotInd$Hirlabdays)] <- 0

# E. create a database of information on fertilizer variables.
#    1. read in plotind dataframe which contains all the data on fertilizer.
#    2. read in the fertilizer composition data
#    3. calculate the important fertilizer price stuff and save in a new file.
Fert_comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2)
Fert_comp$P_share <- as.numeric(Fert_comp$P_share)
Fert_comp$K_share <- as.numeric(Fert_comp$K_share)
PlotData$s3aq44[PlotData$s3aq44 == 9] <- NA
Fert <- PlotData %>% transmute(hhid, plotnum, region, ea, Zone, Inorgfert = factor(s3aq43, labels = c("YES", "NO")), InorgFertkg = s3aq45, InorgFertsh = s3aq46,
                  InorgFertType = factor(s3aq44, labels = c("DAP", "UREA", "TSP", "CAN", "SA", "generic NPK (TZA)", "MRP"))) %>% arrange(InorgFertType)
Fert <- merge(Fert, Fert_comp[, c("Fert_type2", "N_share", "P_share", "K_share")], by.x = c("InorgFertType"),
                 by.y = c("Fert_type2"), all.x = TRUE)
Fert <- mutate(Fert, N = InorgFertkg * (N_share/100), P = InorgFertkg * (P_share/100), K = InorgFertkg * (K_share/100),
                  UnitPricekg = InorgFertsh/InorgFertkg, UnitPrice50kg = UnitPricekg*50, NPricekg = (100/N_share)*UnitPricekg)
Fert <- arrange(Fert, InorgFertType, hhid, plotnum)
Fert$Source <- "Y1"
#write.csv(Fert, "./Analysis/Cleaned_data/FertY1.csv")

# G. Price of items data - create price database
#    1. read in data from community questionnaire section CJ.
#    2. desired variables from the raw data file are selected and renamed. Itemname is changed to itemid to match up with later data. 
#    3. observations for which the price is equal to zero are dropped.
#    4. observations for which the weight is zero are set to NA
CQSECJ2 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1CMDTA_E/SEC_J2.dta", convert.factors = TRUE)
prices <- select(CQSECJ2, region, itemname = itemid, unit = cj06meas, weight = cj06wght, price = cj06pri)
levels(prices$unit) <- c("Kilograms", "Grams", "Litre", "Millilitre", "Pieces", NA)
prices$price[prices$price == 0] <- NA
prices$weight[prices$weight == 0] <- NA

#    5. calculate price per kilogram for each observation
prices$pricePkg <- with(prices,  
                        ifelse(unit == "Grams", price/(weight/1000),
                               ifelse (weight == "Kilograms", price/(weight),
                                       ifelse(unit == "Litre", price/(weight),
                                              ifelse(unit == "Millilitre", price/(weight/1000),
                                                     ifelse(unit == "Pieces", price/(weight), price/weight))))))

write.csv(prices, "./Analysis/Cleaned_Data/prices_y1.csv")

# F. Output variables - combine PlotIO and PlotTree
#    1. read in input and output data
#    2. read in price data
#    3. read in crop codes data
#    3. select plots where maize is grown, but not necessarily only maize
#    4. count the number of crops grown on each plot
#    5. merge price data with output data
CropCodes <- read.xls("Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet = 1)
Output <- select(PlotIO, hhid, plotnum, zaocode, Outputkg, Source)
Output <- Output[!is.na(Output$zaocode), ]
# Select only Plots where maize is grown (but not necessarily only maize) and remove values with Outputkg = 0 or missing
Output.Maize <- ddply(Output, .(hhid, plotnum), transform, Maize = any(zaocode == "Maize"))
Output.Maize <- Output.Maize[Output.Maize$Maize, ]
# Count the number of crops grown on each plot - split by hhid and plotnum
Count <- melt(Output.Maize[, c(1:3)], id = 1:2)
Count <- ddply(Count, .(hhid, plotnum), summarize, cropCount = length(unique(value[!is.na(value)])))
Output.Maize <- merge(Output.Maize, Count, by = c("hhid", "plotnum"))

# H. Merge national and district price data with Output 
Output.Maize <- merge(Output.Maize, EAs[, c(2:6)], by = c("hhid")) 
Output.Maize <- merge(Output.Maize, CropCodes[, c(1, 3, 4)], by.x = c("zaocode"), by.y = c("CropName"))
Output.Maize <- merge(Output.Maize, Prices.Region[, c(1,2,3)], by = c("region", "itemname"), all.x = TRUE)
Output.Maize <- merge(Output.Maize, CropCodes[, c(1,6)], by.x = "zaocode", by.y = "CropName")
Output.Maize$value <- Output.Maize$regionPrice * Output.Maize$Outputkg
## calling summary on OutputI.calc reveals that only 1757 plots have Maize on them - still need to find these strange observations
summary(Output.Maize$zaocode)
Output.Maize <- ddply(Output.Maize, .(region, hhid, plotnum), transform, MaizeFalse = "Maize" %in% zaocode)
Output.Maize <- Output.Maize[Output.Maize$MaizeFalse,]
Output.Maize <- ddply(Output.Maize, .(region, hhid, plotnum), 
                    summarize, PlotValue = sum(value), MaizePrice = regionPrice[zaocode == "Maize"],
                    MaizeValue = MaizePrice * Outputkg[zaocode == "Maize"],
                    OutputkgOld = Outputkg[zaocode == "Maize"], OutputkgNew = PlotValue/MaizePrice, 
                    MaizeShare = MaizeValue/PlotValue * 100, cropCount = unique(cropCount),
                    Beans = any(zaocode == "Beans"), CashCrop = any(CashCrop == "YES"))
Output.Maize$Multicropping <- ifelse(Output.Maize$cropCount > 1,"Multicropping", "Singlecropping")
