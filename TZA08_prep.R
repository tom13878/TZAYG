# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# TZA 2008 data preperation - data tidying and processing
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# 1. set working directory
# setwd("D:\\Dijk158\\Dropbox\\Michiel research\\Micro_IPOP")
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")

# 2. set some options
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# 3. install required packages
x <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "rgdal", "raster", "plyr")
lapply(x, library, character.only = TRUE)
library(dplyr)

# 4. source functions
source("./Analysis/Functions/plus.R") 
source("./Analysis/Functions/missing.plot.R") 

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# A. Create community data and zone variable
#    1. read in household questionnaire section A1 for hhid, region, dir
#    2. read in HH.Geovariables file 
#    3. select variables from household questionnaire and merge with geo variables
#    4. Group regions into zones (from Demographic and Health Survey (DHS))
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
HQSECA1 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1HHDTA_E/SEC_A_T.dta",
                    convert.factors = TRUE)
geo.vars <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1_consdta/HH.Geovariables_Y1.dta",
                     convert.factors = TRUE)
geo.vars <- left_join(geo.vars, select(HQSECA1, hhid, region)) %>%
  rename(lat = lat_modified, lon = lon_modified)

geo.vars$zone[geo.vars$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
geo.vars$zone[geo.vars$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
geo.vars$zone[geo.vars$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
geo.vars$zone[geo.vars$region %in% c("Singida","Dodoma")] <- "Central"
geo.vars$zone[geo.vars$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
geo.vars$zone[geo.vars$region %in% c("Pwani","Morogoro", "Dar es salaam")] <- "Eastern"
geo.vars$zone[geo.vars$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
geo.vars$zone[geo.vars$region %in% c("KASKAZINI UNGUJA", "KUSINI UNGUJA", "MJINI/MAGHARIBI UNGUJA",
                                     "KASKAZINI PEMBA", "KUSINI PEMBA")] <- "Zanzibar"

geo.vars$zone <- factor(geo.vars$zone)
region_and_zone <- select(geo.vars, hhid, region, zone) %>% arrange(zone)
# write.csv(geo.vars, <destfile>)
write.csv(region_and_zone, "./Analysis/Cleaned_data/region_and_zone.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# B. create data on the household characteristics
#    1. read  household questionnaire sections B, C, D, E1, F, G1, and U (all one file)
#    2. read in data from agricultural questionnaiore section 11 
#    3. read in data from agricultural questionnaire section 3A
#    4. select variables on the head of the household, their age and sex and overall size of the HH
#    5. calcualte capital stock, number of plots and plotmissing variables
#    6. merge household data together.
#    7. may still need to add household plot size for weighting later
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
HQSECBU <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta",
                    convert.factors = TRUE)
AQSEC11 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_11_ALL.dta",
                    convert.factors = TRUE)
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta",
                    convert.factors = FALSE)
hh.char <- select(HQSECBU, hhid, member = sbmemno, status = sbq5, sex = sbq2, age = sbq4) %>%
  ddply(.(hhid), transform, hh.size = length(member)) %>% filter(status == "HEAD") %>%
  select(-(member:status))
hh.cap <- ddply(AQSEC11, .(hhid), summarize, cap.own = plus(s11q1 * s11q2),
                cap.rent = plus(s11q7 * s11q9))
hh.cap$cap.rent[is.na(hh.cap$cap.rent)] <- 0 
hh.plots <- ddply(AQSEC3A, .(hhid), summarize, plots = sum(!is.na(plotnum)),
                  plot.missing = factor(missing.plot(s3aq5code)))
hh.total <- left_join(hh.char, hh.cap) %>% left_join(hh.plots)
# write.csv(hh.total, <destfile>, row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# C. input and output variables at the plot level
#    1. read in data from agricultural questionnaire section 4A
#    2. read in data from agricultural questionnaire section 6A
#    3. select desired variables from section 4A and alter the values and levels of some variables
#    4. select desired variables from section 6A
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
AQSEC4A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_4A.dta",
                    convert.factors = TRUE)
AQSEC6A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_6A.dta",
                    convert.factors = TRUE)
plot.IO <- transmute(AQSEC4A, hhid, plotnum, zaocode, total.plot = s4aq3,
                        inter.crop = factor(s4aq6, labels = c("YES", "NO")),
                        output.kg = s4aq15, output.ton = s4aq15/1000,
                        output.sh = s4aq16, seeds.sh = s4aq20, seed.type = s4aq22,
                        harv.comp = s4aq12)
plot.IO$seeds.sh[plot.IO$s4aq19 == "NO"] <- 0 
levels(plot.IO$seed.type) <- c(levels(plot.IO$seed.type), "None purchased")
plot.IO$seed.type[plot.IO$ag4a_19 == "NO"] <- "None purchased"
plot.tree <- select(AQSEC6A, hhid, plotnum, zaocode, output.kg = s6aq8)
# write.csv(plot.IO, "./Analysis/Cleaned_data/plot_IO_Y1.csv", row.names = FALSE)
# write.csv(plot.tree, <dest.file>, row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# D. Plot level indicators
#    1. read in agricultural questionnaire sections 2A and 3A
#    2. read in plot geovariables file and EAs file stored 
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
AQSEC2A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_2A.dta",
                    convert.factors = TRUE)
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta",
                    convert.factors = FALSE)
filepath <- "./Data/Tanzania/2008_09/Stata/TZNPS1_consdta/Plot.Geovariables_Y1_revised2.dta"
plot.geo <- read.dta(filepath, convert.factors = TRUE)

plot.vars <- left_join(AQSEC2A, AQSEC3A) %>% left_join(plot.geo)
# Set values that have been entered incorrectly to NA
plot.vars$s3aq25[plot.vars$s3aq25 == 5] <- NA
plot.vars$s3aq49[plot.vars$s3aq49 == 0] <- NA
#    3. remove unecessary variables which make it hard to calculate the hired
#       and family labour. Transform core variables for analysis. Change NA
#       values for various quantities to zero. 
bad <- grep("s3aq61_id", names(plot.vars))
plot.vars <- plot.vars[, -bad]
fam.lab.days <- apply(select(plot.vars, s3aq61_1:s3aq61_36), 1, plus)
hir.lab.days <- apply(select(plot.vars, s3aq63_1:s3aq63_9), 1, plus)
plot.vars <- cbind(plot.vars, fam.lab.days) %>% cbind(hir.lab.days)

plot.vars <- transmute(plot.vars, hhid, plotnum, region, zone, hir.lab.days,
                     fam.lab.days, area.est = s2aq4 * 0.40468564224,
                     plot.disth = as.numeric(dist01),
                     area.gps = area * 0.40468564224, 
                     soil = factor(s3aq7,labels = c("Sandy", "Loam", "Clay", " Other")),
                     soilq = factor(s3aq8, labels = c("Good", "Average", "Bad")),
                     eroscont = factor(s3aq12, labels=c("YES", "NO")),
                     erosion = factor(s3aq10, labels = c("YES", "NO")),
                     irrigation = factor(s3aq15, labels = c("YES", "NO")),
                     slope = factor(s3aq14, labels = c("Flat bottom", "Flat top", "Slightly sloped",
                                                       "Very steep")),
                     ownership = factor(s3aq22, labels = c("Owned", "Used free of charge",
                                                        "Rented in", "Shared-rent", "shared-own")),
                     land.title = factor(s3aq25, labels = c("YES", "NO")),
                     cultivate = factor(s3aq36,  labels = c("YES", "NO")),
                     pest = factor(s3aq49, labels = c("YES", "NO")),
                     pest.kg = s3aq51_amount,
                     pest.unit = factor(s3aq51_measure, labels = c("KG", "LITRE", "MILLILITRE")),
                     pest.type = factor(s3aq50, labels = c("Pesticide", "Herbicide", "Fungicide",
                                                           "Other")),
                     pest.sh = s3aq52, org.fert = factor(s3aq37,labels = c("YES", "NO")),
                     org.fert.kg = s3aq38, org.fert.sh = s3aq41)

plot.vars$pest.kg <- with(plot.vars,
                       ifelse(pest.unit == "MILLILITRE", pest.kg/1000,
                              ifelse(pest.unit == "LITRE", pest.kg*1,
                                     pest.kg))) ## check this one again

# write.csv(plot.vars, "./Analysis/Cleaned_data/plot_vars.csv , row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 
# E. create a database of information on inorganic fertilizer variables.
#    1. read in agricultural questionnaire section 3A containing data on inorganic fertilizer.
#    2. read in the fertilizer composition data
#    3. calculate the important fertilizer price stuff and save in a new file.
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta",
                    convert.factors = FALSE)
region_and_zone <- read.csv("./Analysis/Cleaned_data/region_and_zone.csv",
                            colClasses = c("character", "factor", "factor"))
AQSEC3A$s3aq44[AQSEC3A$s3aq44 == 9] <- NA
fert.vars <- left_join(AQSEC3A, region_and_zone) %>%
  transmute(hhid, plotnum, region, zone, 
            fert = factor(s3aq43, labels = c("YES", "NO")),
            fert.kg = s3aq45, fert.sh = s3aq46,
            fert.type = factor(s3aq44, labels = c("DAP", "UREA", "TSP", "CAN", "SA",
                                                  "generic NPK (TZA)", "MRP"))) %>%
  arrange(fert.type)

fert.comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2) %>% rename(fert.type = Fert_type2)
fert.comp$P_share <- as.numeric(fert_comp$P_share)
fert.comp$K_share <- as.numeric(fert_comp$K_share)
fert.comp <- select(fert.comp, fert.type, N_share, P_share, K_share)
fert.vars <- left_join(fert.vars, fert_comp ) 
fert.vars <- mutate(fert.vars, N = fert.kg * (N_share/100),
                    P = fert.kg * (P_share/100), K = fert.kg * (K_share/100),
                    unit.pricekg = fert.sh/fert.kg, unit.price50kg = unit.pricekg*50,
                    Npricekg = (100/N_share)*unit.pricekg) %>% arrange(fert.type, hhid, plotnum)

fert.vars$year <- "2008"
# write.csv(fert.vars, "./Analysis/Cleaned_data/fert_vars.csv", row.names = FALSE)
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# F. Price of items data - create price database
#    1. read in data from community questionnaire section CJ.
#    2. desired variables from the raw data file are selected and renamed. Itemname is changed to
#       itemid to match up with later data. 
#    3. observations for which the price is equal to zero are dropped.
#    4. observations for which the weight is zero are set to NA
#    5. prices are changed to units of kilograms
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
CQSECJ2 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1CMDTA_E/SEC_J2.dta",
                    convert.factors = TRUE)
prices <- select(CQSECJ2, region, itemname = itemid, unit = cj06meas, weight = cj06wght,
                 price = cj06pri)
levels(prices$unit) <- c("Kilograms", "Grams", "Litre", "Millilitre", "Pieces", NA)
prices$price[prices$price == 0] <- NA
prices$weight[prices$weight == 0] <- NA

prices$price.unit <- with(prices,  
                        ifelse(unit == "Grams", price/(weight/1000),
                               ifelse (weight == "Kilograms", price/(weight),
                                       ifelse(unit == "Litre", price/(weight),
                                              ifelse(unit == "Millilitre", price/(weight/1000),
                                                     ifelse(unit == "Pieces", price/(weight),
                                                            price/weight))))))

write.csv(prices, "./Analysis/Cleaned_Data/prices_y1.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# G. Winsor prices and create database with regional and national
#    prices
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
source("./Analysis/Functions/winsor.R")
prices <- read.csv("./Analysis/Cleaned_Data/prices_y2.csv")
prices <- ddply(prices, .(region, itemname), function(elt) winsor(elt, 0.975, "price.unit"))

prices.region <- ddply(prices, .(region, itemname), summarize,
                       region.price = mean(price.unit, na.rm = TRUE),
                       num.obs = sum(is.na(price.unit)))
prices.national <- ddply(prices, .(itemname), summarize,
                         national.price = mean(price.unit, na.rm = TRUE))
prices.region$region.price[prices.region$num.obs < 5] <- NA
prices.region <- left_join(prices.region, prices.national)
bad <- is.na(prices.region$region.price)
prices.region$region.price[bad] <- prices.region$national.price[bad]
write.csv(prices.region,"./Analysis/Cleaned_Data/prices_winsor_y1.csv", row.names = FALSE )

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# H. Output variables - combine PlotIO and PlotTree
#    1. read in input and output data
#    2. read in price data
#    3. read in crop codes data
#    3. select plots where maize is grown, but not necessarily only maize
#    4. count the number of crops grown on each plot
#    5. join price data with output data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
crop.codes <- read.xls("Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet = 1)
colClasses <- c("character", "character", "factor", "factor", "factor", "numeric",
                "numeric", "numeric", "numeric", "factor", "factor")
region_and_zone <- read.csv("./Analysis/Cleaned_data/region_and_zone.csv",
                            colClasses = c("character", "factor", "factor"))
prices <- read.csv("./Analysis/Cleaned_Data/prices_winsor_y1.csv")
output <- read.csv("./Analysis/Cleaned_data/plot_IO_Y1.csv", colClasses = colClasses) %>% 
  select(hhid, plotnum, zaocode, output.kg)
output <- output[!is.na(output$zaocode), ]
# Select only Plots where maize is grown (but not necessarily only maize) 
output.maize <- ddply(output, .(hhid, plotnum), transform, maize = any(zaocode == "Maize"))
output.maize <- output.maize[output.maize$maize, ]
# Count the number of crops grown on each plot - split by hhid and plotnum
count <- melt(select(output.maize, hhid, plotnum, zaocode), id = c("hhid", "plotnum"))
count <- ddply(count, .(hhid, plotnum), summarize,
               crop.count = length(unique(value[!is.na(value)])))
output.maize <- left_join(output.maize, count)
 
output.maize <- left_join(output.maize, region_and_zone)
output.maize <- left_join(output.maize, select(crop.codes, CropName, itemid, itemname, CashCrop),
                          by = c("zaocode" = "CropName"))
prices <- select(prices, itemname, region, region.price)
output.maize <- left_join(output.maize, prices)
# output.maize <- left_join(Output.Maize, CropCodes[, c(1,6)], by.x = "zaocode", by.y = "CropName")
output.maize$value <- Output.Maize$regionPrice * Output.Maize$Outputkg
## calling summary on OutputI.calc reveals that only 1757 plots have Maize on them 
summary(Output.Maize$zaocode)
Output.Maize <- ddply(Output.Maize, .(region, hhid, plotnum), transform,
                      MaizeFalse = "Maize" %in% zaocode)
Output.Maize <- Output.Maize[Output.Maize$MaizeFalse,]

Output.Maize <- ddply(Output.Maize, .(region, hhid, plotnum), 
                    summarize, PlotValue = sum(value), MaizePrice = regionPrice[zaocode == "Maize"],
                    MaizeValue = MaizePrice * Outputkg[zaocode == "Maize"],
                    OutputkgOld = Outputkg[zaocode == "Maize"], OutputkgNew = PlotValue/MaizePrice, 
                    MaizeShare = MaizeValue/PlotValue * 100, cropCount = unique(cropCount),
                    Beans = any(zaocode == "Beans"), CashCrop = any(CashCrop == "YES"))
Output.Maize$Multicropping <- ifelse(Output.Maize$cropCount > 1,"Multicropping", "Singlecropping")
