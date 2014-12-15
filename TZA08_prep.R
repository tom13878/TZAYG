# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# TZA 2008 data preperation - data tidying and processing of world bank data into seperate sections
# which will ultimately be combined to create a larger database for year 1 data
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
# A. This sections purpose is to create a database of geo variables which will later be merged into
#    the overall database for year 1. This includes creating a zone variable which will be important
#    for analysis. In addition to auxillary databases are created. The first holds the hhid of each
#    household and their corresponding region and zones, and the second contains each region and 
#    it's correponding zone.
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
hhid.reg.zone <- select(geo.vars, hhid, region, zone) %>% arrange(zone)
reg.zone <- unique(select(geo.vars, hhid, region, zone)) %>% arrange(zone)
write.csv(geo.vars, "./Analysis/Cleaned_data/geo_vars_y1.csv", row.names = FALSE)
write.csv(hhid.reg.zone, "./Analysis/Cleaned_data/hhid_reg_zone_y1.csv", row.names = FALSE)
write.csv(reg.zone, "./Analysis/Cleaned_data/reg_zone_y1.csv", row.names = FALSE)
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# B. This sections purpose is to create variables refering to the individual household. This 
#    information will later be combined into a larger database for year 1 and household variables
#    will be used in analyis
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
write.csv(hh.total, "./Analysis/Cleaned_data/hh_total_y1.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# C. This sections purpose is to create variables on the inputs and outputs at the plot level, for
#    example seeds as an input and crops as output. This is carried out for crops and permanent
#    crops. This section of the database is later combined with price data to construct an output
#    database
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
AQSEC4A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_4A.dta",
                    convert.factors = TRUE)
AQSEC6A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_6A.dta",
                    convert.factors = TRUE)
plot.IO <- transmute(AQSEC4A, hhid, plotnum, zaocode, total.plot = s4aq3,
                        inter.crop = factor(s4aq6, labels = c("YES", "NO")),
                        output.kg = s4aq15, output.ton = s4aq15/1000,
                        output.price = s4aq16, seed.price = s4aq20, seed.type = s4aq22,
                        harv.comp = s4aq12)
plot.IO$seed.price[plot.IO$s4aq19 == "NO"] <- 0 
levels(plot.IO$seed.type) <- c(levels(plot.IO$seed.type), "None purchased")
plot.IO$seed.type[plot.IO$ag4a_19 == "NO"] <- "None purchased"
plot.tree <- select(AQSEC6A, hhid, plotnum, zaocode, output.kg = s6aq8)
write.csv(plot.IO, "./Analysis/Cleaned_data/plot_IO_Y1.csv", row.names = FALSE)
write.csv(plot.tree, "./Analysis/Cleaned_data/plot_tree_Y1.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# D. This sections purpose is to create plot level variables describing the characteristics of each
#    plot which will later be combined into a complete database.
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
fam.lab.days <- apply(select(plot.vars[, -bad], s3aq61_1:s3aq61_36), 1, plus)
hir.lab.days <- apply(select(plot.vars, s3aq63_1:s3aq63_9)[, c(1:2, 4:5, 7:8)], 1, plus)
plot.vars <- cbind(plot.vars, fam.lab.days) %>% cbind(hir.lab.days)

plot.vars <- transmute(plot.vars, hhid, plotnum, hir.lab.days,
                     fam.lab.days, area.est = s2aq4 * 0.40468564224,
                     area.gps = area * 0.40468564224,
                     plot.dist = as.numeric(dist01), 
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
                     pest.price = s3aq52, org.fert = factor(s3aq37,labels = c("YES", "NO")),
                     org.fert.kg = s3aq38, org.fert.price = s3aq41)

plot.vars$pest.kg <- with(plot.vars,
                       ifelse(pest.unit == "MILLILITRE", pest.kg/1000,
                              ifelse(pest.unit == "LITRE", pest.kg*1,
                                     pest.kg))) ## check this one again

plot.vars$org.fert.kg[is.na(plot.vars$org.fert.kg)] <- 0 
plot.vars$org.fert.price[is.na(plot.vars$org.fert.price)] <- 0 
plot.vars$pest.price[is.na(plot.vars$pest.price)] <- 0
plot.vars$pest.kg[is.na(plot.vars$pest.kg)] <- 0
plot.vars$fam.lab.days[is.na(plot.vars$fam.lab.days)] <- 0
plot.vars$hir.lab.days[is.na(plot.vars$hir.lab.days)] <- 0

write.csv(plot.vars, "./Analysis/Cleaned_data/plot_vars_y1.csv" , row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 
# E. This section creates a seperate database of fertilizer variables which will be analysed and 
#    then winsored in the following section. The fertilizer variables will be combined with the
#    larger database and will constitute a major aspect of the analysis
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta",
                    convert.factors = FALSE)
hhid.reg.zone <- read.csv("./Analysis/Cleaned_data/hhid_reg_zone_y1.csv",
                            colClasses = c("character", "factor", "factor"))
AQSEC3A$s3aq44[AQSEC3A$s3aq44 == 9] <- NA
fert.vars <- left_join(AQSEC3A, hhid.reg.zone) %>%
  transmute(hhid, plotnum, region, zone, 
            fert = factor(s3aq43, labels = c("YES", "NO")),
            fert.kg = s3aq45, fert.price = s3aq46,
            fert.type = factor(s3aq44, labels = c("DAP", "UREA", "TSP", "CAN", "SA",
                                                  "generic NPK (TZA)", "MRP"))) %>%
  arrange(fert.type)

fert.comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2) %>% rename(fert.type = Fert_type2)
fert.comp$P_share <- as.numeric(fert.comp$P_share)
fert.comp$K_share <- as.numeric(fert.comp$K_share)
fert.vars <- merge(fert.vars, select(fert.comp, fert.type, N_share, P_share, K_share), all.x = TRUE) 
fert.vars <- mutate(fert.vars, N = fert.kg * (N_share/100),
                    P = fert.kg * (P_share/100), K = fert.kg * (K_share/100),
                    unit.price.kg = fert.price/fert.kg, unit.price.50kg = unit.price.kg*50,
                    nit.price.kg = (100/N_share)*unit.price.kg) %>%
  arrange(fert.type, hhid, plotnum)


fert.vars$year <- "2008"
write.csv(fert.vars, "./Analysis/Cleaned_data/fert_vars_y1.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# F. fertilizer variables are winsored by item and region are cleaned and processed. And stored in
# a seperate file
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
source("./Analysis/Functions/winsor.R")
fert.vars <- read.csv("./Analysis/Cleaned_Data/fert_vars_y1.csv")
fert.vars.winsor <- ddply(fert.vars, .(region, fert.type),
                          function(elt) winsor(elt, 0.975, "nit.price.kg"))
write.csv(fert.vars.winsor, "./Analysis/Cleaned_data/fert_vars_winsor_y1.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# G. Price variables are cleaned and processed. These are then exported and analysed in a seperate
#    code file.
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
prices <- read.csv("./Analysis/Cleaned_Data/prices_y1.csv")
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
# H. Output variables
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
crop.codes <- read.xls("Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet = 1)
hhid.reg.zone <- read.csv("./Analysis/Cleaned_data/hhid_reg_zone_y1.csv",
                            colClasses = c("character", "factor", "factor"))
prices <- read.csv("./Analysis/Cleaned_Data/prices_winsor_y1.csv")
colClasses <- c("character", "character", "factor", "factor", "factor", "numeric",
                "numeric", "numeric", "numeric", "factor", "factor")
output <- read.csv("./Analysis/Cleaned_data/plot_IO_Y1.csv", colClasses = colClasses) %>% 
  select(hhid, plotnum, zaocode, output.kg)
# remove single NA value for zaocode - typo
output <- output[!is.na(output$zaocode), ]

# find plots which contain at least some maize on them and combine with region, zone and
# crop codes data and also with price data
output.maize <- ddply(output, .(hhid, plotnum), transform, maize = any(zaocode == "Maize"))
output.maize <- output.maize[output.maize$maize, ]
output.maize <- left_join(output.maize, hhid.reg.zone)
output.maize <- left_join(output.maize, select(crop.codes, CropName, itemname, CashCrop),
                          by = c("zaocode" = "CropName"))
output.maize <- inner_join(output.maize, select(prices, itemname, region, region.price))

# count the number of crops on each plot and combine with the output and price data
# calculate the value in shillings of each plot
count <- melt(select(output.maize, hhid, plotnum, zaocode), id = c("hhid", "plotnum"))
count <- ddply(count, .(hhid, plotnum), summarize,
               crop.count = length(unique(value[!is.na(value)])))
output.maize <- left_join(output.maize, count)
 
output.maize$value <- output.maize$region.price * output.maize$output.kg

# calculate the Liu-Meyres index adding a variable for multicropping
output.maize <- ddply(output.maize, .(region, hhid, plotnum), 
                    summarize, plot.value = sum(value),
                    maize.price = region.price[zaocode == "Maize"],
                    maize.value = maize.price * output.kg[zaocode == "Maize"],
                    output.kg.old = output.kg[zaocode == "Maize"],
                    output.kg.new = plot.value/maize.price, 
                    maize.share = maize.value/plot.value * 100, crop.count = unique(crop.count),
                    beans = any(zaocode == "Beans"), cash.crop = any(CashCrop == "YES"))
output.maize$multi.cropping <- ifelse(output.maize$crop.count > 1,"Multicropping", "Singlecropping")
write.csv(output.maize, "./Analysis/Cleaned_Data/output_maize_y1.csv", row.names = FALSE)
