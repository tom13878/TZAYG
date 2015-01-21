#' script for cleaning the LSMS-ISA Tanzania 2010 database. The script is split
#' into five section labelled A to E which clean and prepare data on household
#' characteristics, input and output variables at the plot level, plot
#' characteristics, fertilizer prices and information and foods prices.
#' 
#' The output of this script is stored as five seperate CSV files. These files
#' are used to construct a database of winsored fertilizer and goods prices and 
#' an output index. Finally, the output files will be used to create a complete
#' 

# 1. set working directory - may need to change this
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
source("M:/TZAYG/plus.R") 
source("M:/TZAYG/missing.plot.R") 

# A. Household level data - Create HH level indicators

HQSECB <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_B.dta",
                   convert.factors = TRUE)
HQSECC <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_C.dta",
                   convert.factors = TRUE)
HQSECE1 <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_E1.dta",
                    convert.factors = TRUE)
AQSEC11 <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC11.dta",
                    convert.factors = TRUE)
AQSEC3A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC3A.dta",
                    convert.factors = FALSE)

hh.char <- left_join(HQSECC, select(HQSECB, y2_hhid:hh_b05)) %>%
  left_join(select(HQSECE1, y2_hhid, indidy2, hh_e06)) %>%
  ddply(.(y2_hhid), transform, hh.size=length(indidy2)) %>%
  filter(hh_b05 == "HEAD") %>% transmute(y2_hhid,
                                         soh = factor(hh_b02), aoh = hh_b04,
                                         schl = hh_c08, hh.size)


hh.char$schl[hh.char$schl == 9999] <- NA # set year of leaving school to NA if it is unknown (9999)
hh.char$schl[hh.char$schl == 1697] <- 1997

# Compute capital stock per HH
hh.cap <- ddply(AQSEC11, .(y2_hhid), summarize, own.sh = plus(ag11_01*ag11_02),
                rent.sh = plus(ag11_07*ag11_09))
hh.cap$rent.sh[is.na(hh.cap$rent.sh)] <- 0 # set missing values to 0
hh.plots <- ddply(AQSEC3A, .(y2_hhid), summarize, plots = sum(!is.na(plotnum)),
                  plot.missing = factor(missing.plot(zaocode)))

hh.total <- left_join(hh.char, hh.cap) %>% left_join(hh.plots)

# add a region variable year 2 is a pain because factor labels have to come from another file
HQSECA <- read.dta('C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_A.dta', convert.factors = TRUE) 
y2commlink <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2COMDTA/y2commlink.dta", convert.factors = TRUE)

y2_hhid.region <- unique(select(HQSECA, y2_hhid, region)) %>% 
        mutate(region = factor(region, labels = levels(factor(y2commlink$id_01))))

hh.total <- left_join(hh.total, y2_hhid.region)

write.csv(hh.total, "M:/cleaned_data/2010/household.csv", row.names = FALSE)

# B. plot level input - output data

AQSEC4A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC4A.dta",
                    convert.factors = TRUE)
CropCodes <- read.xls("./Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet=1)

plot.IO <- transmute(AQSEC4A, y2_hhid, plotnum, zaocode = factor(zaocode,
                      levels = CropCodes$zaocode, labels = CropCodes$CropName),
                      total.plot = ag4a_01, crop.share = ag4a_02,
                      plant.prob = ag4a_03, inter.crop = ag4a_04,
                      harv.prob = ag4a_10, output.kg = ag4a_15,
                      output.ton = ag4a_15/1000, output.sh = ag4a_16,
                      seeds.sh = ag4a_21, seed.type = ag4a_23,
                      harv.comp = ag4a_12, ag4a_09, ag4a_19)

levels(plot.IO$crop.share) <- c(levels(plot.IO$crop.share), 1)
plot.IO$crop.share[plot.IO$total.plot == "YES"] <- 1
levels(plot.IO$plant.prob) <- c(levels(plot.IO$plant.prob), "Full area planted")
plot.IO$plant.prob[plot.IO$total.plot == "YES"] <- "Full area planted"
levels(plot.IO$harv.prob) <- c(levels(plot.IO$harv.prob), "Full area harvested")
plot.IO$harv.prob[plot.IO$ag4a_09 == "NO"] <- "Full area harvested"
plot.IO$seeds.sh[plot.IO$ag4a_19 == "NO"] <- 0 
levels(plot.IO$seed.type) <- c(levels(plot.IO$seed.type), "None purchased")
plot.IO$seed.type[plot.IO$ag4a_19 == "NO"] <- "None purchased"
plot.IO <- select(plot.IO, -(ag4a_09:ag4a_19))

write.csv(plot.IO, "M:/cleaned_data/2010/plot_input_output.csv", row.names = FALSE)

#' C.
AQSEC2A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC2A.dta",
                    convert.factors = TRUE)
AQSEC3A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC3A.dta",
                    convert.factors = FALSE)
plot.geo <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/Plot.Geovariables_Y2.dta",
                     convert.factors = TRUE)
areas <- read.dta('./Data/Plot_size/areas_tza_y2_imputed.dta')
names(areas)[1] <- 'y2_hhid'

plot.vars <- left_join(AQSEC2A, AQSEC3A) %>% left_join(areas) %>%
        left_join(plot.geo)

plot.vars$ag3a_14[plot.vars$ag3a_14 == 3] <- NA # correction for one value that is 3 (impossible)

bad <- grep("ag3a_70_id", names(plot.vars))
fam.lab.days <- apply(select(plot.vars[, -bad], ag3a_70_1:ag3a_70_30), 1, plus)
hir.lab.days <- apply(select(plot.vars, ag3a_72_1:ag3a_72_9)[, c(1:3, 5:7, 9:11, 13:15)], 1, plus)
plot.vars <- cbind(plot.vars, fam.lab.days) %>% cbind(hir.lab.days)

plot.vars <- transmute(plot.vars, y2_hhid, plotnum, hir.lab.days, fam.lab.days,
                 area.est = ag2a_04*0.40468564224, area.gps = area_gps_mi_50, 
                 plot.dist = as.numeric(dist01), 
                 main.crop = factor(zaocode), 
                 soil = factor(ag3a_09, labels=c("Sandy", "Loam", "Clay", " Other")), 
                 soilq = factor(ag3a_10, labels=c("Good", "Average", "Bad")),
                 eroscont = factor(ag3a_14, labels = c("YES", "NO")), 
                 erosion = factor(ag3a_12, labels = c("YES", "NO")), 
                 slope = factor(ag3a_16, labels = c("Flat bottom", "Flat top",
                                                    "Slightly sloped", "Very steep")), 
                 irrigation = factor(ag3a_17, labels = c("YES", "NO")), 
                 ownership = factor(ag3a_24, labels = c("Owned", "Used free of charge", "Rented in",
                                                      "Shared-rent", "shared-own")),
                 land.title = factor(ag3a_27, labels = c("YES", "NO")),
                 cultivate = factor(ag3a_38,  labels = c("YES", "NO")),
                 org.fert = factor(ag3a_39,labels = c("YES", "NO")), org.fert.kg = ag3a_40,
                 org.fert.price = ag3a_43, pest = factor(ag3a_58, labels=c("YES", "NO")),
                 pest.type = factor(ag3a_59, labels = c("Pesticide", "Herbicide",
                                                        "Fungicide", "Other"))
                 ,pest.price = ag3a_61, ag3a_60_2, ag3a_60_1) 
                 
plot.vars$pest.kg = with(plot.vars,
                         ifelse(ag3a_60_2=="MILLILITRE", ag3a_60_1/1000,
                                ifelse(ag3a_60_2=="LITRE",ag3a_60_1*1, ag3a_60_1)))
plot.vars <- select(plot.vars, -(c(ag3a_60_2, ag3a_60_1)))

plot.vars$org.fert.kg[is.na(plot.vars$org.fert.kg)] <- 0 
plot.vars$org.fert.price[is.na(plot.vars$org.fert.price)] <- 0 
plot.vars$pest.price[is.na(plot.vars$pest.price)] <- 0
plot.vars$pest.kg[is.na(plot.vars$pest.kg)] <- 0
plot.vars$fam.lab.days[is.na(plot.vars$fam.lab.days)] <- 0
plot.vars$hir.lab.days[is.na(plot.vars$hir.lab.days)] <- 0

write.csv(plot.vars, "M:/cleaned_data/2010/plot_variables.csv", row.names = FALSE)

#' D. fertilizer variables are taken from agricultural questionnaire section 3A 
#'    and used to produce a database of plots by fertilizer type and quantity
#'    used of nitrogen per plot for use in the production function
AQSEC3A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC3A.dta",
                    convert.factors = FALSE)
s <- select(AQSEC3A, y2_hhid, plotnum, type1 = ag3a_46, type2 = ag3a_53)
m <- melt(s, id = c('y2_hhid', 'plotnum'))
m <- arrange(m, variable)

ty1 <- filter(m, variable == 'type1')
ty2 <- filter(m, variable == 'type2')

ty1j <- left_join(ty1, select(AQSEC3A, y2_hhid, plotnum, value = ag3a_46, fert.kg = ag3a_47))
ty1j <- select(ty1j, y2_hhid, plotnum, type = value, fert.kg)

ty2j <- left_join(ty2, select(AQSEC3A, y2_hhid, plotnum, value = ag3a_53, fert.kg = ag3a_54))
ty2j <- select(ty2j, y2_hhid, plotnum, type = value, fert.kg)

tot <- rbind(ty1j, ty2j)

# attach labels to fertilizer types
tot <- transform(tot, type = factor(type, labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                                   "generic NPK (TZA)", "MRP")))
# read in fertilizer composition table
fert.comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2) %>% rename(type = Fert_type2)
fert.comp$P_share <- as.numeric(fert.comp$P_share)
fert.comp$K_share <- as.numeric(fert.comp$K_share)
fert.comp$N_share <- as.numeric(fert.comp$N_share)

# create fertilizer variables
fert.vars <- merge(tot, select(fert.comp, type, N_share, P_share, K_share), all.x = TRUE) %>%
        mutate(N = fert.kg * (N_share/100), P = fert.kg * (P_share/100), K = fert.kg * (K_share/100)) %>%
        select(y2_hhid, plotnum, everything()) %>%
        ddply(.(y2_hhid, plotnum), summarize,
              nitrogen.kg = sum(N, na.rm = TRUE),
              phosphorous.kg = sum(P, na.rm = TRUE),
              potassium.kg = sum(K, na.rm = TRUE)) %>%
        arrange(desc(nitrogen.kg))

write.csv(fert.vars, "M:/cleaned_data/2010/fertilizer_variables.csv", row.names = FALSE)

#' E. read in the community consumer prices from the community questionnaire and
#'    return as output a cleaned set of prices that can be used to calculate the
#'    Liu meyers index

CQSECCJ <- read.dta("Data/Tanzania/2010_11/Stata/TZNPS2COMDTA/COMSEC_CJ.dta",
                    convert.factors = TRUE)
prices <- select(CQSECCJ, region = id_01, itemname, unit1 = cm_j01a, weight1 = cm_j01b,
                 price1 = cm_j01c, unit2 = cm_j02a, weight2 = cm_j02b, price2 = cm_j02c)
prices$price1[prices$price1 == 0] <- NA
prices$weight1[prices$weight1 == 0] <- NA
prices$price2[prices$price2 == 0] <- NA
prices$weight2[prices$weight2 == 0] <- NA
prices$price.loc <- with(prices,
                         ifelse(unit1 == "Grams", price1/(weight1/1000),
                                ifelse (unit1 == "Kilograms", price1/(weight1),
                                        ifelse(unit1 == "Litre", price1/(weight1),
                                               ifelse(unit1 == "Millilitre", price1/(weight1/1000),
                                                      ifelse(unit1 == "Pieces", price1/(weight1),
                                                             price1/weight1))))))
prices$price.dis <- with(prices,
                         ifelse(unit2 == "Grams", price2/(weight2/1000),
                                ifelse (unit2 == "Kilograms", price2/(weight2),
                                        ifelse(unit2 == "Litre", price2/(weight2),
                                               ifelse(unit2 == "Millilitre", price2/(weight2/1000),
                                                      ifelse(unit2 == "Pieces", price2/(weight2),
                                                             price2/weight2))))))
prices <- select(prices, region, itemname, price.loc, price.dis)
good <- !(is.na(prices$price.loc) & is.na(prices$price.dis))
prices <- prices[good, ]
write.csv(prices, "M:/cleaned_data/2010/prices.csv", row.names = FALSE)

