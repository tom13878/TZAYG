#' script for cleaning the 


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


# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# B. Household level data - Create HH level indicators

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
                                         schl = hh_c08, prioc = factor(hh_e06), hh.size)


hh.char$schl[hh.char$schl == 9999] <- NA # set year of leaving school to NA if it is unknown (9999)
hh.char$schl[hh.char$schl == 1697] <- 1997
hh.char$prioc[hh.char$prioc %in% c(5,6)] <- "Agriprime"
hh.char$prioc[hh.char$prioc %in% c(1:4)] <- "Nonagriprime"

# hh.char$hi.ed <- with(hh.char, 
#                      ifelse(hh_c03=="No", 0,
#                             ifelse(hh_c05 == "Yes", hh_b04 - hh_c04,(hh_b04-(2010-hh_c08))-hh_c04))) # assume 2010 is year age was asked
# HH[which(HH$Hyeduc<0),]<-NA # set negative values to NA

# Create HH indicators (age and educ head, size, capital, etc)
# Table<-cbind(Freq=table(HH$Hyeduc), Cumul=cumsum(table(HH$Hyeduc)),relative=prop.table(table(HH$Hyeduc)))
# HH[which(HH$Hyeduc<0),]<-NA # set negative values to NA 
# HH$Hprioc<-factor(HH$Hprioc)

# Compute capital stock per HH
hh.cap <- ddply(AQSEC11, .(y2_hhid), summarize, cap.own = plus(ag11_01*ag11_02),
                cap.rent = plus(ag11_07*ag11_09))
hh.cap$cap.rent[is.na(hh.cap$cap.rent)] <- 0 # set missing values to 0
hh.plots <- ddply(AQSEC3A, .(y2_hhid), summarize, plots = sum(!is.na(plotnum)),
                  plot.missing = factor(missing.plot(zaocode)))

hh.total <- left_join(hh.char, hh.cap) %>% left_join(hh.plots)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# C. plot level input - output data

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

# write.csv(plot.IO, "./Analysis/Cleaned_data/plot_IO_y2.csv", row.names = FALSE)
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# D. plot level indicators data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
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

write.csv(plot.vars, './Analysis/Cleaned_data/plot_vars_y2.csv' , row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# E. create a database of information on inorganic fertilizer variables.
#    1. read in plot.vars dataframe which contains all the data on fertilizer.
#    2. read in the fertilizer composition data
#    3. calculate the important fertilizer price stuff and save in a new file.
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
AQSEC3A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC3A.dta",
                    convert.factors = FALSE)

fert.main <- AQSEC3A %>% 
  transmute(y2_hhid, plotnum, fert.type = factor(ag3a_46,labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                                                  "generic NPK (TZA)", "MRP")),
            fert.kg = ag3a_47, fert.price = ag3a_49,
            voucher = factor(ag3a_48, labels=c("YES", "NO")),
            main.fert = factor(ifelse(is.na(fert.type), NA, "MAIN"))) %>%
  arrange(fert.type)

fert.second <- AQSEC3A %>% 
  transmute(y2_hhid, plotnum, fert.type = factor(ag3a_53,labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                                                  "generic NPK (TZA)", "MRP")),
            fert.kg=ag3a_54, fert.price = ag3a_56, voucher = factor(ag3a_55, labels=c("YES", "NO")),
            main.fert = factor(ifelse(is.na(fert.type), NA, "SECOND"))) %>%
  arrange(fert.type)

fert.vars <- rbind(fert.main, fert.second)
fert.vars$fert.price[fert.vars$fert.price == 0] <- NA

fert.comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2) %>% rename(fert.type = Fert_type2)
fert.comp$P_share <- as.numeric(fert.comp$P_share)
fert.comp$K_share <- as.numeric(fert.comp$K_share)
fert.comp$N_share <- as.numeric(fert.comp$N_share)
fert.comp$N_share[fert.comp$N_share == 0] <- NA
fert.vars <- merge(fert.vars, select(fert.comp, fert.type, N_share, P_share, K_share), all.x = TRUE) 
fert.vars <- mutate(fert.vars, N = fert.kg * (N_share/100),
                    P = fert.kg * (P_share/100), K = fert.kg * (K_share/100),
                    unit.price.kg = fert.price/fert.kg, unit.price.50kg = unit.price.kg*50,
                    nit.price.kg = (100/N_share)*unit.price.kg) %>%
        select(y2_hhid, plotnum, everything())

fert.vars$year <- "2010"
write.csv(fert.vars, "./Analysis/Cleaned_data/fert_vars_y2.csv", row.names = FALSE)



# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# F. price data 
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````


# write.csv(prices, "./Analysis/Cleaned_Data/prices_y2.csv", row.names = FALSE)

