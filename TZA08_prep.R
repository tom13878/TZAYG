#' script for cleaning the LSMS-ISA Tanzania 2008 database. The script is split
#' into five section labelled A to E which clean and prepare data on household
#' characteristics, input and output variables at the plot level, plot
#' characteristics, fertilizer prices and information and foods prices.
#' 
#' The output of this script is stored as five seperate CSV files. These files
#' are used to construct a database of winsored fertilizer and goods prices and 
#' an output index. Finally, the output files will be used to create a complete

setwd("c:/Users/morle001/Dropbox/Micro_IPOP")

options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

library('foreign')
library('stringr')
library('gdata')
library('car')
library('reshape2')
library('plyr')
library('dplyr')

source("M:/TZAYG/plus.R") 
source("M:/TZAYG/missing.plot.R") 

# ``````````````````````````````````````````````````````````````````````````````
#' create database of household characteristics for the HEAD of the household
HQSECBU <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1HHDTA_E/SEC_B_C_D_E1_F_G1_U.dta",
                    convert.factors = TRUE)
head <- select(HQSECBU, hhid, sbmemno, sbq5, sbq2, sbq4) %>%
  ddply(.(hhid), transform, hh.size = length(sbmemno)) %>% filter(sbq5 == "HEAD") %>%
  select(hhid, soh = sbq2, aoh = sbq4)
all(unique(HQSECBU$hhid) %in% unique(head$hhid)) # TRUE

#' create a database of household capital stocks
#' set NA values to zero where a household does not rent or own a particular
#' piece of equipment. 
AQSEC11 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_11_ALL.dta",
                    convert.factors = TRUE)
capital <- ddply(AQSEC11, .(hhid), summarize, own.sh = plus(s11q1 * s11q2),
                rent.sh = plus(s11q7 * s11q9))
capital$rent.sh[is.na(capital$rent.sh)] <- 0 
capital$own.sh[is.na(capital$own.sh)] <- 0 

#' calculate the number of plots per household
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta",
                    convert.factors = FALSE)
plot <- ddply(AQSEC3A, .(hhid), summarize, plots = sum(!is.na(plotnum)),
                  plot.missing = factor(missing.plot(s3aq5code)))

#' join together the household information 
hh.total <- left_join(head, capital) %>% left_join(plot)
write.csv(hh.total, "M:/cleaned_data/2008/household.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````
#' plot level variables including the input and output data and variables
#' particular to the individual plots. first up the 
#' 
#' 
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

write.csv(plot.vars, "M:/cleaned_data/2008/plot_variables.csv", row.names = FALSE)

#' input and output data at the plot level note the duplication of plots due to
#' multiple crops on a single plot. This will be aggregated using the Liu meyers
#' index to find a 'maize value' for each plot 
AQSEC4A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_4A.dta",
                    convert.factors = TRUE)

plot.IO <- transmute(AQSEC4A, hhid, plotnum, zaocode, total.plot = s4aq3,
                        inter.crop = factor(s4aq6, labels = c("YES", "NO")),
                        output.kg = s4aq15, output.ton = s4aq15/1000,
                        output.price = s4aq16, seed.price = s4aq20, seed.type = s4aq22,
                        harv.comp = s4aq12)
plot.IO$seed.price[plot.IO$s4aq19 == "NO"] <- 0 
levels(plot.IO$seed.type) <- c(levels(plot.IO$seed.type), "None purchased")
plot.IO$seed.type[plot.IO$ag4a_19 == "NO"] <- "None purchased"

write.dta(plot.IO, "M:/cleaned_data/2008/plot_input_output.dta", version = 7L)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
#' note that year 1 only asks for a single fertilizer and so each plot is entered only once, 
#' This is not the case for year two
AQSEC3A <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1AGDTA_E/SEC_3A.dta",
                    convert.factors = FALSE)

AQSEC3A$s3aq44[AQSEC3A$s3aq44 == 9] <- NA
fert.vars <- transmute(AQSEC3A, hhid, plotnum, fert = factor(s3aq43, labels = c("YES", "NO")),
            fert.kg = s3aq45, fert.price = s3aq46,
            fert.type = factor(s3aq44, labels = c("DAP", "UREA", "TSP", "CAN", "SA",
                                                  "generic NPK (TZA)", "MRP"))) 

fert.comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2) %>% rename(fert.type = Fert_type2)
fert.comp$P_share <- as.numeric(fert.comp$P_share)
fert.comp$K_share <- as.numeric(fert.comp$K_share)
fert.comp$P_share[fert.comp$P_share == 0] <- NA
fert.comp$K_share[fert.comp$K_share == 0] <- NA
fert.comp$N_share[fert.comp$N_share == 0] <- NA

fert.vars <- merge(fert.vars, select(fert.comp, fert.type, N_share, P_share, K_share), all.x = TRUE) 
fert.vars <- mutate(fert.vars, N = fert.kg * (N_share/100),
                    P = fert.kg * (P_share/100), K = fert.kg * (K_share/100)) %>%
        select(hhid, plotnum, everything())

write.csv(fert.vars, "M:/cleaned_data/2008/fertilizer_variables.csv", row.names = FALSE)

# ``````````````````````````````````````````````````````````````````````````````````````````````````
#' prices of goods in the community section are calcualted to be used in the Liu
#' Meyres output section after winsoring. 


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
good <- complete.cases(prices)
prices <- filter(prices, good) %>% select(region, itemname, price.unit)

write.csv(prices, "M:/cleaned_data/2008/prices.csv", row.names = FALSE)
