# -------------------------------------
# calculate the maize producer prices
# for wave 2 from agricultural 
# questionnaire section 5
#
# below the same calculation is made
# for producer prices in wave 3.
# -------------------------------------

setwd( "c:/Users/morle001/Dropbox/Micro_IPOP" )
library( haven )
library(foreign)
library(dplyr)

# read in the price data
prod_price_w2 <- read_dta( "./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC5A.dta")
maize_price_w2 <- subset(prod_price, zaocode==11 & ag5a_01==1) %>% select(y2_hhid:ag5a_19)

# calculate the unit price paid by customers 1 and 2
maize_price_w2 <- transmute(maize_price_w2, y2_hhid, ag5a_04_1, unit_price1=ag5a_06/ag5a_05,
                          unit_price2=ag5a_11/ag5a_10)

# take the average across the two prices
average_price <- (maize_price2$unit_price1 + maize_price2$unit_price2)/2

# where there is no unit price for the second customer use customer 1 unit price
# otherwise use the average of the two prices.
maize_price_w2$final_price <- ifelse(is.na(maize_price_w2$unit_price2), maize_price_w2$unit_price1, average_price)

# read in regions to match up with the household producer prices.
HQSECA <- read.dta('C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_A.dta', convert.factors = TRUE) 
y2commlink <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2COMDTA/y2commlink.dta", convert.factors = TRUE)

hhid_region_w2 <- unique(select(HQSECA, y2_hhid, region)) %>% 
        mutate(region = factor(region, labels = tolower(levels(factor(y2commlink$id_01)))))

# merge regions with the prices
maize_price_w2 <- left_join(maize_price_w2, hhid_region_w2)

# write.csv(maize_price_w2, "M:/TZAYG/data/2012/producer_prices_w3.csv", row.names=FALSE)

rm(list=ls())

# -------------------------------------
# producer prices wave 3
# -------------------------------------

filepath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data/"
setwd( filepath )

prod_price_w3 <- read_spss("AG_SEC_5A.SAV")
maize_price_w3 <- subset(prod_price_w3, zaocode==11 & ag5a_01==1) %>% select(y3_hhid:ag5a_13)

# calculate the unit price of maize sold to customer 1 and 2
maize_price_w3 <- transmute(maize_price_w3, y3_hhid, unit_price1=ag5a_06/ag5a_05,
                          unit_price2=ag5a_13/ag5a_12)
average_price <- (maize_price_w3$unit_price1 + maize_price_w3$unit_price2)/2

# where there is no unit price for the second customer use customer 1 unit price
# otherwise use the average of the two prices.
maize_price_w3$final_price <- ifelse(is.na(maize_price_w3$unit_price2), maize_price_w3$unit_price1, average_price)

# read in regions
filepath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data/"
setwd( filepath )

hhid_region_w3 <- read.spss( "HH_SEC_A.SAV", to.data.frame=TRUE ) %>%
        select( y3_hhid, region=hh_a01_1 )

# also need to change the levels of the regions to lowercase
levels( hhid_region_w3$region ) <- tolower( levels( hhid_region_w3$region ) )
levels( hhid_region_w3$region )[levels( hhid_region_w3$region )=="mjini/magharibi unguja"] <- "mjini magharibi"

# join up with the maize prices
maize_price_w3 <- left_join(maize_price_w3, hhid_region_w2)

# write.csv(maize_price_w3, "M:/TZAYG/data/2012/producer_prices_w3.csv", row.names=FALSE)

