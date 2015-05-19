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
source("M:/TZAYG/functions/plus.R") 
source("M:/TZAYG/functions/missing.plot.R") 

# A. Household level data - Create HH level indicators

# 

HHB <- read.dta( "./Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_B.dta",
                   convert.factors = TRUE )

AG11 <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC11.dta",
                     convert.factors = TRUE)

HH <- select( HHB, y2_hhid, indidy2, sex = hh_b02, age = hh_b04, status = hh_b05 )
by_hhid <- group_by( HH, y2_hhid ) %>% summarise( hh_size =length( indidy2 ) )
HH <- filter( HH, status == "HEAD" )
HH <- left_join( select( HH, -indidy2, -status ), by_hhid )
levels(HH$sex) <- tolower(levels(HH$sex))

# Compute capital stock per HH
cap <- select( AG11, y2_hhid, itemcode, quantity_own=ag11_01, value_own=ag11_02,
               quantity_rent=ag11_07, value_rent=ag11_09 )
cap <- group_by( cap, y2_hhid ) %>%
        summarise( own_sh=plus( quantity_own*value_own ),
                   rent_sh=plus( quantity_rent*value_rent ) )

HH_total <- left_join( HH, cap )

write.csv( HH_total, "M:/TZAYG/data/2010/HH_total_w2.csv", row.names=FALSE )

# B. plot level input - output data

AG4A <- read.dta( "./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC4A.dta",
                    convert.factors=TRUE )
CropCodes <- read.xls( "./Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet=1 )



plot.IO <- transmute( AG4A, y2_hhid, plotnum, zaocode=factor( zaocode,
                      levels=CropCodes$zaocode, labels=CropCodes$CropName ),
                      total_plot=ag4a_01, inter_crop=ag4a_04, seed_type=ag4a_23,
                      seed_sh=ag4a_21, output_kg=ag4a_15, output_sh=ag4a_16 )

plot.IO$zaocode <- tolower(plot.IO$zaocode)

write.csv( plot.IO, "M:/TZAYG/data/2010/plot_output_w2.csv", row.names=FALSE )

#' C. area variables
areas <- read.dta( './Data/Plot_size/areas_tza_y2_imputed.dta')
areas <- select( areas, y2_hhid=case_id, plotnum, area_est=area_sr_orig, area_gps, area_gps_imputed=area_gps_mi_50 )
write.csv(areas, "M:/TZAYG/data/2010/areas_w2.csv", row.names=FALSE)

#' D. plot variables
AG3A <- read.dta( "./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC3A.dta",
                    convert.factors=TRUE )

# start with househjold and family labour
lab <- select( AG3A, y2_hhid, plotnum, ag3a_70_id1:ag3a_72_9 )
bad <- grep( "ag3a_70_id", names( lab ) )

# remove houshold labour IDs and question ag3a_71 which we don't need
lab <- lab[, -bad]
lab <- select( lab, -ag3a_71 )

# remove the wage paid in shillings to hired labour - might want this back later
bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]

# create a dataframe with just family and hired labour
lab <- transmute( lab, y2_hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:26], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 27:ncol( lab )], na.rm=TRUE ) )

# create a datafrane with plot level characteristics
plot_vars <- select( AG3A, y2_hhid, plotnum, soil=ag3a_09, soilq=ag3a_10,
                     erosion=ag3a_12, slope=ag3a_16, irrig=ag3a_17,
                     fallow=ag3a_21, fallow_years=ag3a_22, org=ag3a_39,
                     orgQ1=ag3a_40, inorg1=ag3a_45, inorg_type1=ag3a_46,
                     inorgQ1=ag3a_47, voucher1=ag3a_48, inorg_price1=ag3a_49,
                     inorg2=ag3a_52,inorg_type2=ag3a_53, inorgQ2=ag3a_54,
                     voucher2=ag3a_55, inorg_price2=ag3a_56, pest=ag3a_58,
                     pestQ=ag3a_60_1, pestU=ag3a_60_2, short_rain=ag3a_74,
                     short_rain_crop=ag3a_75, owned=ag3a_24 )

# change levels of the fertilizer names
plot_vars <- mutate(plot_vars,
                    inorg_type1 = factor( inorg_type1,
                                          labels=c( "DAP", "UREA", "TSP", "CAN", "SA",
                                                    "generic NPK (TZA)", "MRP" ) ),
                    inorg_type2 = factor( inorg_type2,
                                          labels=c( "DAP", "UREA", "TSP", "CAN", "SA",
                                                    "generic NPK (TZA)", "MRP" ) ) )

# put together fertilizer variables. Each plot has the possibility of two 
# types of fertilizer. As a result getting the total value of fertilizer on a
# plot requires some manipulation
s <- select( plot_vars, y2_hhid, plotnum, inorg_type1, inorg_type2 )
m <- melt( s, id = c( 'y2_hhid', 'plotnum' ) ) 

# filter m for eachtype of fertilizer and join this up with the quantity
ty1 <- filter( m, variable == 'inorg_type1' ) %>%
        left_join( select(plot_vars, y2_hhid, plotnum, quantity=inorgQ1, price=inorg_price1 ) ) 
ty2 <- filter( m, variable == 'inorg_type2' ) %>%
        left_join( select(plot_vars, y2_hhid, plotnum, quantity=inorgQ2, price=inorg_price2 ) ) 
tot <- rbind( ty1, ty2 ) %>% rename( type=value )

# read in fertilizer composition table
fert_comp <- read.xls( "Data/Other/Fert_comp.xlsx", sheet=2 ) %>%
        rename( type=Fert_type2 )

fert_comp <- transform( fert_comp, P_share = as.numeric(P_share),
                        K_share = as.numeric( K_share ),
                        N_share = as.numeric( N_share ) )

# join composition table with fertilizer information on type
fert_vars <- left_join( tot, select( fert_comp, type, N_share, P_share, K_share ) )

# calculate Nitrogen share of each fertilizer type
fert_vars <- mutate( fert_vars, N=quantity*( N_share/100 ),
                     P=quantity*( P_share/100 ), K=quantity*( K_share/100 ) )

# set quantity, price and N, that are NA to zero
fert_vars$quantity <- ifelse(is.na(fert_vars$quantity), 0, fert_vars$quantity)
fert_vars$price <- ifelse(is.na(fert_vars$price), 0, fert_vars$price)
fert_vars$N <- ifelse(is.na(fert_vars$N), 0, fert_vars$N)

# calculate a price of nitrogen per plot by first converting nitrogen
# into its nitrogen components, calcualting a weighted average of the prices
# and finally using this weighted price and the total quantity of nitrogen to
# make a unit price of nitrogen.
fert_vars <- group_by( fert_vars, y2_hhid, plotnum ) %>%
        summarise( price_type1=price[variable %in% "inorg_type1"],
                   price_type2=price[variable %in% "inorg_type2"],
                   quantity_type1=N[variable %in% "inorg_type1"],
                   quantity_type2=N[variable %in% "inorg_type2"],
                   nitrogen_kg=sum( N, na.rm=TRUE ),
                   phosphorous_kg=sum( P, na.rm=TRUE ),
                   potassium_kg=sum( K, na.rm=TRUE ),
                   nitrogen_price=(price_type1*quantity_type1+price_type2*quantity_type2)/nitrogen_kg,
                   nitrogen_unit_price=nitrogen_price/nitrogen_kg)


# join fertilizer information with other important variables
plot_vars <- left_join( plot_vars, fert_vars ) %>% left_join( lab )

# write.csv(plot_vars, "M:/TZAYG/data/2010/plot_variables_w2.csv", row.names=FALSE)
     

#' E. read in the community consumer prices from the community questionnaire and
#'    return as output a cleaned set of prices that can be used to calculate the
#'    Liu meyers index

CQSECCJ <- read.dta("Data/Tanzania/2010_11/Stata/TZNPS2COMDTA/COMSEC_CJ.dta",
                    convert.factors = TRUE)

prices <- select(CQSECCJ, region = id_01, item_name=itemname, vill_unit = cm_j01a, vill_weight = cm_j01b,
                 vill_price = cm_j01c, dis_unit = cm_j02a, dis_weight = cm_j02b, dis_price = cm_j02c)

# one item is "", get rid of this one

prices <- filter(prices, !(item_name==""))

prices$vill_weight[prices$vill_weight == 0] <- NA
prices$vill_price[prices$vill_price == 0] <- NA
prices$dis_weight[prices$dis_weight == 0] <- NA
prices$dis_price[prices$dis_price == 0] <- NA

# get unit prices - making sure to convert units into kilograms, i.e. price
# per kilogram.
prices$vill_price <- with( prices,
                           ifelse( vill_unit == "Grams", vill_price/( vill_weight/1000 ),
                                   ifelse( vill_unit == "Kilograms", vill_price/( vill_weight ),
                                           ifelse( vill_unit == "Litre", vill_price/( vill_weight ),
                                                   ifelse( vill_unit == "Millilitre", vill_price/( vill_weight/1000 ),
                                                           ifelse( vill_unit == "Pieces", vill_price/( vill_weight ),
                                                                   vill_price/vill_weight ) ) ) ) ) )

prices$dis_price <- with( prices,
                          ifelse( dis_unit == "Grams", dis_price/( dis_weight/1000 ),
                                  ifelse( dis_unit == "Kilograms", dis_price/( dis_weight ),
                                          ifelse( dis_unit == "Litre", dis_price/( dis_weight ),
                                                  ifelse( dis_unit == "Millilitre", dis_price/( dis_weight/1000 ),
                                                          ifelse( dis_unit == "Pieces", dis_price/( dis_weight ),
                                                                  dis_price/dis_weight ) ) ) ) ) )

# don't need original prices anymore, just local and district level unit prices
prices <- select( prices, region, item_name, vill_price, dis_price )

# # test to see if each item is available for each region
# test <- unique( select(prices, region, item_name) )
# test_split <- split(test, test$region)
# test_split_result <- sapply(test_split, function(elt) unique(test$item_name) %in% elt$item_name)
# table(test_split_result)


# put all regions to lower case to match up between different waves
prices$region <- tolower(prices$region)

# we need a price for every good in every region. So merge the prices with
# every possible region and price combination
prices <- left_join( unique( select(prices, region, item_name) ), prices)

# write.csv(prices, "M:/TZAYG/data/2010/prices_w2.csv", row.names = FALSE)

