# -------------------------------------
#' preparation file for third wave (2012)
#' of Tanzania data. The output of the file
#' is a collection of databases that will be
#' combined in to form a complete cross 
#' section for 2012.
#' 
#' Each section can be run seperately
#' provided that that all the packages 
#' and functions needed are called first
#' 
#' Unlike earlier preparation files, data
#' on area is kept separate. This is a
#' result of missing area measurments
#' which are imputed in another file and
#' joined to the main data later.
# -------------------------------------

# set working directory and install packages
filepath <- 'W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data'
setwd( filepath )

library( haven )
library( reshape2 )
library( gdata )
library( plyr )
library( dplyr )

source( "M:/TZAYG/functions/plus.R" ) 

# -------------------------------------
# household data
# -------------------------------------

# get basic infomation from the household survey: variable indidy3 is the
# identification number of each individual in the household. Only information
# on the HEAD of the household (who is assumed to be the plot manager) is
# required for later analysis
HHB <- read_dta( 'HH_SEC_B.dta' )
HHB$hh_b02 <- as_factor(HHB$hh_b02) %>% tolower()
HH <- select( HHB, y3_hhid, y2_hhid, indidy3, sex = hh_b02, age = hh_b04, status = hh_b05 )
by_hhid <- ddply( HH, .(y3_hhid), summarise, hh_size =length( indidy3 ) )
HH <- subset( HH, status %in% 1 )
HH <- left_join( select( HH, -indidy3, -status ), by_hhid )
# levels(HH$sex) <- tolower(levels(HH$sex))
# Get a variable for whether the household is in a rural area or not
HHA <- read_dta( 'HH_SEC_A.dta' )
rural_weight <- select( HHA, y3_hhid, y3_rural, y3_weight )

# TODO {tom}: Add in a variable for the number of plots owned by a household.
# calcualte the households capital stocks. quantity refers to the 
# quantity of a particular item owned or rented by the household.
# value refers to the price in shillings for that item if it were 
# to be sold. 
AG11 <- read_dta( 'AG_SEC_11.dta' )
cap <- select( AG11, y3_hhid, itemname, quantity_own=ag11_01, value_own=ag11_02,
               quantity_rent=ag11_07, value_rent=ag11_09 )
cap <- ddply( cap, .(y3_hhid), summarise, own_sh=plus( quantity_own*value_own ),
                   rent_sh=plus( quantity_rent*value_rent ) )

# join together information on household characteristics and capital
HH_total <- left_join( HH, cap )

# write_dta(HH_total, "M:/TZAYG/data/2012/HH_total_w3.dta")

# ------------------------------------
# plot output and harvest details
# ------------------------------------

AG4A <- read_dta( 'AG_SEC_4A.dta' )
# input and output variables
plot.IO <- transmute( AG4A, y3_hhid, plotnum, zaocode, total_plot=ag4a_01,
               inter_crop=ag4a_04, seed_type=ag4a_08, seed_sh=ag4a_12,
               output_kg=ag4a_28, output_sh=ag4a_29 )

# write_dta( plot.IO, "M:/TZAYG/data/2012/plot_output_w3.dta" )

# -------------------------------------
# plot level variables - need to work this out with the final
# -------------------------------------

# agricultural questionnaire section 3A contains a lot of plot specific 
# information including fertilizer use.
AG3A <- read_dta( 'AG_SEC_3A.dta' )

# the plotnumber variable in AG3A has whitespace that needs to be removed
gsub( " ", "", "M1 ", fixed=TRUE ) # returns "M1"
AG3A$plotnum <- gsub( " ", "", AG3A$plotnum, fixed=TRUE )

# calculate the total labour days for hired and family labour. This requires
# manipulation because of the way values were entered into the survey.
# see agricultural questionnaire Qs 72 and 74 for details.
lab <- select( AG3A, y3_hhid, plotnum, ag3a_72_id1:ag3a_74_16 )
bad <- grep( "ag3a_72_id", names( lab ) )
lab <- lab[, -bad]
bad <- names( lab )[( length( lab )-15 ):length( lab )][seq( from=4, to=16, by=4 )]
lab <- lab[, -which( names( lab ) %in% bad )]

lab <- transmute( lab, y3_hhid, plotnum,
                  fam_lab_days=rowSums( lab[, 3:30], na.rm=TRUE ),
                  hir_lab_days=rowSums( lab[, 32:ncol( lab )], na.rm=TRUE ) )

# Select variables that are important for analysis
# fallow has 0 if plot has never been left fallow and 98 if the respondent does
# not know when the last time the plot was left fallow
plot_vars <- select( AG3A, y3_hhid, plotnum, soil=ag3a_10, soilq=ag3a_11,
               erosion=ag3a_13, slope=ag3a_17, irrig=ag3a_18,
               fallow=ag3a_22, fallow_years=ag3a_23, org=ag3a_41,
               orgQ1=ag3a_42, inorg1=ag3a_47, inorg_type1=ag3a_48,
               inorgQ1=ag3a_49, voucher1=ag3a_50, inorg_price1=ag3a_51,
               inorg2=ag3a_54, inorg_type2=ag3a_55, inorgQ2=ag3a_56,
               voucher2=ag3a_57, inorg_price2=ag3a_58, pest=ag3a_60,
               pestQ=ag3a_62_1, pestU=ag3a_62_2, short_rain=ag3a_81,
               short_rain_crop=ag3a_82, owned=ag3a_25 )

# revalue the ownership variable to only OWNED or RENTED
# plot_vars$owned <- revalue(plot_vars$owned,
#                            c( "SHARED - RENT"="RENTED",
#                               "SHARED - OWN"="OWNED",
#                               "RENTED IN"="RENTED" ) )

# calculate the amount of nitrogen that was used per plot - main problem
# is that when two kinds of inorganic fertilizer have been used on the same plot
# it is necessary to calculate the sum of nitrogen in each fertilizer.
plot_vars <- mutate(plot_vars,
               inorg_type1 = factor( inorg_type1,
                                    labels=c( "DAP", "UREA", "TSP", "CAN", "SA",
                                             "generic NPK (TZA)", "MRP" ) ),
               inorg_type2 = factor( inorg_type2,
                                    labels=c( "DAP", "UREA", "TSP", "CAN", "SA",
                                             "generic NPK (TZA)", "MRP" ) ) )

# select only plot key and fertilizer type - melt the data frame so that the
# observational unit because fertilizer type per plot
s <- select( plot_vars, y3_hhid, plotnum, inorg_type1, inorg_type2 )
m <- melt( s, id = c( 'y3_hhid', 'plotnum' ) ) 

# filter m for eachtype of fertilizer and join this up with the quantity
ty1 <- filter( m, variable == 'inorg_type1' ) %>%
        left_join( select(plot_vars, y3_hhid, plotnum, quantity=inorgQ1, price=inorg_price1 ) ) 
ty2 <- filter( m, variable == 'inorg_type2' ) %>%
        left_join( select(plot_vars, y3_hhid, plotnum, quantity=inorgQ2, price=inorg_price2 ) ) 
tot <- rbind( ty1, ty2 ) %>% rename( type=value )

# read in fertilizer composition table
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")
fert_comp <- read.xls( "Data/Other/Fert_comp.xlsx", sheet=2 ) %>%
        rename( type=Fert_type2 )
setwd(filepath)
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
fert_vars <- group_by( fert_vars, y3_hhid, plotnum ) %>%
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

# write.csv(plot_vars, "M:/TZAYG/data/2012/plot_variables_w3.csv", row.names=FALSE)

# -------------------------------------
# consumer prices of foodstuffs
# -------------------------------------

CQSECCF <- read.spss( "COM_SEC_CF.SAV", to.data.frame=TRUE )

# There are both village and district level prices
# rename variables to something that makes sense and select only what you want
# variables prefixed by vill refer to village market measurements. Variables
# prefixed by dis refer to district market measurements 
prices <- select( CQSECCF, region=id_01, item_name, vill_unit=cm_f06meas,
                 vill_weight=cm_f06wght, vill_price=cm_f06pri, dis_unit = cm_f06meas2,
                 dis_weight=cm_f06wght2, dis_price=cm_f06pri2 )

# set some values to NA rather than zero, otherwise end up with strange values
# when converting units. Also it doesn't make sense when there is a weight but no
# price, or a price and no weight
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

# test to see if each item is available for each region
test <- unique( select(prices, region, item_name) )
test_split <- split(test, test$region)
test_split_result <- sapply(test_split, function(elt) unique(test$item_name) %in% elt$item_name)
table(test_split_result)

# need to make sure there is a price observation for every item for every region
prices <- left_join( unique( select(prices, region, item_name) ), prices)

# a final thing. whoever imputed the data for year seriously does not understand
# white space. The item_name variable is all messed up. following code fixes this
x <- strsplit( as.character(prices$item_name) , "  " )
prices$item_name <- factor( sapply( x, function( elt ) return( elt[[1]] ) ) )

# write.csv( prices, "M:/TZAYG/data/2012/prices_w3.csv", row.names = FALSE )
