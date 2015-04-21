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

library( foreign )
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
HHB <- read.spss( 'HH_SEC_B.SAV', to.data.frame = TRUE )
HH <- select( HHB, y3_hhid, y2_hhid, indidy3, sex = hh_b02, age = hh_b04, status = hh_b05 )
by_hhid <- group_by( HH, y3_hhid ) %>% summarise( hh_size =length( indidy3 ) )
HH <- filter( HH, status == "HEAD" )
HH <- left_join( select( HH, -indidy3, -status ), by_hhid )

# Get a variable for whether the household is in a rural area or not
HHA <- read.spss( 'HH_SEC_A.SAV', to.data.frame = TRUE )
rural_weight <- select( HHA, y3_hhid, y3_rural, y3_weight )

# TODO {tom}: Add in a variable for the number of plots owned by a household.
# calcualte the households capital stocks. quantity refers to the 
# quantity of a particular item owned or rented by the household.
# value refers to the price in shillings for that item if it were 
# to be sold. 
AG11 <- read.spss( 'AG_SEC_11.SAV', to.data.frame = TRUE )
cap <- select( AG11, y3_hhid, itemname, quantity_own=ag11_01, value_own=ag11_02,
               quantity_rent=ag11_07, value_rent=ag11_09 )
cap <- group_by( cap, y3_hhid ) %>%
        summarise( own_sh=plus( quantity_own*value_own ),
                   rent_sh=plus( quantity_rent*value_rent ) )

# join together information on household characteristics and capital
HH_total <- left_join( HH, cap )

# write to file
# write.csv(HH_total, "M:/cleaned_data/HH_total_w3.csv", row.names=FALSE)

# ------------------------------------
# plot output and harvest details
# ------------------------------------

filepath <- "c:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Other/CropCodes.xlsx"
AG4A <- read.spss( 'AG_SEC_4A.SAV', to.data.frame = TRUE )
# input and output variables
plot.IO <- transmute( AG4A, y3_hhid, plotnum, zaocode, total_plot=ag4a_01,
               inter_crop=ag4a_04, seed_type=ag4a_08, seed_sh=ag4a_12,
               output_kg=ag4a_28, output_sh=ag4a_29 )

# write.csv(plot.IO, "M:/cleaned_data/2012/plot_output_w3.csv", row.names=FALSE)

# -------------------------------------
# plot level variables
# -------------------------------------

# agricultural questionnaire section 3A contains a lot of plot specific 
# information including fertilizer use.
AG3A <- read.spss( 'AG_SEC_3A.SAV', to.data.frame=TRUE )

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
plot.vars <- select( AG3A, y3_hhid, plotnum, soil=ag3a_10, soilq=ag3a_11,
               erosion=ag3a_13, slope=ag3a_17, irrig=ag3a_18,
               fallow=ag3a_22, fallow_years=ag3a_23, org=ag3a_41,
               orgQ1=ag3a_42, inorg1=ag3a_47, inorg_type1=ag3a_48,
               inorgQ1=ag3a_49, voucher1=ag3a_50, inorg2=ag3a_54,
               inorg_type2=ag3a_55, inorgQ2=ag3a_56, voucher2=ag3a_57,
               pest=ag3a_60, pestQ=ag3a_62_1, pestU=ag3a_62_2, 
               short_rain=ag3a_81, short_rain_crop=ag3a_82, owned=ag3a_25 )

# revalue the ownership variable to only OWNED or RENTED
plot.vars$owned <- revalue(plot.vars$owned,
                           c( "SHARED - RENT"="RENTED",
                              "SHARED - OWN"="OWNED",
                              "RENTED IN"="RENTED" ) )

# calculate the amount of nitrogen that was used per plot - main problem
# is that when two kinds of inorganic fertilizer have been used on the same plot
# it is necessary to calculate the sum of nitrogen in each fertilizer.
plot.vars <- mutate(plot.vars,
               inorg_type1 = factor( inorg_type1,
                                    labels=c( "DAP", "UREA", "TSP", "CAN", "SA",
                                             "generic NPK (TZA)", "MRP" ) ),
               inorg_type2 = factor( inorg_type2,
                                    labels=c( "DAP", "UREA", "TSP", "CAN", "SA",
                                             "generic NPK (TZA)", "MRP" ) ) )

# select only plot key and fertilizer type - melt the data frame so that the
# observational unit because fertilizer type per plot
s <- select( plot.vars, y3_hhid, plotnum, inorg_type1, inorg_type2 )
m <- melt( s, id = c( 'y3_hhid', 'plotnum' ) ) 

# filter m for eachtype of fertilizer and join this up with the quantity
ty1 <- filter( m, variable == 'inorg_type1' ) %>%
        left_join( select(plot.vars, y3_hhid, plotnum, quantity = inorgQ1 ) )
ty2 <- filter( m, variable == 'inorg_type2' ) %>%
        left_join( select(plot.vars, y3_hhid, plotnum, quantity = inorgQ2 ) )
tot <- rbind( ty1, ty2 ) %>% rename( type = value )

# read in fertilizer composition table
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")
fert.comp <- read.xls( "Data/Other/Fert_comp.xlsx", sheet = 2 ) %>%
        rename( type = Fert_type2 )
setwd(filepath)

fert.comp <- transform( fert.comp, P_share = as.numeric(P_share),
                        K_share = as.numeric( K_share ),
                        N_share = as.numeric( N_share ) )

# join composition table with fertilizer information on type
fert.vars <- left_join( tot, select( fert.comp, type, N_share, P_share, K_share ) )

# calculate Nitrogen share of each fertilizer type
fert.vars <- mutate( fert.vars, N=quantity*( N_share/100 ),
                     P=quantity*( P_share/100 ), K=quantity*( K_share/100 ) )

# calculate the sum of nitrogen used on each plot 
fert.vars <- group_by( fert.vars, y3_hhid, plotnum ) %>%
        summarise( nitrogen_kg=sum( N, na.rm=TRUE ),
                  phosphorous_kg=sum( P, na.rm=TRUE ),
                  potassium_kg=sum( K, na.rm=TRUE ) )

# join fertilizer information with other important variables
plot.vars <- left_join( plot.vars, fert.vars ) %>% left_join( lab )

# write.csv(plot.vars, "M:/cleaned_data/2012/plot_variables_w3.csv", row.names=FALSE)

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

# if both vill_price AND dis_price are NA then we can do nothing with them so
# I remove them here
good <- !( is.na( prices$vill_price ) & is.na( prices$dis_price ) )
prices <- prices[good, ]

# a final thing. whoever imputed the data for year seriously does not understand
# white space. The item_name variable is all messed up. following code fixes this
x <- strsplit( as.character(prices$item_name) , "  " )
prices$item_name <- factor( sapply( x, function( elt ) return( elt[[1]] ) ) )

# write everything to a CSV file to use later
# write.csv( prices, "M:/cleaned_data/2012/prices_w3.csv", row.names = FALSE )
