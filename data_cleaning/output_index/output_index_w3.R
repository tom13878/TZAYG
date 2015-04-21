# -------------------------------------
#' Liu Meyeres calculation for third 
#' wave of Tanzania panel (2012). 
# -------------------------------------

library( foreign )
library( plyr )
library( dplyr )

filepath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data/"
setwd( filepath )

# read in prices
prices <- read.csv( "M:/cleaned_data/2012/prices_clean_y3.csv" ) %>% 
        select( region, item_name, region_price )

# read in region to match up with price regions
regions <- read.spss( "HH_SEC_A.SAV", to.data.frame=TRUE ) %>%
        select( y3_hhid, region=hh_a01_1 )

# read in output
output <- read.csv( "M:/cleaned_data/2012/plot_output_y3.csv" )
        

# read in crop codes because itemnames in each year are different
crop_codes <- read.csv( "M:/cleaned_data/crop_code_y3.csv" )

# Need to have item_name, region variables in lower case to match up with crop
# name conversion table in crop_codes. Also one of the islands is spelt
# differently 
levels( output$zaocode ) <- tolower( levels( output$zaocode ) ) 
levels( prices$region ) <- tolower( levels( prices$region ) )
levels( prices$item_name ) <- tolower( levels( prices$item_name ) )
levels( regions$region ) <- tolower( levels( regions$region ) )
levels( regions$region )[levels( regions$region )=="mjini/magharibi unguja"] <- "mjini magharibi"

# watch out for duplicates. There are two farmers who grow maize and beans on
# their fields and for some reason they have reported the beans as though they
# were two separate crops on the same plot.
# test <- ddply( output, .( y3_hhid, plotnum, zaocode ), function( elt ) nrow( elt ) ) 
# test[test$V1 %in% 2,]

# filter on only those plots that have maize on them and count the number of
# of crops per maize plot. Also filter out the plots where there is a duplicate
# as described above
by_plot <- group_by(output, y3_hhid, plotnum) %>%
        filter(any(zaocode %in% "maize") & length(zaocode) == length(unique(zaocode))) %>%
        summarise(crop_count=length(zaocode)) # 3077 plots once a couple of duplicates have been removed.

# merge together the output, region and cropcodes and the price data. Need
# crop codes because zaocodes in the output data and the item_name in the price 
# data are different
output_maize <- left_join( by_plot, output )
output_maize <- left_join( output_maize, regions )
output_maize <- left_join( output_maize, crop_codes )
output_maize <- left_join( output_maize, prices )

# get a value variable which is the value of each crop on the plot.
output_maize$value <- output_maize$region_price * output_maize$output_kg

# finally calculate the Lie Myres index
output_maize <- ddply(output_maize, .(y3_hhid, plotnum), 
                      summarize, plot_value=sum(value),
                      maize_price=region_price[zaocode %in% "maize"],
                      maize_value=maize_price * output_kg[zaocode == "maize"],
                      output_kg_old=output_kg[zaocode == "maize"],
                      output_kg_new = plot_value/maize_price, 
                      maize_share = maize_value/plot_value * 100, crop_count = unique(crop_count),
                      beans = any(zaocode == "beans"), cash_crop=any(cash_crop))

# save output
# write.csv(output_maize, "M:/cleaned_data/output_index_y3.csv", row.names=FALSE)
