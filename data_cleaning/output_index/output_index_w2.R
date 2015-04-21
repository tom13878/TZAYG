#'output for year 2 using the Liu Meyers index
#'
setwd('M:/TZAYG/data/2010')

library(foreign)
library(plyr)
library(reshape2)
library(dplyr)

# Compute output index following Sheahan

prices <- read.csv('./prices_clean_w2.csv')

# make hhid a character vector
output <- read.csv('./plot_output_w2.csv') %>% 
        select(y2_hhid, plotnum, zaocode, output_kg, total_plot, inter_crop, seed_type)

crop_codes <- read.csv( "M:/cleaned_data/crop_code_y3.csv" )

# factor labels for regions in the second wave of the Tanzania data do not exist
# therefore we do not know which region is which and cannot merge with prices. 
# the code below grabs the region labels from wave 3 and matches up with the
# corresponding wave 2 hhid. However some wave 3 households do not hace a wave 2
# equivalent and therefore any household left as "                " is removed
# from the data.


setwd( "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data/" )

regions_w3 <- read.spss( "HH_SEC_A.SAV", to.data.frame=TRUE ) %>%
        select( y3_hhid, region=hh_a01_1 )
hhid_link <- read.spss( 'HH_SEC_B.SAV', to.data.frame = TRUE ) %>%
        select( y3_hhid, y2_hhid )
regions_w2 <- left_join( regions_w3, hhid_link ) %>% select( y2_hhid, region ) %>%
        filter( !( y2_hhid=="                " ) ) %>% unique()

setwd('M:/TZAYG/data/2010')

# put levels of certain variables to all lower case
# TODO: {tom morley} go back and cmake variables lower case in preparation file
output$zaocode <- tolower( output$zaocode )
levels( prices$item_name ) <- tolower( levels( prices$item_name ) )
levels( regions_w2$region ) <- tolower( levels( regions_w2$region ) )
levels( regions_w2$region )[levels( regions_w2$region )=="mjini/magharibi unguja"] <- "mjini magharibi"
output$y2_hhid <- as.character(output$y2_hhid)
# check for duplicates. For example if a farmer has responded twice for one crop on the
# same plot
test <- ddply( output, .( y2_hhid, plotnum, zaocode ), function( elt ) nrow( elt ) ) 
test[test$V1 %in% 2,]

# filter on only those plots that have maize on them and count the number of
# of crops per maize plot. Also filter out the plots where there is a duplicate
# as described above
by_plot <- group_by(output, y2_hhid, plotnum) %>%
        filter(any(zaocode %in% "maize") & length(zaocode) == length(unique(zaocode))) %>%
        summarise(crop_count=length(zaocode)) # 3077 plots once a couple of duplicates have been removed.


# merge together the output, region and cropcodes and the price data. Need
# crop codes because zaocodes in the output data and the item_name in the price 
# data are different
output_maize <- left_join( by_plot, output )
output_maize <- left_join( output_maize, regions_w2 )
output_maize <- left_join( output_maize, crop_codes )
output_maize <- left_join( output_maize, prices )


output_maize$value <- output_maize$region_price * output_maize$output_kg

# test for any plots that have something funny going on. There si a duplication
# somewhere
test <- group_by(output_maize, y2_hhid, plotnum, zaocode) %>% summarise(N=n())
summary(test) # should be at most 1 observations per hhid-plot-crop unit    
              # any greater and there is duplication! Or a problem with the regions

# One of the problem households is "1001003306011201", a closer look shows that
# this household in wave 2 is linked with more than one wave3 household
hhid_link[hhid_link$y2_hhid=="1001003306011201",]
hhid_link[hhid_link$y2_hhid=="1002011311009601",]
test2 <- na.omit(test)
test2[test2$N > 1,]

# finally calculate the Liu Myres index
output_maize1 <- ddply(output_maize, .(y2_hhid, plotnum), 
                      summarize, plot_value=sum(value),
                      maize_price=region_price[zaocode %in% "maize"]),
                      maize_value=maize_price * output_kg[zaocode == "maize"],
                      output_kg_old=output_kg[zaocode == "maize"],
                      output_kg_new = plot_value/maize_price, 
                      maize_share = maize_value/plot_value * 100, crop_count = unique(crop_count),
                      beans = any(zaocode == "beans"), cash_crop=any(cash_crop))




# find plots which contain at least some maize on them and combine with region, zone and
# crop codes data and also with price data
output.maize <- ddply(output.maize, .(y2_hhid, plotnum), transform, maize = any(zaocode == "Maize"))
output.maize <- output.maize[output.maize$maize, ]
bad <- is.na(output.maize$maize)
output.maize <- output.maize[!bad, ]
output.maize <- merge(output.maize, select(crop.codes, crop.name, itemname, cash.crop),
                      by.x = "zaocode", by.y = "crop.name", all.x = TRUE)
output.maize <- left_join(output.maize, select(prices, item_name, region, region_price))

count <- melt(select(output.maize, y2_hhid, plotnum, zaocode), id = c("y2_hhid", "plotnum"))
count <- ddply(count, .(y2_hhid, plotnum), summarize,
               crop.count = length(unique(value[!is.na(value)])))
output.maize <- left_join(output.maize, count)
output.maize <- output.maize[!is.na(output.maize$zaocode), ]

output.maize$value <- output.maize$region_price * output.maize$output_kg

# too many values????

output.maize.test <- ddply(output.maize, .(y2_hhid, plotnum), 
                      summarize, plot_value = sum(value),
                      maize_price = region_price[zaocode == "Maize"]),
                      maize_value = maize_price * output_kg[zaocode == "Maize"],
                      output_kg_old = output_kg[zaocode == "Maize"],
                      output_kg_new = plot_value/maize_price, 
                      maize_share = maize_value/plot_value * 100, crop_count = unique(crop.count),
                      beans = any(zaocode == "Beans"), cash_crop = any(cash_crop == "YES"))


output_maize <- ddply(output_maize, .(y3_hhid, plotnum), 
                      summarize, plot_value=sum(value),
                      maize_price=region_price[zaocode %in% "maize"],
                      maize_value=maize_price * output_kg[zaocode == "maize"],
                      output_kg_old=output_kg[zaocode == "maize"],
                      output_kg_new = plot_value/maize_price, 
                      maize_share = maize_value/plot_value * 100, crop_count = unique(crop_count),
                      beans = any(zaocode == "beans"), cash_crop=any(cash_crop))

output <- filter(output, zaocode == "Maize") %>% 
        select(y2_hhid, plotnum, total.plot, inter.crop, seed.type)
output.maize <- left_join(output.maize, output)

# write.csv(output.maize, "./2010/output_maize_y2.csv", row.names = FALSE)
