#'output for year 2 using the Liu Meyers index
#'
setwd('M:/TZAYG/data/2010')

library(foreign)
library(plyr)
library(reshape2)
library(dplyr)
library(haven)

# Compute output index following Sheahan

prices <- read.csv('./prices_clean_w2.csv')

# make hhid a character vector
output <- read.csv('./plot_output_w2.csv') %>% 
        select(y2_hhid, plotnum, zaocode, output_kg, total_plot, inter_crop, seed_type)

crop_codes <- read.csv( "M:/cleaned_data/crop_code_y3.csv" )

# add an extra entry to the crop-codes file because casava is spelled differently
# betweem wave 2 and 3 . . . typical!
crop_codes <- rbind(crop_codes, data.frame(zaocode="casava", item_name="casava fresh", cash_crop=FALSE))
crop_codes$item_name[crop_codes$zaocode=="bulrush millet"] <- "millet (grain)"
crop_codes$item_name[crop_codes$zaocode=="tobacco"] <- "mangoes"

# wave 2 data does not have factor labels for regions, so these must be added
# manually using the information at the back of the wave 2 durvey
region_names <- c("dodoma", "arusha", "kilimanjaro", "tanga", "morogoro", "pwani",
                  "dar es salaam", "lindi", "mtwara", "ruvuma", "iringa", "mbeya",
                  "singida", "tabora", "rukwa", "kigoma", "shinyanga", "kagera",
                  "mwanza", "mara", "manyara", "kaskazini unguja", "kusini unguja",
                  "mjini magharibi", "kaskazini pemba", "kusini pemba")
hhid_region <- read_dta("C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_A.dta")
hhid_region <- select(hhid_region, y2_hhid, region)
hhid_region$region <- factor(hhid_region$region, labels=region_names)

# watch out for ghost households! CREEPY . .. .. !!!!
hhid_region$y2_hhid <- as.character(as.numeric(hhid_region$y2_hhid))

# put levels of certain variables to all lower case
# TODO: {tom morley} go back and cmake variables lower case in preparation file
output$zaocode <- tolower( output$zaocode )
levels( prices$item_name ) <- tolower( levels( prices$item_name ) )
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
        summarise(crop_count=length(zaocode)) 

# merge together the output, region and cropcodes and the price data. Need
# crop codes because zaocodes in the output data and the item_name in the price 
# data are different
output_maize <- left_join( by_plot, output )
output_maize <- left_join( output_maize, hhid_region )
output_maize <- left_join( output_maize, crop_codes )
output_maize <- left_join( output_maize, prices )

# just a banana, coconut, plum and pineaple which are still problems
output_maize[is.na(output_maize$region_price),]

# all( crop_codes$item_name %in% unique(prices$item_name) )
# all(unique(hhid_region$region) %in% unique(prices$region))
# all(unique(prices$region) %in% unique(hhid_region$region))


output_maize$value <- output_maize$region_price * output_maize$output_kg

# test for any plots that have something funny going on. There si a duplication
# somewhere
# test <- group_by(output_maize, y2_hhid, plotnum, zaocode) %>% summarise(N=n())
# summary(test) # should be at most 1 observations per hhid-plot-crop unit    
#               # any greater and there is duplication! Or a problem with the regions
# 
# # One of the problem households is "1001003306011201", a closer look shows that
# # this household in wave 2 is linked with more than one wave3 household
# hhid_link[hhid_link$y2_hhid=="1001003306011201",]
# hhid_link[hhid_link$y2_hhid=="1002011311009601",]
# test2 <- na.omit(test)
# test2[test2$N > 1,]

# there are some NA values for zaocode that stop the ddply from working. Not
# clear why these have an output level if they have no zaocode . . . 
# TODO {tom morley}{ Look back over the output section to find out why this might be }
output_maize <- output_maize[!(is.na(output_maize$zaocode)),]

# finally calculate the Liu Myres index
output_maize <- ddply(output_maize, .(y2_hhid, plotnum), 
                      summarize, plot_value=sum(value),
                      maize_price=region_price[zaocode %in% "maize"],
                      maize_value=maize_price * output_kg[zaocode=="maize"],
                      output_kg_old=output_kg[zaocode=="maize"],
                      output_kg_new=plot_value/maize_price, 
                      maize_share=maize_value/plot_value * 100,
                      crop_count = unique(crop_count),
                      beans=any(zaocode == "beans"), cash_crop=any(cash_crop))



# write_dta(output_maize, "M:/TZAYG/data/2010/output_index_w2.dta")