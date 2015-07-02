# maize counts
library(haven)
library(dplyr)

pout10 <- read.csv("M:/TZAYG/data/2010/plot_output_w2.csv")
by_hhid <- group_by(pout10 , y2_hhid) %>% summarise(maize=any(zaocode %in% "maize"))
by_plot <- group_by(pout10 , y2_hhid, plotnum) %>% summarise(maize=any(zaocode %in% "maize"))
        
pout12 <- read_dta("M:/TZAYG/data/2012/plot_output_w3.dta")
by_hhid2 <- group_by(pout12, y3_hhid) %>% summarise(maize=any(zaocode %in% 11))

# start from the lowest level of analysis which is the plot level
# for rural households that produce maize
filepath <- 'W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data'
setwd( filepath )

pout <- read.dta( 'AG_SEC_4A.dta', convert.factors=TRUE ) %>% transmute( y3_hhid, plotnum, zaocode, total_plot=ag4a_01,
                                                                         inter_crop=ag4a_04, seed_type=ag4a_08, seed_sh=ag4a_12,
                                                                         output_kg=ag4a_28, output_sh=ag4a_29, mono=ag4a_04 )

# now remove anything that is not a rural household.
HHA <- read_dta( 'HH_SEC_A.dta' )
rural <- select( HHA, y3_hhid, rural=y3_rural )
pout <- left_join(pout, rural)
pout <- filter(pout, rural %in% 1)

# count the number of fields that have maize on them
pout <- ddply(pout, .(y3_hhid, plotnum), transform, maize=any(zaocode %in% "MAIZE"))

# before selecting only maize plots let's see how many plots have maize within our sample
x <- unique(select(pout, y3_hhid, plotnum, maize)) #51%

# and how many farmers grow maize in total
y <- ddply(pout, .(y3_hhid), summarise, maize=any(maize)) #54%

# now filter off to get only the maize fields
pout <- filter(pout, maize)