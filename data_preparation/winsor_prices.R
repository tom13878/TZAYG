#' winsoring of prices data at the 95th percentile for 2008 and 2010
#' winsoring function for data frames is stored on github and called
#' winsor.R. Aggregation of prices top replace the prices for which we do not
#' have a good regional level with the national level prices.
#' The output of this script is stored as cleaned data files that can be
#' used to create the Liu-Meyres output index

setwd('M:/cleaned_data')
library(plyr)
library(dplyr)
source("M:/TZAYG/winsor.R")

# prices in 2008
prices <- read.csv("./2008/prices.csv")
prices.winsor <- ddply(prices, .(itemname), function(elt) winsor(elt, "price.unit", 0.90))

prices.region <- ddply(prices.winsor, .(region, itemname), summarize,
                       region.price = mean(price.unit, na.rm = TRUE),
                       num.obs = length(price.unit))

prices.national <- ddply(prices, .(itemname), summarize,
                         national.price = mean(price.unit, na.rm = TRUE))

prices.region$region.price[prices.region$num.obs < 5] <- NA
prices.region <- left_join(prices.region, prices.national) 
bad <- is.na(prices.region$region.price)
prices.region$region.price[bad] <- prices.region$national.price[bad]
prices.region <- select(prices.region, region, everything(), -national.price)

write.csv(prices.region,"./2008/prices_winsor_y1.csv", row.names = FALSE )

