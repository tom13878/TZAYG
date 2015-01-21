#' winsnoring prices for year 2
#' 
#' prices in 2010


setwd('M:/cleaned_data')
library(plyr)
library(dplyr)
source("M:/TZAYG/winsor.R")

prices <- read.csv("./2010/prices.csv")

# prices <- ddply(prices, .(itemname), function(elt) winsor(elt,"price.loc", 0.975))
# prices <- ddply(prices, .(itemname), function(elt) winsor(elt, "price.dis", 0.975))

prices.region <- ddply(prices, .(region, itemname), summarize,
                       region.price.loc = mean(price.loc, na.rm = TRUE),
                       region.price.dis = mean(price.dis, na.rm = TRUE),
                       num.obs.loc = sum(is.na(price.loc)),
                       num.obs.dis = sum(is.na(price.dis)),
                       region.price.tot = sum(region.price.loc,
                                              region.price.dis, na.rm = TRUE)/2,
                       num.obs.tot = sum(num.obs.loc, num.obs.dis, na.rm = TRUE))

prices.national <- ddply(prices, .(itemname), summarize,
                         national.price.loc = mean(price.loc, na.rm = TRUE),
                         national.price.dis = mean(price.dis, na.rm = TRUE),
                         national.price.tot = sum(national.price.loc,
                                                  national.price.dis,
                                                  na.rm = TRUE)/2)

prices.region$region.price.tot[prices.region$num.obs.tot < 5] <- NA
prices.region <- left_join(prices.region, prices.national)
bad <- is.na(prices.region$region.price.tot)
prices.region$region.price.tot[bad] <- prices.region$national.price.tot[bad]

write.csv(prices.region,"./2010/prices_winsor_y2.csv", row.names = FALSE)
