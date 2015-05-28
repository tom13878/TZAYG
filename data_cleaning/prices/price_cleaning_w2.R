#' Winsoring of 2010 price data to get rid of outliers. Two methods are 
#' considered: Winsoring at the 95th percentile and winsoring depeding on the
#' deviation from the median. Histograms and qqplots indicate that winsoring
#' according to the second method is preferable, this also has a more economic 
#' interpretation
#' 
#' regional prices for and item where there are less than 5 observations are
#' replaced with the national average for that item. 
#' 
#' The oputput of this script is a set of prices for 2010 which will be used to
#' construct the Liu Meyers output index (Sheahan 2011)


setwd('M:/TZAYG/data/2010')
library(ggplot2)
library(plyr)
library(dplyr)
source("M:/TZAYG/functions/winsor.R")

# plotting functions
# plotpattern <- function(df, col){
#        ggplot(df, aes_string(col)) + geom_histogram(fill = 'red', colour = 'black', binwidth = 20) +
#                 labs(title = paste(as.character(df$itemname[1]), 'price plot', sep = ' ')) +
#                 theme_bw()
# }
# 
# quantile_plot <- function(x, y){
#         qqplot(x, y, col = 'red', xlab = 'Theoretical Quantiles',
#                ylab = 'Sample Quantiles', main = 'qqplot')
#         
#         abline(a = 0, b = 1, col = 4)      
# }
# 
# dens_plot <- function(x, breaks = 25, freq = FALSE){
#         hist(x, breaks = breaks, col = 'lightgreen', freq = freq)
#         curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE, col="darkblue", lwd=2)
# }

# price data from 2010 preparation file
prices <- read.csv("./prices_w2.csv")

# problem: there are large outliers in the price data which need to be removed
# before analysis. Assume that maize prices are normally distributed across
# tanzania. QQplot shows problem with outliers
# a flick through the plots below shows the problem with some of the data. It is
# the high cost values that seem to have the highest outliers.
# d_ply(prices, .(itemname), function(elt) plotpattern(elt, 'vill_price'), .print = TRUE)
# 
# range(prices$vill_price, na.rm = TRUE)
# summary(prices$vill_price)
# 
# range(prices$dis_price, na.rm = TRUE)
# summary(prices$dis_price)
# 
# x <- rnorm(231, mean = 0, sd = 1)
# m <- subset(prices, item_name == 'Maize (grain)') %>%
#         mutate(dis_price = (dis_price - mean(dis_price, na.rm = TRUE))/sd(dis_price, na.rm = TRUE),
#                vill_price = (vill_price - mean(vill_price, na.rm = TRUE))/sd(vill_price, na.rm = TRUE))
# 
# quantile_plot(x, m$dis_price)
# quantile_plot(x, m$vill_price)

# winsoring prices to get rid of outliers using median average deviation 
# winsoring function
# see http://www.r-bloggers.com/winsorization/ for details
prices_winsor <- ddply(prices, .(item_name), function(elt) winsor3(elt, c('vill_price', 'dis_price'), multiple = 3))

# d_ply(prices.winsor, .(itemname), function(elt) plotpattern(elt, 'vill_price'), .print = TRUE)
# d_ply(prices.winsor, .(itemname), function(elt) plotpattern(elt, 'dis_price'), .print = TRUE)
# 
# # closer look at Maize prices only
# m2 <- subset(prices.winsor, item_name == 'Maize (grain)') %>% 
#         mutate(vill_price = (vill_price - mean(vill_price, na.rm = TRUE))/sd(vill_price, na.rm = TRUE),
#                dis_price = (dis_price - mean(dis_price, na.rm = TRUE))/sd(dis_price, na.rm = TRUE))
# 
# quantile_plot(x, m2$vill_price)
# quantile_plot(x, m2$dis_price)
# 
# dens_plot(m2$vill_price, breaks = 20)
# dens_plot(m2$dis_price, breaks = 20)

# create price variables and make a final price index
region_price <- ddply( prices_winsor, .( region, item_name ), summarize,
                       vill_price_region=mean( vill_price, na.rm = TRUE ),
                       dis_price_region=mean( dis_price, na.rm = TRUE ),
                       vill_obs_region=sum( is.na( vill_price ) ),
                       dis_obs_region=sum( is.na( dis_price ) ),
                       region_price=sum( vill_price_region,
                                         dis_price_region, na.rm=TRUE )/2,
                       tot_obs=sum( vill_obs_region, dis_obs_region, na.rm=TRUE ) )

national_price <- ddply( prices, .( item_name ), summarize,
                         vill_price_national=mean( vill_price, na.rm=TRUE ),
                         dis_price_national=mean( dis_price, na.rm=TRUE ),
                         national_price=sum( vill_price_national,
                                             dis_price_national,
                                             na.rm=TRUE )/2 )

region_price$region_price[region_price$tot_obs < 5] <- NA
region_price <- left_join( region_price, national_price )
bad <- is.na( region_price$region_price )
region_price$region_price[bad] <- region_price$national_price[bad]

# write.csv( region_price,"./prices_clean_w2.csv", row.names = FALSE )
