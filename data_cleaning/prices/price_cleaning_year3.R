# -------------------------------------
#' consumer price winsoring for year 3
#' wave of Tanzania data
#' 
#' First some basic analysis of the prices
#' before and after winsoring. Followed
#' by the actual preparation of the prices
#' 
# -------------------------------------
setwd('M:/cleaned_data')
library(ggplot2)
library(plyr)
library(dplyr)
source("M:/TZAYG/functions/winsor.R")

# read in price data prepared earlier in 
# TZA12_prep.R file and stored  in
# M:/cleaned_data/2012/prices.csv"
prices <- read.csv( "M:/cleaned_data/2012/prices.csv" )

# -----------BASIC ANALYSIS -----------

# plotting functions
plotpattern <- function( df, col ){
        ggplot( df, aes_string( col ) ) + geom_histogram( fill='red', colour='black', binwidth=50 ) +
                labs( title=paste( as.character( df$item_name[1] ), 'price plot', sep=' ' ) ) +
                theme_bw(  )
}

quantile_plot <- function( x, y ){
        qqplot( x, y, col = 'red', xlab = 'Theoretical Quantiles',
               ylab = 'Sample Quantiles', main = 'qqplot' )
        
        abline( a = 0, b = 1, col = 4 )      
}

dens_plot <- function( x, breaks = 25, freq = FALSE ){
        hist( x, breaks = breaks, col = 'lightgreen', freq = freq )
        curve( dnorm( x, mean=mean( x ), sd=sd( x ) ), add=TRUE, col="darkblue", lwd=2 )
}

# use plotpattern function to search for outliers in the data in both the village
# and district level prices. There are lots of outliers in pretty much all of the
# food stuffs at both the village and the district level.
d_ply(prices, .( item_name ), function( elt ) plotpattern( elt, 'vill_price' ), .print=TRUE )
d_ply(prices, .( item_name ), function( elt ) plotpattern( elt, 'dis_price' ), .print=TRUE )

# check out some basic stats of the local and district level prices
range( prices$vill_price, na.rm=TRUE )
summary( prices$vill_price )

range( prices$dis_price, na.rm=TRUE )
summary( prices$dis_price )

# make a quantile plot to see how the distribution of maize prices compares
# to the normal distribution. Need to standardise the maize prices first.
x <- rnorm( 289, mean=0, sd=1 )
m <- filter( prices, item_name=='Maize (grain)' ) %>%
        mutate( dis_price=( dis_price - mean(dis_price, na.rm=TRUE ) )/sd( dis_price, na.rm=TRUE ),
               vill_price=( vill_price - mean( vill_price, na.rm=TRUE ) )/sd( vill_price, na.rm=TRUE ) )

quantile_plot( x, m$vill_price ) # ouch
quantile_plot( x, m$dis_price )  # double ouch

# winsor both price variables and check to see what they look like afterwards
# winsoring like this will skew the distribution to the right. Might want to 
# ammend winsoring functions to overcome this!
prices_winsor <- ddply( prices, .( item_name ), function( elt ) winsor3( elt, c( 'vill_price', 'dis_price' ), multiple=3 ) )

# may also need to watch the maize prices, seems to be a lage number of 
# observations that end up at the edge of the distribution suggesting that a 
# better approach is needed.
d_ply( prices_winsor, .( item_name ), function( elt ) plotpattern( elt, 'vill_price' ), .print=TRUE )
d_ply( prices_winsor, .( item_name ), function( elt ) plotpattern( elt, 'dis_price' ), .print=TRUE )

# check the winsored prices with a quantile plot 
m2 <- subset( prices_winsor, item_name=='Maize (grain)' ) %>% 
        mutate( vill_price=( vill_price - mean( vill_price, na.rm = TRUE ) )/sd( vill_price, na.rm = TRUE ),
               dis_price=( dis_price - mean( dis_price, na.rm = TRUE ) )/sd( dis_price, na.rm = TRUE ) )

quantile_plot( x, m2$vill_price ) # much better
quantile_plot( x, m2$dis_price )  # Also much better

dens_plot( m2$vill_price, breaks = 20 ) # not bad
dens_plot( m2$dis_price, breaks = 20 )  # Also not bad


# ----------- FINAL PRICES ------------
# For later analysis we need prices. Here two sets of prices are created from
# the village and district prices by taking the average of each across regions
# AND items. For example we might take the average of all Maize sold in village
# markets in Dodoma. We also want to know the average ove the averages across
# village and district level markets and the number of observations we have
# to determine whether these prices are reliable or not

region_price <- ddply( prices_winsor, .( region, item_name ), summarize,
                       vill_price_region=mean( vill_price, na.rm = TRUE ),
                       dis_price_region=mean( dis_price, na.rm = TRUE ),
                       vill_obs_region=sum( is.na( vill_price ) ),
                       dis_obs_region=sum( is.na( dis_price ) ),
                       region_price=sum( vill_price_region,
                                          dis_price_region, na.rm=TRUE )/2,
                       tot_obs=sum( vill_obs_region, dis_obs_region, na.rm=TRUE ) )

#' Analogous, to the regional prices, we may need national prices where the
#' regional observations are too few to be reliable. Here we calculate national
#' averages of the village and district prices and also the average of the 
#' village and district averages. No need for observations because ultimately
#' the national prices will replace any regional prices which are not reliable
#' and will always have at least as many observations as the regional price

national_price <- ddply( prices, .( item_name ), summarize,
                         vill_price_national=mean( vill_price, na.rm=TRUE ),
                         dis_price_national=mean( dis_price, na.rm=TRUE ),
                         national_price=sum( vill_price_national,
                                              dis_price_national,
                                                  na.rm=TRUE )/2 )

# set any regional prices that have less than five observations to NA. When 
# a regional price is NA, it is then replaced with the correpsonding national
# price.

region_price$region_price[region_price$tot_obs < 5] <- NA
region_price <- left_join( region_price, national_price )
bad <- is.na( region_price$region_price )
region_price$region_price[bad] <- region_price$national_price[bad]

# finally take a quick look at all of the prices
d_ply( region_price, .( item_name ), function( elt ) plotpattern( elt, 'region_price' ), .print = TRUE )

# write.csv( region_price,"M:/cleaned_data/2012/prices_clean_y3.csv", row.names = FALSE )

