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


setwd('M:/cleaned_data')
library(ggplot2)
library(plyr)
library(dplyr)
source("M:/TZAYG/winsor.R")

# plotting functions
plotpattern <- function(df, col){
       ggplot(df, aes_string(col)) + geom_histogram(fill = 'red', colour = 'black', binwidth = 20) +
                labs(title = paste(as.character(df$itemname[1]), 'price plot', sep = ' ')) +
                theme_bw()
}

quantile_plot <- function(x, y){
        qqplot(x, y, col = 'red', xlab = 'Theoretical Quantiles',
               ylab = 'Sample Quantiles', main = 'qqplot')
        
        abline(a = 0, b = 1, col = 4)      
}

dens_plot <- function(x, breaks = 25, freq = FALSE){
        hist(x, breaks = breaks, col = 'lightgreen', freq = freq)
        curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE, col="darkblue", lwd=2)
}

# price data from 2010 preparation file
prices <- read.csv("./2010/prices.csv")

# problem: there are large outliers in the price data which need to be removed
# before analysis. Assume that maize prices are normally distributed across
# tanzania. QQplot shows problem with outliers
# a flick through the plots below shows the problem with some of the data. It is
# the high cost values that seem to have the highest outliers.
d_ply(prices, .(itemname), function(elt) plotpattern(elt, 'price.loc'), .print = TRUE)

range(prices$price.loc, na.rm = TRUE)
summary(prices$price.loc)

range(prices$price.dis, na.rm = TRUE)
summary(prices$price.dis)

x <- rnorm(231, mean = 0, sd = 1)
m <- subset(prices, itemname == 'Maize (grain)') %>%
        mutate(price.dis = (price.dis - mean(price.dis, na.rm = TRUE))/sd(price.dis, na.rm = TRUE),
               price.loc = (price.loc - mean(price.loc, na.rm = TRUE))/sd(price.loc, na.rm = TRUE))

quantile_plot(x, m$price.dis)
quantile_plot(x, m$price.loc)

# winsoring prices to get rid of outliers using median average deviation 
# winsoring function
# see http://www.r-bloggers.com/winsorization/ for details
prices.winsor <- ddply(prices, .(itemname), function(elt) winsor3(elt, c('price.loc', 'price.dis'), multiple = 3))

d_ply(prices.winsor, .(itemname), function(elt) plotpattern(elt, 'price.loc'), .print = TRUE)
d_ply(prices.winsor, .(itemname), function(elt) plotpattern(elt, 'price.dis'), .print = TRUE)

# closer look at Maize prices only
m2 <- subset(prices.winsor, itemname == 'Maize (grain)') %>% 
        mutate(price.loc = (price.loc - mean(price.loc, na.rm = TRUE))/sd(price.loc, na.rm = TRUE),
               price.dis = (price.dis - mean(price.dis, na.rm = TRUE))/sd(price.dis, na.rm = TRUE))

quantile_plot(x, m2$price.loc)
quantile_plot(x, m2$price.dis)

dens_plot(m2$price.loc, breaks = 20)
dens_plot(m2$price.dis, breaks = 20)

# create price variables and make a final price index
prices.region <- ddply(prices.winsor, .(region, itemname), summarize,
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

d_ply(prices.region, .(itemname), function(elt) plotpattern(elt, 'region.price.tot'), .print = TRUE)

write.csv(prices.region,"./2010/prices_winsor_y2.csv", row.names = FALSE)
