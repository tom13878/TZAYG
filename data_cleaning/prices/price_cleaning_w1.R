#' Winsoring of 2008 price data to get rid of outliers. Two methods are 
#' considered: Winsoring at the 95th percentile and winsoring depeding on the
#' deviation from the median. histograms and qqplots indicate that winsoring
#' according to the second method is preferable, this also has a more economic 
#' interpretation
#' 
#' regional prices for and item where there are less than 5 observations are
#' replaced with the national average for that item. 
#' 
#' The oputput of this script is a set of prices for 2008 which will be used to
#' construct the Liu Meyers output index (Sheahan 2011)


setwd('M:/cleaned_data')
library(ggplot2)
library(plyr)
library(dplyr)

# winsor functions - available on github: https://github.com/tom13878/TZAYG
source("M:/TZAYG/winsor.R")

prices <- read.csv("./2008/prices.csv")

# problem: there are large outliers in the price data which need to be removed
# before analysis. Assume that maize prices are normally distributed across
# tanzania. QQplot shows problem with outliers
d_ply(prices, .(itemname), failwith(NA, plotpattern), .print = TRUE)

range(prices$price.unit)
summary(prices$price.unit)
hist(prices$price.unit)

x <- rnorm(246, mean = 0, sd = 1)
m <- subset(prices, itemname == 'Maize (grain)') %>%
        mutate(price.unit = (price.unit - mean(price.unit))/sd(price.unit))
qqplot(x, m$price.unit, col = 'red', xlab = 'Theoretical Quantiles',
       ylab = 'Sample Quantiles', main = 'qqplot')

# plotting functions
plotpattern <- function(df){
        ggplot(df, aes(x = price.unit)) +
                geom_histogram(fill = 'white', colour = 'black', binwidth = 20) +
                labs(title = paste(as.character(df$itemname[1]), 'price plot', sep = ' '))
}

quantile_plot <- function(x, y){
        qqplot(x, y, col = 'red', xlab = 'Theoretical Quantiles',
               ylab = 'Sample Quantiles', main = 'qqplot winsored')
        
        abline(a = 0, b = 1, col = 4)      
}

dens_plot <- function(x, breaks = 25, freq = FALSE){
        hist(x, breaks = breaks, col = 'lightgreen', freq = freq)
        curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE, col="darkblue", lwd=2)
}
# winsoring prices to get rid of outliers using two types of winsoring function
# see http://www.r-bloggers.com/winsorization/ for details
prices.winsor1 <- ddply(prices, .(itemname), function(elt) winsor1(elt, "price.unit", fraction = 0.05))
prices.winsor2 <- ddply(prices, .(itemname), function(elt) winsor2(elt, "price.unit", multiple = 3))

d_ply(prices.winsor1, .(itemname), failwith(NA, plotpattern), .print = TRUE)
d_ply(prices.winsor2, .(itemname), failwith(NA, plotpattern), .print = TRUE)

m2 <- subset(prices.winsor1, itemname == 'Maize (grain)') %>% 
        mutate(price.unit = (price.unit - mean(price.unit))/sd(price.unit))
m3 <- subset(prices.winsor2, itemname == 'Maize (grain)') %>% 
        mutate(price.unit = (price.unit - mean(price.unit))/sd(price.unit))

quantile_plot(x, m2$price.unit)
quantile_plot(x, m3$price.unit)

dens_plot(m2$price.unit, breaks = 20)
dens_plot(m3$price.unit, breaks = 20)

#' choose the second winsoring method as it is closer to a theoretical
#' normal distribution and has a better fit line than the first method. Also has
#' a more appealing economic interpretation and a nicer histogram
#' 
#' create a price series for use in the Liu meyeres index

prices.region <- ddply(prices.winsor2, .(region, itemname), summarize,
                       region.price = mean(price.unit, na.rm = TRUE),
                       num.obs = length(price.unit))

prices.national <- ddply(prices.winsor2, .(itemname), summarize,
                         national.price = mean(price.unit, na.rm = TRUE))

prices.region$region.price[prices.region$num.obs < 5] <- NA
prices.region <- left_join(prices.region, prices.national) 
bad <- is.na(prices.region$region.price)
prices.region$region.price[bad] <- prices.region$national.price[bad]
prices.region <- select(prices.region, region, everything(), -national.price)

write.csv(prices.region,"./2008/prices_winsor_y1.csv", row.names = FALSE )


