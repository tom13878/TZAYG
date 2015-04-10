# -------------------------------------
#' predictive mean matching to get over 
#' The problem of missing area values in
#' R
#' -----------------------------------

# install.packages('mice')
# install.packages('VIM')

library(mice)
library(VIM)
library(lattice)
library(ggplot2)

# use the comp dataset from before
comp <- read.csv("M:/comp.csv")
row.names(comp) <- comp$X
#kill of hhid and plotnum
comp$y3_hhid <- comp$plotnum <- comp$X<- NULL

# this function gives an overview of the missingness in the data
# read as there are 5396 where we have complete values
# 2051 where we are missing just area.gps and so on.
md.pattern(comp)

# this function returns a list with counts of how many observations
# are available when pairs of variables are: there, not there, one
# or another of them is available.
md.pairs(comp)

# this plot helps us to identify the missingness in the data.
# blue dots are for values that we have both variables
# red dots are where we have missing values for both variables
# in the case below the red dots are represent values that are
# missing for area.gps but are available for area.est (since 
# all the missing area.est ones have already been removed)
marginplot(comp[, 1:2], col=c('blue', 'red', 'orange'))


# MICE imputation method
imp1 <- mice(comp, m=5)

# calling head reveals some of the values in the dataset
# for each of the five imputations. Clearly need to clean
# up some of the values here. 
head(imp1$imp$area.gps) 

# use the complete function to combine the data with the
# original data. this returns six 'separate' dataframes
# where the missing values in the last five have been 
# replaced with the values of the correspodning imputation
head(complete(imp1, 1)$area.gps)
head(complete(imp1, 2)$area.gps)
head(complete(imp1, 3)$area.gps)
imp1_tot <- complete(imp1, "long", include=TRUE)

# extract values, average them, then include them where
# values are missing in real dataset?
test <- (complete(imp1, 1)$area.gps+complete(imp1, 2)$area.gps+complete(imp1, 3)$area.gps+complete(imp1, 4)$area.gps+complete(imp1, 5)$area.gps)/5

# where there are missing values in the comp data replace with imputed data
comp2 <- select(comp, area.gps, area.est)
comp2$area.gps <- ifelse(is.na(comp$area.gps), test, comp$area.gps)
comp2$dev <- comp2$area.gps - comp2$area.est
dev2 <- comp$area.gps - comp2$area.est
plot(comp2$dev, col='red')
points(dev2, col='blue')

area_impute_gps <- select(comp2, area.gps)

# split on the M in plot number and then
# add it back in later
split_on_M <-strsplit(row.names(area_impute_gps), "M")
y3_hhid <- sapply(split_on_M, function(elt) return(elt[1]))
plotnum <- sapply(split_on_M, function(elt) return(paste('M', elt[2], sep="")))

# whitespace snuck in somehow!!!
plotnum <- gsub(" ", "", plotnum)

# combine everything together
area_impute_gps$y3_hhid <- y3_hhid
area_impute_gps$plotnum <- plotnum

# now we have GPS measurements everywhere that we have estimated measurements
# and these can be combined with the rest of the data

# plotting the points indicates that it is the deviations between the gps
# measures and the self reported in the actual data that are biggest
# in other words the imputation is better behaved.

# --------------NOTES------------------
#' watch out for the row names, for some
#' reason these are not working properly
#' Not every plot has a plot number?????
#' 
#' Also make sure that you went through 
#' the process of creating the comp data 
#' properly, as in did you winsor at the 
#' right place etc.
#' 
#' Remember to turn everything into
#' hectacres at some point!
# -------------------------------------
