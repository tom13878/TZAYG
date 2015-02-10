# regression preparation file: select variables of interest for the regression
# analysis and make some transformations 

setwd('M:/cleaned_data')

library(plyr)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')

# read in data
db0 <- read.csv("./2010/database_2010.csv")

# sheahan identidies three conditions that a plot must meet to be included in 
# the regression
#' 1. have maize and no more than six other crops
#' 2. maize is not produced alongside a major cash crop
#' 3. maize constitudes at least 25% of the calculated revenues from the field.
db1 <- filter(db0, !(is.na(output.kg.new)) & crop.count <= 7 & !(cash.crop) & maize.share >= 25)

#' put variables into per hectacre terms before checking for outliers.
db2 <- mutate(db1, tot.cap.sh = own.sh + rent.sh,
              output.kgh.new = output.kg.new/area.gps,
              output.kgh.old = output.kg.old/area.gps,
              nitrogen.kgh = nitrogen.kg/area.gps,
              org.fert.kgh = org.fert.kg/area.gps,
              fam.lab.daysh = fam.lab.days/area.gps,
              hir.lab.daysh = fam.lab.days/area.gps,
              tot.cap.shh = tot.cap.sh/area.gps,
              potassium.kgh = potassium.kg/area.gps,
              phosphorous.kgh = phosphorous.kg/area.gps)

hist(db2$output.kgh.new)
hist(db2$nitrogen.kgh)
hist(db2$tot.cap.shh)
hist(db2$fam.lab.daysh)
hist(db2$hir.lab.daysh)

# for some strange the hired labour and the family labour have the same mean and
# standard deviation. Need to go back and double check the code that created these
# variables

# follow Liverpool-Tasie, Barrett and Sheahan and winsor values at 99% level. if
# they are still very large then try 95% level. If that doesn't give anything
# reasonable for fertilizer inputs replace fertilizer over 1 ton with 700kg to
# try and limit the impact that outliers are having on the rgression.

# winsor the input and output variables that will be used in the regression: 
# nitrogen, labour and capital. 
x <- c('output.kgh.new', 'nitrogen.kgh', 'tot.cap.shh', 'fam.lab.daysh', 'hir.lab.daysh')
db3 <- winsor4(db2, x)
stargazer(select(db3, output.kgh.new, nitrogen.kgh, tot.cap.shh, fam.lab.daysh, hir.lab.daysh))
# the variable that still seems bery large is capital but take the log in the regression
# test OLS: quadratic term is not significant. think of the circumstances under which it
# would be decreasing. This will depend on the region and zone, on the soilq etc.
ols_test <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh^2) + log(tot.cap.shh) + area.gps, data = db3)
