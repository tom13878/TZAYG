""" second analysis file looking at some regressions using only the year 2 data
initially to get an idea of what variables are significant. This is largely a
repeat of Michiel's earlier analysis for the poster. This also follows Sheahan
(2011) in terms of what variables have been selected."""

setwd('M:/cleaned_data')

library(plyr)
library(DescTools)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')

# read in data
db0 <- read.csv("./2010/database_2010.csv")

#' change variables in shillings using tanzania CPI. Source: world bank. base
#' year is 2010. create an area variable which uses the farmers estimate when
#' there is no GPS measurement available.  
db1 <- mutate(db0, own.sh = ifelse(year == 2008, own.sh*1.061571, own.sh),
              rent.sh = ifelse(year == 2008, rent.sh*1.061571, rent.sh),
              area = ifelse(is.na(area.gps), area.est, area.gps),
              tot.cap.sh = own.sh + rent.sh) %>%
        ddply(.(y2_hhid), transform, total.area = sum(area))

# seed type has a value of 'non purchased'in this case it is assumed that the 
# cultivar is traditional
db1$seed.type <- revalue(db1$seed.type, c('None purchased' = 'TRADITIONAL'))

db1$area[db1$area == 0] <- NA
db1$output.kg.new[db1$output.kg.new == 0] <- NA
db1$pest.kg[db1$pest.kg == 0] <- NA
db1$org.fert.kg[db1$org.fert.kg == 0] <- NA
db1$tot.cap.sh[db1$tot.cap.sh == 0] <- NA

#' remove values for areas that are too small or too big, There are large
#' (impossible) outliers in both variables. One
#' option is to winsor these values. An alternative is just to throw them away.
#' here I choose to just throw them away (discuss with Michiel and Jeff)
#' variables are mutated into per hectacre form. Note that the trim function
#' is designed to remove the 1st and 99th percentiles
x0 <- c('area', 'nitrogen.kg', 'output.kg.new', 'output.kg.old', 'org.fert.kg',
        'pest.kg', 'fam.lab.days', 'hir.lab.days', 'tot.cap.sh', 'potassium.kg',
        'phosphorous.kg')
db2 <- trim2(db1, x0) %>%
        mutate(output.kgh.new = output.kg.new/area,
               output.kgh.old = output.kg.old/area,
               nitrogen.kgh = nitrogen.kg/area,
               pest.kgh = pest.kg/area,
               org.fert.kgh = org.fert.kg/area,
               fam.lab.daysh = fam.lab.days/area,
               hir.lab.daysh = fam.lab.days/area,
               tot.cap.shh = tot.cap.sh/area,
               potassium.kgh = potassium.kg/area,
               phosphorous.kgh = phosphorous.kg/area)
