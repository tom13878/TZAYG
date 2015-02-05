# second analysis file 

setwd('M:/cleaned_data')

library(plyr)
library(DescTools)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')

# read in data
db0 <- read.csv('M:/cleaned_data/database_final.csv')

#' change variables in shillings using tanzania CPI. Source: world bank. base
#' year is 2010. create an area variable which uses the farmers estimate when
#' there is no GPS measurement available.  
db1 <- mutate(db0, own.sh = ifelse(year == 2008, own.sh*1.061571, own.sh),
              rent.sh = ifelse(year == 2008, rent.sh*1.061571, rent.sh),
              area = ifelse(is.na(area.gps), area.est, area.gps),
              tot.lab.days = fam.lab.days + hir.lab.days,
              tot.cap.sh = own.sh + rent.sh) %>%
        ddply(.(hhid), transform, total.area = sum(area))

# seed type has a value of 'non purchased'in this case it is assumed that the 
# cultivar is traditional
db1$seed.type <- revalue(db1$seed.type, c('None purchased' = 'TRADITIONAL'))
db1$nitrogen.kg[db1$nitrogen.kg == 0] <- NA
db1$area[db1$area == 0] <- NA
db1$output.kg.new[db1$output.kg.new == 0] <- NA
db1$pest.kg[db1$pest.kg == 0] <- NA
db1$org.fert.kg[db1$org.fert.kg == 0] <- NA
db1$tot.lab.days[db1$tot.lab.days == 0] <- NA
db1$tot.cap.sh[db1$tot.cap.sh == 0] <- NA

#' remove values for areas that are too small or too big, There are large
#' (impossible) outliers in both variables. One
#' option is to winsor these values. An alternative is just to throw them away.
#' here I choose to just throw them away (discuss with Michiel and Jeff)
#' variables are mutated into per hectacre form. Note that the trim function
#' is designed to remove the 1st and 99th percentiles
x0 <- c('area', 'nitrogen.kg', 'output.kg.new', 'output.kg.old', 'org.fert.kg',
        'pest.kg', 'tot.lab.days', 'tot.cap.sh', 'potassium.kg', 'phosphorous.kg')
db2 <- trim2(db1, x0) %>%
        mutate(output.kgh.new = output.kg.new/area,
               output.kgh.old = output.kg.old/area,
               nitrogen.kgh = nitrogen.kg/area,
               pest.kgh = pest.kg/area,
               org.fert.kgh = org.fert.kg/area,
               tot.lab.daysh = tot.lab.days/area,
               tot.cap.shh = tot.cap.sh/area,
               potassium.kgh = potassium.kg/area,
               phosphorous.kgh = phosphorous.kg/area)

# select only the year 2 plots to have a look at and get some results from
# regressions
x <- c('Mbeya', 'Arusha', 'Iringa', 'Manyara', 'Kilimanjaro', 'Rukwa', 'Ruvuma')
db2$maize.belt <- ifelse(db2$region %in% x, TRUE, FALSE)

# base OLS regression with only nitrogen area and a years dummy
ols0 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) + area +
                   year + maize.belt + soh + aoh + log(tot.cap.shh) +
                   tot.lab.daysh + irrigation + beans + seed.type + slope +
                   pest.kg, data = db2)


stargazer(ols0, type = 'text')

