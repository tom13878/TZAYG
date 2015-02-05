# second analysis file looking at some regressions using only the year 2 data
# initially to get an idea of what variables are significant. This is largely a
# repeat of Michiel's earlier analysis for the poster. This also follows Sheahan
# (2011) in terms of what variables have been selected.

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

# select only the year 2 plots to have a look at and get some results from
# regressions
x <- c('Mbeya', 'Arusha', 'Iringa', 'Manyara', 'Kilimanjaro', 'Rukwa', 'Ruvuma')
db2$maize.belt <- ifelse(db2$region %in% x, TRUE, FALSE)
db2$area2 <- db2$area*db2$area

#' OLS regressions using quadratic form. Significant variables are nitrogen, 
#' pesticide use, the log of total capital,
# base specification including only the continous variables 
ols0 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) + area +
                   area2 + log(tot.cap.shh) + hir.lab.daysh + fam.lab.daysh + 
                   log(total.area),
           data = db2)
# dummy variables added for doil quality, irrigation and seed type
ols1 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) + soilq +
                   irrigation + seed.type + area + area2 + log(tot.cap.shh) +
                   hir.lab.daysh + fam.lab.daysh + log(total.area),
           data = db2)
# more dummy variables added for farmer characteristics such as sex and age
ols2 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) + soilq +
                   irrigation + seed.type + area + soh + aoh +
                   area2 + log(tot.cap.shh) + hir.lab.daysh + fam.lab.daysh + log(total.area),
           data = db2)
# dummy variable added for pesticide use (1/0) and legumes and intercropping
ols3 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) + soilq +
                   irrigation + seed.type + area + soh + aoh + pest + beans + inter.crop +
                   area2 + log(tot.cap.shh) + hir.lab.daysh + fam.lab.daysh + log(total.area),
           data = db2)
# interaction terms added to see how nitrogen and phosphorous interact
ols4 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) + soilq +
                   I(nitrogen.kgh * phosphorous.kgh) + phosphorous.kgh +
                   irrigation + seed.type + area + soh + aoh + pest + beans + inter.crop +
                   area2 + log(tot.cap.shh) + hir.lab.daysh + fam.lab.daysh + log(total.area),
           data = db2)

stargazer(ols0, ols2, ols3, ols4, type = 'text')

ols7 <- lm(output.kgh.new ~ nitrogen.kgh*soilq + I(nitrogen.kgh * phosphorous.kgh)*soilq, data = db2)
stargazer(ols0, ols3, ols5, type = 'text')

#' OLS regressions quadratic form take 2.

ols0.1 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) +
                     log(tot.cap.shh) + hir.lab.daysh + pest + inter.crop, 
             data = db2)
ols0.2 <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh*nitrogen.kgh) +
                     log(tot.cap.shh) + hir.lab.daysh + pest + inter.crop +
                     area + irrigation + soilq + seed.type, 
             data = db2)

                     

