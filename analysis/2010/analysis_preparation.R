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
#' In another Sheahan and barrett paper 50% is used instead of 25%

db1 <- filter(db0, crop.count <= 7 & !(cash.crop) & maize.share >= 25)

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

db2 <- filter(db2, output.kgh.new <=10000)


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
x <- c('output.kgh.new', 'nitrogen.kgh' , 'tot.cap.shh', 'fam.lab.daysh', 'hir.lab.daysh')
db3 <- trim2(db2, x) %>% select(y2_hhid, plotnum, region, yield = output.kgh.new, nit = nitrogen.kgh, cap = tot.cap.shh,
                                  famlab = fam.lab.daysh, hired = hir.lab.daysh, soh, soilq,
                                  irrig = irrigation, pest, beans, inter.crop, seed.type,
                                  area = area.gps, aoh, slope, maize.share)
db3$y2_hhid <- as.character(db3$y2_hhid)


# make some more variables that will be important later on including zone
db3$zone[db3$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
db3$zone[db3$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
db3$zone[db3$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
db3$zone[db3$region %in% c("Singida","Dodoma")] <- "Central"
db3$zone[db3$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
db3$zone[db3$region %in% c("Pwani","Morogoro", "Dar es Salaam")] <- "Eastern"
db3$zone[db3$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
db3$zone[db3$region %in% c("Kaskazini Unguja", "Kusini Unguja", "Mjini Magharibi",
                           "Kaskazini Pemba", "Kusini Pemba")] <- "Zanzibar"

# For every variable that takes on zero values add a new variable for log specifications
db3 <- mutate(db3, nit2 = ifelse(nit == 0, nit + 1, nit),
              yield2 = ifelse(yield == 0, yield + 1, yield),
              cap2 = ifelse(cap == 0, cap + 1, cap),
              famlab2 = ifelse(famlab == 0, famlab + 1, famlab),
              nitsqrt = nit*nit)

x <- c('Mbeya', 'Arusha', 'Iringa', 'Manyara', 'Kilimanjaro', 'Rukwa', 'Ruvuma')
db3$maize.belt <- ifelse(db3$region %in% x, TRUE, FALSE)

# test a quadratic model:

olsQ <- lm(yield ~ nit:zone + I(nit*nit) + cap + famlab + area + I(area^2) + aoh +
                   soh + pest + irrig + beans + inter.crop + seed.type + soilq + slope,
           data = db3)

# try subsetting on the souther highlands and running a quadratic regression again
db3SH <- subset(db3, zone == 'Southern Highlands')

olsQSH <- lm(yield ~ nit + I(nit^2) + log(cap) + famlab + area + I(area^2) + aoh +
                   soh + pest + irrig + beans + inter.crop + seed.type + soilq + slope,
           data = db3SH)

# Sheahan has a lot of interaction variables. Introduce interactions with zones
olsQ <- lm(yield ~ nit:maize.belt + I(nit*nit):maize.belt + cap + famlab + area +  + aoh +
                   soh + pest + irrig + beans + inter.crop + seed.type  + slope,
           data = db3)

MB <- subset(db3, maize.belt)
log <- !(complete.cases(MB))
ols_test <- lm(yield ~ nit + I(nit*nit) + cap + famlab +maize.share, data = MB)


# try a cobb douglass specification instead
olsCD <- lm(log(yield2) ~ log(nit2) + log(cap2) + log(famlab2) + aoh + soh + pest
            + irrig + beans + inter.crop + seed.type + soilq + slope, data = db3)
            


# what kind of controls are needed to remove southern highlands advantages?