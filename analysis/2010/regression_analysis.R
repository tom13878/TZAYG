# second analysis file looking at some regressions using only the year 2 data
# initially to get an idea of what variables are significant. This is largely a
# repeat of Michiel's earlier analysis for the poster. This also follows Sheahan
# (2011) in terms of what variables have been selected.

# results of model do seem very sensitive to outliers

setwd('M:/cleaned_data')

library(plyr)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')

# read in data
db0 <- read.csv("./2010/database_2010.csv")

#' change variables in shillings using tanzania CPI. Source: world bank. base
#' year is 2010. create an area variable which uses the farmers estimate when
#' there is no GPS measurement available.
#' 
#' Sheahan specifies three criteria for selecting maize plots
#' 1. have maize and no more than six crops
#' 2. maize is not produced alongside a major cash crop
#' 3. maize constitudes at least 25% of the calculated revenues from the field.
#' Apply these three criteria to the current database. squared term  on nitrogen
#' is still not significant however

db3 <- filter(db2, !is.na(output.kgh.new) & crop.count <= 7 & !(cash.crop) & maize.share >= 25 & !(output.kgh.new > 6000))
x0 <- c('area', 'nitrogen.kg', 'output.kg.new', 'output.kg.old', 'org.fert.kg',
        'pest.kg', 'fam.lab.days', 'hir.lab.days', 'tot.cap.sh', 'potassium.kg',
        'phosphorous.kg')  
db1 <- mutate(db0, own.sh = ifelse(year == 2008, own.sh*1.061571, own.sh),
              rent.sh = ifelse(year == 2008, rent.sh*1.061571, rent.sh),
              area = ifelse(is.na(area.gps), area.est, area.gps),
              tot.cap.sh = own.sh + rent.sh) %>%
        ddply(.(y2_hhid), transform, total.area = sum(area)) %>%
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

# plot all of the key variables for the analysis
plotpattern <- function(df, col){
                qplot(aes_string(col), data = df)                 
}
qplot(aes_string(x), data = df)

# seed type has a value of 'non purchased'in this case it is assumed that the 
# cultivar is traditional

#' remove values for areas that are too small or too big, There are large
#' (impossible) outliers in both variables. One
#' option is to winsor these values. An alternative is just to throw them away.
#' here I choose to just throw them away (discuss with Michiel and Jeff)
#' variables are mutated into per hectacre form. Note that the trim function
#' is designed to remove the 1st and 99th percentiles
#' 
#' Need to look again at how you are cleaning this data. for example there are
#' still a lot of very large values which you are not controlling for here!

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
# x <- c('Mbeya', 'Arusha', 'Iringa', 'Manyara', 'Kilimanjaro', 'Rukwa', 'Ruvuma')
# db2$maize.belt <- ifelse(db2$region %in% x, TRUE, FALSE)
db2$area2 <- db2$area*db2$area

#' OLS regressions quadratic preferred model: still need to add a variable for
#' schooling and find out why family and hired labour days are colinear
#' Also need to add in dummy variables for regions - look at what Michiel did in
#' his analysis.
#' 
#' Sheahan specifies three criteria for selecting maize plots
#' 1. have maize and no more than six crops
#' 2. maize is not produced alongside a major cash crop
#' 3. maize constitudes at least 25% of the calculated revenues from the field.
#' Apply these three criteria to the current database. squared term  on nitrogen
#' is still not significant however
db3 <- filter(db2, !is.na(output.kgh.new) & crop.count <= 7 & !(cash.crop) & maize.share >= 25 & !(output.kgh.new > 6000)) %>%
        select(output.kgh.new, nitrogen.kgh, region, soilq, area, area2, soh, aoh, fam.lab.daysh, tot.cap.shh, phosphorous.kgh,
               irrigation, pest, beans, inter.crop, seed.type, hir.lab.daysh)
# also add in a zone variable
db3$zone[db3$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
db3$zone[db3$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
db3$zone[db3$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
db3$zone[db3$region %in% c("Singida","Dodoma")] <- "Central"
db3$zone[db3$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
db3$zone[db3$region %in% c("Pwani","Morogoro", "Dar es Salaam")] <- "Eastern"
db3$zone[db3$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
db3$zone[db3$region %in% c("Kaskazini Unguja", "Kusini Unguja", "Mjini Magharibi",
                                     "Kaskazini Pemba", "Kusini Pemba")] <- "Zanzibar"

# quadratic form
olsQ <- lm(output.kgh.new ~ nitrogen.kgh + I(nitrogen.kgh * nitrogen.kgh) + area +
                    aoh + log(tot.cap.shh) + fam.lab.daysh + soh + soilq +
                   irrigation + pest + beans + inter.crop + seed.type + maize.share,
           data = db2)

# test regression to find out why hired and family labour days are so similar.
# not sure what to make of this!
ols_test <- lm(hir.lab.days ~ fam.lab.days, data = db2)
plot(ols_test$resid)
abline(ols_test)

# preffered specification for the cobb Douglas model
# could also use a dummy variable for fertilizer rather than doing log(0 + 1)
db3$nitrogen2 <- ifelse(db3$nitrogen.kgh == 0, db3$nitrogen.kgh + 1, db3$nitrogen.kgh)
db3$fam.lab.daysh2 <- ifelse(db3$fam.lab.daysh == 0, db3$fam.lab.daysh + 1, db3$fam.lab.daysh)

# dummy coefficients in a log-log linear model are percentage changes when multipled by 100
olsCD <- lm(log(output.kgh.new) ~ log(nitrogen2) + log(tot.cap.shh) + log(fam.lab.daysh2) +
                   soh + soilq + irrigation + pest + beans + inter.crop + seed.type,
           data = db3)

# preffered specification of the translog model: what interactions to include.
# only include interactions that makes some economic/agronomic sense. 
olsTL <- lm(log(output.kgh.new) ~ log(nitrogen2) + log(tot.cap.shh) + log(fam.lab.daysh2) +
                    I(0.5 * log(nitrogen2)^2) + I(0.5 * log(tot.cap.shh)^2) + 
                    I(0.5 * log(fam.lab.daysh2)^2) + I(log(nitrogen2) * log(tot.cap.shh)) +
                    I(log(nitrogen2) * log(fam.lab.daysh2)) + I(log(tot.cap.shh) * log(fam.lab.daysh2)) +
                    soilq + irrigation + seed.type,
            data = db2)


# use the relevel command and set the ref argument to the reference level to make
# interpreting regressions easier.
plot(ols9$resid)
# make a scatter plot of the predictced values against the residuals. Can you
# predict the error? 
yhat <- predict(ols9)
plot(yhat, ols9$resid, main = 'residual values plotted againt predicted values', col = 'blue',
     xlab = 'predicted', ylab = 'residual')
# think about interaction terms by differentiating the linear equation. So if you
# think that the partial effect of nitrogen should include phosphourous then include
# a (multiplicative) interaction term. For example, an increase in nitrogen results
# in a higher maize yield for plots which already have a high level of phosphorous
# or high soil quality. To summarize these interaction effects, evaluate the equation
# at interesting values of the interaction term (phosphorous), such as the mean or
# lower/upper quartiles of the phosphorous distribution
ols_test2 <- lm(log(output.kgh.new) ~ log(nitrogen2), data = db2)
yhat2 <- predict(ols_test2)
plot(yhat2, ols_test2$resid, main = 'residual values plotted againt predicted values', col = 'blue',
     xlab = 'predicted', ylab = 'residual')
