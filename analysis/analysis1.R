#' analysis of panel set constructed from tanzania household survey from 2008
#' and 2010 - no regressions here, just a good look at the data. For regressions
#' look at analysis2.R
setwd('M:/cleaned_data')

library(plyr)
library(DescTools)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')

# functions
plotpattern <- function(df, col1, col2){
        ggplot(df, aes(x = nitrogen.kgh, y = output.kgh.new)) +
                geom_point() + labs(title = paste0(as.character(df$region[1]))) +
                theme_bw()
}

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
# table of some key variables using plots 
stargazer(select(db1, area, nitrogen.kg, output.kg.new), type = 'text')

#' remove values for areas that are too small or too big, There are large
#' (impossible) outliers in both variables. One
#' option is to winsor these values. An alternative is just to throw them away.
#' here I choose to just throw them away (discuss with Michiel and Jeff)
#' variables are mutated into per hectacre form. Note that the trim function
#' is designed to remove the 1st and 99th percentiles
db2 <- trim2(db1, c('area', 'nitrogen.kg', 'output.kg.new', 'output.kg.old')) %>%
        mutate(output.kgh.new = output.kg.new/area,
               output.kgh.old = output.kg.old/area,
               nitrogen.kgh = nitrogen.kg/area,
               pest.kgh = pest.kg/area,
               org.fert.kgh = org.fert.kg/area,
               tot.lab.daysh = tot.lab.days/area,
               tot.cap.shh = tot.cap.sh/area)

#' frequency tables for some key fator variables and look for normality in 
#' nitrogen 
Freq(db2$soilq)
Freq(db2$seed.type)
Freq(db2$irrigation)
Desc(log(db2$nitrogen.kgh))

#' table of key variables after trimming has been performed and using per 
#' hectacre values. Definitely an improvement but still suspicious of some of 
#' the higher values!
stargazer(select(db2, area, nitrogen.kgh, output.kgh.new), type = 'text')        

# plots of key variables, look for relationships
# 1. plot of nitrogen against output. year 2 panel reveals a lot of farmers who
#    are using a small amount of nitrogen per hectacre. This is not present in
#    year 1
g0 <- ggplot(db2, aes(x = nitrogen.kgh, y = output.kgh.new)) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 2. plot of output against area to check if there are a lot of small plots
#    (where there would be a small amount of fertilizer used). Data seems to
#    suggest that smaller plots are more productive tahn big ones
g1 <- ggplot(db2, aes(x = area, y = output.kgh.new)) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 3. check whether overall 'farm size' measured in terms of total plot area
#    makes a difference. Similar conclusion - seems that the smaller the farm
#    the more productive it is.
g2 <- ggplot(db2, aes(x = total.area, y = output.kgh.new)) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 4. There is a large variation in area, nitrogen use per hectacre and output.
#    try a log scale to deal with this. Strong positive relationship exhibited
#    across years.
g3 <- ggplot(db2, aes(x = log(nitrogen.kgh), y = log(output.kgh.new))) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 5. look at histogram of output values split by year. 
g4 <- ggplot(db2, aes(x = output.kgh.new)) +
        geom_histogram(binwidth = 300, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year)

# 6. look for some relationships between other factors and output 
g5 <- ggplot(na.omit(select(db2, seed.type, output.kgh.old)), aes(x = factor(seed.type), output.kgh.old)) +
        geom_boxplot(outlier.shape = 21) 
g6 <- ggplot(na.omit(select(db2, irrigation, output.kgh.old)), aes(x = factor(irrigation), output.kgh.old)) +
        geom_boxplot(outlier.shape = 21) 
g7 <- ggplot(na.omit(select(db2, soilq, output.kgh.old)), aes(x = factor(soilq), output.kgh.old)) +
        geom_boxplot(outlier.shape = 21) 

# 7. add colour to plots based on some factors
g8 <- ggplot(db2, aes(x = log(nitrogen.kgh), y = log(output.kgh.new), colour = factor(soilq))) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 8. do farmers with low soil quality use less fertilizer? From the plot it
#    seems the case
g9 <- ggplot(na.omit(select(db2, nitrogen.kgh, soilq)), aes(x = factor(soilq), y = nitrogen.kgh)) +
                     geom_boxplot(outlier.shape = 21)

# 9. histogram of area distribution. The area is very mmuch skewed to the right 
#    best top use log for area. the plot g11 demonstrates the difference between
#    gps and estimated area size. 
g10 <- ggplot(db2, aes(x = area)) +
        geom_histogram(binwidth = 0.05, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year, scales = 'free')
g11 <- ggplot(db2, aes(x = log(area))) +
        geom_histogram(binwidth = 0.05, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year, scales = 'free')

# 10. histogram of nitrogen distribution. 
g12 <- ggplot(db2, aes(x = nitrogen.kgh)) +
        geom_histogram(binwidth = 5, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year, scales = 'free')

# 11. look again at relationship between nitrogen per hectacre and output per
#     hectacre facetted on region
g13 <- ggplot(db2, aes(x = log(nitrogen.kgh), y = log(output.kgh.new))) +
        geom_point() + facet_grid(. ~ region, scales = 'free')

# do some ANOVA tests using factors like seed.type and region and irrigation
# make tables of where maize in produced most, which regions?

#' tables for looking at how maize and nitrogen are split across regions. 
by_region_year <- group_by(db2, region, year)
avgs_region <- summarise(by_region_year,
                         avg_out_ha = mean(output.kgh.new, na.rm = TRUE),
                         avg_nit_ha = mean(nitrogen.kgh, na.rm = TRUE),
                         avg_lab_ha = mean(tot.lab.daysh, na.rm = TRUE),
                         avg_cap_ha = mean(tot.cap.shh, na.rm = TRUE),
                         avg_size = mean(total.area, na.rm = TRUE))

#'quick plots of some less crucial variables
qplot(log(tot.cap.shh), log(output.kgh.new), data = db2, alpha = 0.2, colour = 'pink')
qplot(log(tot.lab.daysh), log(output.kgh.new), data = db2, alpha = 0.2, colour = 'pink')
