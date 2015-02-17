#' analysis of 2010 data from world bank surveys. 

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
db1$pest.kg[db1$pest.kg == 0] <- NA
db1$org.fert.kg[db1$org.fert.kg == 0] <- NA
db1$tot.lab.days[db1$tot.lab.days == 0] <- NA
db1$tot.cap.sh[db1$tot.cap.sh == 0] <- NA
# table of some key variables using plots 
stargazer(select(db1, area, nitrogen.kg, output.kg.new), type = 'text')

#' remove values for areas that are too small or too big, There are large
#' (impossible) outliers in both variables. One
#' option is to winsor these values. An alternative is just to throw them away.
#' here I choose to just throw them away (discuss with Michiel and Jeff)
#' variables are mutated into per hectacre form. Note that the trim function
#' is designed to remove the 1st and 99th percentiles
x0 <- c('area', 'nitrogen.kg', 'output.kg.new', 'output.kg.old', 'org.fert.kg',
       'pest.kg', 'tot.lab.days', 'tot.cap.sh')
db2 <- trim2(db1, x0) %>%
        mutate(output.kgh.new = output.kg.new/area,
               output.kgh.old = output.kg.old/area,
               nitrogen.kgh = nitrogen.kg/area,
               pest.kgh = pest.kg/area,
               org.fert.kgh = org.fert.kg/area,
               tot.lab.daysh = tot.lab.days/area,
               tot.cap.shh = tot.cap.sh/area)

#' basic descriptive statistics
df0 <- filter(db2, year == 2008) %>%
        select(area, nitrogen.kgh, output.kgh.new, output.kgh.old, pest.kgh,
              org.fert.kgh, tot.lab.daysh, tot.cap.shh)

df1 <- filter(db2, year == 2010) %>%
        select(area, nitrogen.kgh, output.kgh.new, output.kgh.old, pest.kgh,
               org.fert.kgh, tot.lab.daysh, tot.cap.shh)

stargazer(df0, type = 'text')
stargazer(df1, type = 'text')

Freq(db2$soilq)
Freq(db2$seed.type)
Freq(db2$irrigation)

#' some household characteristics looking at farmers who use fertilizer and 
#' those who do not. 
by_year <- group_by(db2, year) %>% ddply(.(hhid), transform,
                                         fert = ifelse(any(!(is.na(nitrogen.kg))), TRUE, FALSE))
hh08_users <- filter(by_year, year == 2008, fert == TRUE)
hh08_users <- unique(select(hh08_users, hhid, region, aoh, soh, total.area, tot.cap.sh))
hh08_non_users <- filter(by_year, year == 2008, fert == FALSE)
hh08_non_users <- unique(select(hh08_non_users, hhid, region, aoh, soh, total.area, tot.cap.sh))        

hh10_users <- filter(by_year, year == 2010, fert == TRUE)
hh10_users <- unique(select(hh10_users, hhid, region, aoh, soh, total.area, tot.cap.sh))
hh10_non_users <- filter(by_year, year == 2010, fert == FALSE)
hh10_non_users <- unique(select(hh10_non_users, hhid, region, aoh, soh, total.area, tot.cap.sh))  

stargazer(hh08_users, type = 'text')
stargazer(hh08_non_users, type = 'text')
stargazer(hh10_users, type = 'text')
stargazer(hh10_non_users, type = 'text')

# test whether mean fertilizer use is statistically different across years using
# Anova methods, first have a look at some distributions and then start
# defining an environment using some factors. problem is what kind of
# distribution is appropriate 
# area histogram across years - area imputation is clear in 2008
plot_hist <- function(df, col, binwidth){
        ggplot(df, aes_string(col)) +
                geom_histogram(binwidth = binwidth, fill = 'lightyellow', colour = 'black') +
                theme_bw() + facet_grid(. ~ year, scales = 'free')
}

g0 <- plot_hist(db2, 'area', 0.2)
g1 <- plot_hist(db2, 'nitrogen.kgh', binwidth = 10)
g2 <- plot_hist(db2, 'output.kgh.new', binwidth = 250)
g3 <- plot_hist(db2, 'tot.lab.daysh', binwidth = 200)
g4 <- plot_hist(db2, 'org.fert.kgh', binwidth = 1000)
g01 <- ggplot(db2, aes(x = log(area))) +
        geom_histogram(binwidth = 0.05, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year, scales = 'free')
g11 <- ggplot(db2, aes(x = log(nitrogen.kgh))) +
        geom_histogram(binwidth = 0.05, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year, scales = 'free')
g21 <- ggplot(db2, aes(x = log(output.kgh.new))) +
        geom_histogram(binwidth = 0.5, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year, scales = 'free')

#' frequency tables for some key fator variables and look for normality in 
#' nitrogen 
Freq(db2$soilq)
Freq(db2$seed.type)
Freq(db2$irrigation)


#' table of key variables after trimming has been performed and using per 
#' hectacre values. Definitely an improvement but still suspicious of some of 
#' the higher values!
stargazer(select(db2, output.kgh.new, area, nitrogen.kgh, tot.lab.daysh, tot.cap.shh), type = 'text')        

# plots of key variables, look for relationships
# 1. plot of nitrogen against output. year 2 panel reveals a lot of farmers who
#    are using a small amount of nitrogen per hectacre. This is not present in
#    year 1
g5 <- ggplot(db2, aes(x = nitrogen.kgh, y = output.kgh.new)) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 2. plot of output against area to check if there are a lot of small plots
#    (where there would be a small amount of fertilizer used). Data seems to
#    suggest that smaller plots are more productive tahn big ones
g6 <- ggplot(db2, aes(x = area, y = output.kgh.new)) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 3. check whether overall 'farm size' measured in terms of total plot area
#    makes a difference. Similar conclusion - seems that the smaller the farm
#    the more productive it is.
g7 <- ggplot(db2, aes(x = total.area, y = output.kgh.new)) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 4. There is a large variation in area, nitrogen use per hectacre and output.
#    try a log scale to deal with this. Strong positive relationship exhibited
#    across years.
g8 <- ggplot(db2, aes(x = log(nitrogen.kgh), y = log(output.kgh.new))) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 5. look at histogram of output values split by year. 
g9 <- ggplot(db2, aes(x = output.kgh.new)) +
        geom_histogram(binwidth = 300, fill = 'lightyellow', colour = 'black') +
        theme_bw() + facet_grid(. ~ year)

# 6. look for some relationships between other factors and output 
g10 <- ggplot(na.omit(select(db2, seed.type, output.kgh.old)), aes(x = factor(seed.type), output.kgh.old)) +
        geom_boxplot(outlier.shape = 21) 
g11 <- ggplot(na.omit(select(db2, irrigation, output.kgh.old)), aes(x = factor(irrigation), output.kgh.old)) +
        geom_boxplot(outlier.shape = 21) 
g12 <- ggplot(na.omit(select(db2, soilq, output.kgh.old)), aes(x = factor(soilq), output.kgh.old)) +
        geom_boxplot(outlier.shape = 21) 

# 7. add colour to plots based on some factors
g13 <- ggplot(db2, aes(x = log(nitrogen.kgh), y = log(output.kgh.new), colour = factor(soilq))) +
        geom_point() + facet_grid(. ~ year, scales = 'free')

# 8. do farmers with low soil quality use less fertilizer? From the plot it
#    seems the case
g14 <- ggplot(na.omit(select(db2, nitrogen.kgh, soilq)), aes(x = factor(soilq), y = nitrogen.kgh)) +
                     geom_boxplot(outlier.shape = 21)


# 9. look again at relationship between nitrogen per hectacre and output per
#     hectacre facetted on region
g15 <- ggplot(db2, aes(x = log(nitrogen.kgh), y = log(output.kgh.new))) +
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

# A look at the regions which did receive vouchers for subsidies in year 2
# some regions did receive partciulalry high subsidies such as Ruvuma and Iringa
xtabs( ~  region + voucher, data = db2)

# is there a difference in characteristics between those who received subsidies 
# and those who do not - do some ANOVA tests for just year 2 dats for those who
# did receive subsidies and those who did not
