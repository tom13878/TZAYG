# exploratory analysis of year 1 data - note that there is not a GPS measured
# area variable for all of the plots. Here we are relying on the farmers
# estimate for 75% of plots. This is not reliable and creates all sorts of 
# problems

setwd('M:/cleaned_data')
library(plyr)
library(DescTools)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')
source('M:/TZAYG/LaTeX_out.R')

# read in data
db0 <- read.csv("./2008/database_2008.csv")
db0$hhid <- as.character(db0$hhid)

# create an area variable which is equal to the GPS measured plot area when 
# available and the farmers estimated area otherwise. Set capital variables 
# equal to zero to male it possible to sum them. 
db1 <- mutate(db0, area = ifelse(is.na(area.gps), area.est, area.gps))
db1$area[db1$area == 0] <- NA

db1$own.sh[is.na(db1$own.sh)] <- 0
db1$own.sh[is.na(db1$rent.sh)] <- 0

db1 <- mutate(db1, tot.cap.sh = own.sh + rent.sh) %>%
        ddply(.(hhid), transform, total.area = sum(area, na.rm = TRUE))

# create per hectacre variables
db1 <- mutate(db1, yield = output.kg.new/area,
              nitrogen = nitrogen.kg/area,
              cap = tot.cap.sh/area,
              labF = fam.lab.days/area,
              labH = hir.lab.days/area,
              pest = pest.kg/area)

qplot(nitrogen, binwidth = 2.5, alpha = 0.3, fill = 'lightyellow', data = db1)
qplot(yield, alpha = 0.3, fill = 'lightyellow', data = db2)

# the area measurments are an issue because the farmer area measurements are too
# discrete - GPS measurements are much better
g0 <- ggplot(subset(db1, !(nitrogen == 0) & nitrogen < 200 & yield < 5000),
             aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# histogram of area highlights the discreteness caused by the farmers estimates
g1 <- ggplot(subset(db1, area > 0.01 & area < 10), aes(x = area)) +
        geom_histogram(binwidth = 0.1, fill = 'red', colour = 'black') +
        theme_bw()

# restrict attention to plots that do have some yield, i.e. only focus on plots
# that have maize grown on them
db2 <- db1[!is.na(db1$yield),]

# following Sheahan (2011) restrict attention to plots which do not have more
# than 7 crops on them, do not have a cash crop and where maize makes up at 
# least 25% of the total area of the plot
db2 <- filter(db2, crop.count < 7, !cash.crop, maize.share >= 25)

# there are clear outliers in the data so restrict attention to a 'feasible'
# range of values. GYGA indicates yield potential is 10000kg/ha, and fertilizer
# use of greater than 700kg/ha seems ridiculous. There is a code problem here
# with NA values. When subsetting NA values for Nitrogen drop out for every
# variable in the dataset. Hence nitrogen values are only compared for those
# values that are not NA
db2 <- db2[db2$nitrogen[!is.na(db2$nitrogen)] <= 700, ] 
db2 <- db2[db2$yield <= 8000,]

# inspection of plots indicates that there are still outliers in the data in 
# particular, because we have discrete measures of nitrogen (because fertilizer)
# is sold in 50kg bags, and because the farmers round their plot sizes to the 
# nearest whole or half number.
g2 <- ggplot(subset(db2, !(nitrogen == 0)),
             aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

nitrogen_test <- subset(db2, !is.na(nitrogen))
by_nitrogen <- group_by(nitrogen_test, nitrogen)
nitrogen_test2 <- summarise(by_nitrogen, n())
summary(nitrogen_test2)

g2.1 <- ggplot(subset(db2, !(nitrogen == 0)),
             aes(x = area, y = nitrogen)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

g2.2 <- ggplot(subset(db2, !(nitrogen == 0)),
             aes(x = area, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# winsor variables that enter the production function at the 95th/99th percentile
x1 <- c('yield', 'nitrogen', 'cap', 'labF', 'labH', 'pest')
db2 <- winsor4(db2, x1, 0.05)

g3 <- ggplot(subset(db2, !(nitrogen == 0)),
             aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# finally make a selection of just those variables that we want to include in the
# analysis
db3 <- select(db2, hhid, plotnum, region, yield, nitrogen, cap, labF, labH,
              soil, soilq,soh, aoh, beans, pest, area.gps, total.area, org.fert,
              crop.count, maize.share, seed.type, inter.crop, org.fert,
              irrigation, slope)

# make a maize.belt variable based on the regions that have the highest maize
# yields
by_region <- group_by(db3, region)
y <- summarise(by_region, avg_yield = mean(yield, na.rm = TRUE))
y <- y[order(y$avg_yield),]
x <- as.character(y$region[y$avg_yield > 1000])
db3$maize.belt <- ifelse(db3$region %in% x, TRUE, FALSE)

# make zone variable
db3$zone[db3$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
db3$zone[db3$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
db3$zone[db3$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
db3$zone[db3$region %in% c("Singida","Dodoma")] <- "Central"
db3$zone[db3$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
db3$zone[db3$region %in% c("Pwani","Morogoro", "Dar es salaam")] <- "Eastern"
db3$zone[db3$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
db3$zone[db3$region %in% c("KASKAZINI UNGUJA", "KUSINI UNGUJA", "MJINI/MAGHARIBI UNGUJA",
                                     "KASKAZINI PEMBA", "KUSINI PEMBA")] <- "Zanzibar"

# Use the 2008 data to construct some tables to include in the paper. The data 
# is to discrete to be used for a proper regression analysis!

# ------------------------
# descriptive statistics: by region and by zone
# ------------------------

by_region <- group_by(db3, region) %>% summarise(
        no.farmers = length(unique(hhid)),
        no.plots = length(unique(paste0(hhid, plotnum))),
        avg.yield = round(mean(yield, na.rm = TRUE), 2),
        avg.nitrogen = round(mean(nitrogen, na.rm = TRUE), 2),
        f = round((length(unique(hhid[!(is.na(nitrogen))]))/no.farmers)*100, 1),
        p = round(
                (length
                 (unique(paste0(hhid, plotnum)[!(is.na(nitrogen))]))/no.plots)*100,
                1)
)

by_region$region <- as.character(by_region$region)

by_zone <- group_by(db3, zone) %>% summarise(
        no.farmers = length(unique(hhid)),
        no.plots = length(unique(paste0(hhid, plotnum))),
        avg.yield = round(mean(yield, na.rm = TRUE), 2),
        avg.nitrogen = round(mean(nitrogen, na.rm = TRUE), 2),
        f = round((length(unique(hhid[!(is.na(nitrogen))]))/no.farmers)*100, 1),
        p = round(
                (length
                 (unique(paste0(hhid, plotnum)[!(is.na(nitrogen))]))/no.plots)*100,
                1)
)
# output for LaTeX tables
print(LaTeX_out(by_region))
print(LaTeX_out(by_zone))
