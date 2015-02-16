setwd('M:/cleaned_data')

library(plyr)
library(DescTools)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')

# read in 2010 data file
db0 <- read.csv("./2010/database_2010.csv")
db0$y2_hhid <- as.character(db0$y2_hhid)

# create a total capital variable and a total plot size variable.
# first make all NA's in capital variables equal to zero 
db0$own.sh[is.na(db0$own.sh)] <- 0
db0$own.sh[is.na(db0$rent.sh)] <- 0

db1 <- mutate(db0, tot.cap.sh = own.sh + rent.sh) %>%
        ddply(.(y2_hhid), transform, total.area = sum(area.gps, na.rm = TRUE))

# create per hectacre values for yield, nitrogen, capital and labour
db1 <- mutate(db1, yield = output.kg.new/area.gps,
              nitrogen = nitrogen.kg/area.gps,
              cap = tot.cap.sh/area.gps,
              labF = fam.lab.days/area.gps,
              labH = hir.lab.days/area.gps,
              pest = pest.kg/area.gps)



# some quick histograms reveal that there are a lot of problems with outliers
# Suggest winsoring at th 99% percentile following Liverpool-Tasie (2014). Take
# A slightly different appraoch with nitrogen and replace any values of more
# than 700kg/ha with 700kg/ha. Discuss with Michiel, now you are taking what
# might be unlikely values and replacing them with 
qplot(yield, data = db1)
qplot(nitrogen, data = db1)
qplot(cap, data = db1)
qplot(labF, data = db1)
qplot(labH, data = db1)

x0 <- c('nitrogen', 'yield')
x <- c('yield', 'nitrogen', 'cap', 'labF', 'labH', 'pest')
db1 <- db1[db1$nitrogen < 7000 & db1$yield < 10000,] 
db1.2 <- db1[db1$nitrogen < 700 & db1$yield < 10000,]
db2 <- winsor4(db1, x, 0.01)
db2.1 <- db2[db2$nitrogen < 700 & db2$yield < 10000,]

# winsored values make everything look a lot nicer (note that values are not yet
# restricted to maize growing fields)
qplot(yield, data = db2)
qplot(nitrogen, data = db2)
qplot(cap, data = db2)
qplot(labF, data = db2)
qplot(labH, data = db2)

# we want to rstrict our analysis to only those plots that have maize on them
# and therefore only plots that have some output
db2 <- filter(db1.2, !is.na(yield))

# furthermore we want to restrict our attention only to plots that have a crop
# of no more than 7, have at least a 25% maize share and which have no cash
# crops, following Sheahan 2011
db2 <- filter(db2, crop.count <= 7 & maize.share >= 25 & !(cash.crop))

qplot(yield, data = db2)
qplot(nitrogen, data = db2)
qplot(log(cap), data = db2)
qplot(labF, data = db2)
qplot(labH, data = db2)

# Because we are only looking at small holder farmers, remove farmers who have a
# plotsize of greater than 10ha, and also remove very small farmers with less
# than 10% of a hectacre (made this bit up)
# db2 <- filter(db2, !(area.gps > 10) & !(area.gps < 0.1))

# finally make a selection of just those variables that we want to include in the
# analysis
db3 <- select(db2, y2_hhid, plotnum, region, yield, nitrogen, cap, labF, labH, soil, soilq,
              soh, aoh, beans, pest, area.gps, total.area, org.fert, crop.count,
              maize.share, seed.type, inter.crop, org.fert, irrigation, slope)

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
db3$zone[db3$region %in% c("Pwani","Morogoro", "Dar es Salaam")] <- "Eastern"
db3$zone[db3$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
db3$zone[db3$region %in% c("Kaskazini Unguja", "Kusini Unguja", "Mjini Magharibi",
                           "Kaskazini Pemba", "Kusini Pemba")] <- "Zanzibar"

# correlation matrix using
m<-cor(db3[, whichNumerics(db3)], use="pairwise.complete.obs")
PlotCorr(m, col=PalDescTools("RedWhiteBlue1", 100), border="grey")

# some scatter plots of yield against the other key variables for the analysis
# first up: nitrogen vs yield, I've subset to drop out the guys who use no
# nitrogen. Alternativeley take logs. Also added a regression line
g1.1 <- ggplot(subset(db1.2, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# facet on soil type
g1.1 <- g0 + facet_grid(. ~ soilq, scales = 'free')
g2 <- g0 + facet_grid(. ~ irrigation, scales = 'free')
g3 <- g0 + facet_grid(. ~ seed.type, scales = 'free')
g4 <- g0 + facet_grid(. ~ inter.crop, scales = 'free')
g5 <- g0 + facet_grid(. ~ pest, scales = 'free')
# yield against log capital large values make it really hard to judge
# relationship in levels
g1 <- ggplot(subset(db3, !(cap == 0)) , aes(x = log(cap), y = yield)) +
        geom_point() + theme_bw()

# yield against lab (note that this is only using the family labour)
g2 <- ggplot(db3, aes(x = lab, y = yield)) + geom_point() + theme_bw()

# relationship between nitrogen and yield facetted on important indicators
g3 <- ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + facet_grid(. ~ soilq)

# now try a simple quadratic function
olsQ <- lm(yield ~ nitrogen + I(nitrogen*nitrogen) + labF + labH + cap,
           data = db1.1)

olsQ1 <- lm(yield ~ nitrogen:maize.belt + I(nitrogen*nitrogen):maize.belt +
                    labF + labH + cap + area.gps + soh + aoh + pest +
                    maize.share + slope + seed.type + irrigation + soil, 
            data = db3)

olsQ <- lm(yield ~ nitrogen:maize.belt + I(nitrogen*nitrogen):maize.belt + cap + labF + labH + irrigation +
                   seed.type + area.gps + soh + aoh + pest + beans + inter.crop +
                   org.fert + log(total.area) + zone + maize.share + 
                   factor(crop.count),
           data = db3)

# getting significance here relies on two guys who are using a lot! of nitrogen
# per hectacre. Without these guys no significance!

subset(db3, nitrogen > 400)
