setwd('M:/cleaned_data')

library(plyr)
library(DescTools)
library(stargazer)
library(ggplot2)
library(dplyr)
source('M:/TZAYG/winsor.R')

# read in 2010 data file
db0 <- read.csv("./2010/database_2010.csv")
db3$y2_hhid <- as.character(db3$y2_hhid)

# create a total capital variable and a total plot size variable.
# first make all NA's in capital variables equal to zero 
db0$own.sh[is.na(db0$own.sh)] <- 0
db0$own.sh[is.na(db0$rent.sh)] <- 0

db1 <- mutate(db0, tot.cap.sh = own.sh + rent.sh) %>%
        ddply(.(y2_hhid), transform, total.area = sum(area.gps, na.rm = TRUE))

# create per hectacre values for nitrogen and output
db1 <- mutate(db1, yield = output.kg.new/area.gps,
              nitrogen = nitrogen.kg/area.gps)

# create area weighted values for capital and labour by field
db1 <- mutate(db1, cap = tot.cap.sh*(area.gps/total.area),
              lab = fam.lab.days * (area.gps/total.area))

# some quick histograms reveal that there are a lot of problems with outliers
# Suggest winsoring at th 99% percentile following Liverpool-Tasie (2014). Take
# A slightly different appraoch with nitrogen and replace any values of more
# than 700kg/ha with 700kg/ha. Discuss with Michiel, now you are taking what
# might be unlikely values and replacing them with 
qplot(yield, data = db1)
qplot(nitrogen, data = db1)
qplot(cap, data = db1)
qplot(lab, data = db1)

x <- c('yield', 'nitrogen', 'cap', 'lab')
db2 <- winsor4(db1, x)

# winsored values make everything look a lot nicer (note that values are not yet
# restricted to maize growing fields)
qplot(yield, data = db2)
qplot(nitrogen, data = db2)
qplot(cap, data = db2)
qplot(lab, data = db2)

# we want to rstrict our analysis to only those plots that have maize on them
# and therefore only plots that have some output
db2 <- filter(db2, !is.na(yield))

# furthermore we want to restrict our attention only to plots that have a crop
# of no more than 7, have at least a 25% maize share and which have no cash
# crops, following Sheahan 2011
db2 <- filter(db2, crop.count <= 7 & maize.share >= 25 & !(cash.crop))

qplot(yield, data = db2)
qplot(nitrogen, data = db2)
qplot(log(cap), data = db2)
qplot(lab, data = db2)

# Because we are only looking at small holder farmers, remove farmers who have a
# plotsize of greater than 10ha, and also remove very small farmers with less
# than 10% of a hectacre (made this bit up)
db2 <- filter(db2, !(area.gps > 10) & !(area.gps < 0.1))

# finally make a selection of just those variables that we want to include in the
# analysis
db3 <- select(db2, y2_hhid, plotnum, region, yield, nitrogen, cap, lab, soil, soilq,
              soh, aoh, beans, pest, area.gps, total.area, org.fert, crop.count,
              maize.share, seed.type, inter.crop, org.fert, irrigation)

# make a maize.belt variable based on the regions that have the highest maize
# yields
by_region <- group_by(db3, region)
y <- summarise(by_region, avg_yield = mean(yield, na.rm = TRUE))
y <- y[order(y$avg_yield),]
x <- as.character(y$region[y$avg_yield > 1000])
db3$maize.belt <- ifelse(db3$region %in% x, TRUE, FALSE)

# correlation matrix using
m<-cor(db3[, whichNumerics(db3)], use="pairwise.complete.obs")
PlotCorr(m, col=PalDescTools("RedWhiteBlue1", 100), border="grey")

# some scatter plots of yield against the other key variables for the analysis
# first up: nitrogen vs yield, I've subset to drop out the guys who use no
# nitrogen. Alternativeley take logs. Also added a regression line
g0 <- ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# facet on soil type
g1 <- g0 + facet_grid(. ~ soil, scales = 'free')
g2 <- g0 + facet_grid(. ~ irrigation, scales = 'free')
g3 <- g0 + facet_grid(. ~ seed.type, scales = 'free')

# yield against log capital large values make it really hard to judge
# relationship in levels
g1 <- ggplot(subset(db3, !(cap == 0)) , aes(x = log(cap), y = yield)) +
        geom_point() + theme_bw()

# yield against lab (note that this is only using the family labour)
g2 <- ggplot(db3, aes(x = lab, y = yield)) + geom_point() + theme_bw()

# relationship between nitrogen and yield facetted on important indicators
g3 <- ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + facet_grid(. ~ soilq)

# now try a simple quadratic regression
olsQ <- lm(yield ~ nitrogen + I(nitrogen*nitrogen) + cap + lab + irrigation +
                   seed.type + area.gps + soh + aoh + pest + beans + inter.crop +
                   org.fert + log(total.area) + region + maize.share + 
                   factor(crop.count),
           data = subset(db3, !(nitrogen > 1000)))

# getting significance here relies on two guys who are using a lot! of nitrogen
# per hectacre. Without these guys no significance!

subset(db1, nitrogen > 1000)