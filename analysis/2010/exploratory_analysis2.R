# exploratory analysis of 2010 data

setwd('M:/cleaned_data')

library(plyr)
library(DescTools)
library(stargazer)
library(ggplot2)
library(rattle)
library(frontier)
library(dplyr)
source('M:/TZAYG/winsor.R')

# read in 2010 data file
db0 <- read.csv("./2010/database_2010.csv")
db0$y2_hhid <- as.character(db0$y2_hhid)

# create a total capital variable and a total plot size variable.
# first make all NA's in capital variables equal to zero so it is possible to
# sum them. Also change the seed type variable. 
db0$own.sh[is.na(db0$own.sh)] <- 0
db0$own.sh[is.na(db0$rent.sh)] <- 0
db0$seed.type <- revalue(db1$seed.type, c('None purchased' = 'TRADITIONAL'))

db1 <- mutate(db0, tot.cap.sh = own.sh + rent.sh) %>%
        ddply(.(y2_hhid), transform, total.area = sum(area.gps, na.rm = TRUE))

# create per hectacre values for yield, nitrogen, capital and labour
db1 <- mutate(db1, yield = output.kg.new/area.gps,
              nitrogen = nitrogen.kg/area.gps,
              cap = tot.cap.sh/area.gps,
              labF = fam.lab.days/area.gps,
              labH = hir.lab.days/area.gps,
              pest = pest.kg/area.gps)

# some quick histograms reveal that there are a lot of outliers that are clearly
# not a good reflection of the data. One strategy is to winsor values of key
# variables following Liverpool-Tasie etal (2014). To exclude values that are
# highly unlikley. we restrict our attention to values that fall
# within an agronomic range of feasibility. 
qplot(yield, data = db1)
qplot(nitrogen, data = db1)
qplot(cap, data = db1)
qplot(labF, data = db1)
qplot(labH, data = db1)

# restrict attention to plots that do have some yield, i.e. only focus on plots
# that have maize grown on them
db2 <- db1[!is.na(db1$yield),]

# following Sheahan (2011) restrict attention to plots which do not have more
# than 7 crops on them, do not have a cash crop and where maize makes up at 
# least 25% of the total area of the plot
db2 <- filter(db2, crop.count < 7, !cash.crop, maize.share >= 25)

# there are clear outliers in the data so restrict attention to a 'feasible'
# range of values. GYGA indicates yield potential is 10000kg/ha, and fertilizer
# use of greater than 700kg/ha seems ridiculous. 
db2 <- db2[db2$nitrogen <= 700 & db2$yield <= 8000, ]

# close inspection of the plots reveals there are still values that may be
# outliers in the data. A plot of nitrogen against yield highlights these
# outliers both for nitrogen and for yield
g0 <- ggplot(subset(db2, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# a plot to see how area affects the yield and nitrogen variables. One thing
# that is clear is that plots with a very small area are biasing our results
# upwards for both the yield and the nitrogen variables.
g1.1 <- ggplot(db2, aes(x = area.gps, y = yield)) +
        geom_point(alpha = 0.3) + theme_bw() + 
        labs(title = 'yield response to nitrogen') +
        stat_smooth()

g1.2 <- ggplot(subset(db2, !(nitrogen == 0) & area.gps < 10),
               aes(x = area.gps, y = nitrogen)) +
        geom_point(alpha = 0.3) + theme_bw() +
        labs(title = 'yield response to nitrogen') +
        stat_smooth(colour = 'black')

# one solution is to restrict our attention to values which fit the criteria of
# small holder farmers by setting a minimum and maximum limit for plot size. 
db2.1 <- subset(db2, area.gps <= 10 & area.gps >= 0.01)

g2 <- ggplot(subset(db2.1, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# a second solution is to winsor values to remove values that are still
# considered suspect. This is a conservative approach because values that are 
# removed are replaced by the values at whatever quantile winsoring is performed
# as restrictions have already been imposed on the yield and nitrogen variables
# winsoring is carried out at the 0.01% level on these variables
x0 <- c('yield', 'nitrogen', 'cap', 'labF', 'labH', 'pest')
db2 <- winsor4(db2, x0, 0.01)

g3 <- ggplot(subset(db2, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# In the following analysis values have been winsored at the 99th percentile. 
# post winsoring plots
g4 <- ggplot(subset(db2, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()
g4.1 <- ggplot(db2, aes(x = log(cap), y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()
g4.2 <- ggplot(db2, aes(x = labF, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()
g4.3 <- ggplot(subset(db2, !(labH == 0)), aes(x = labH, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# finally make a selection of the variables included in analysis and remove
# earlier dataframes that are no longer needed
db3 <- select(db2, y2_hhid, plotnum, region, yield, nitrogen, cap, labF, labH,
              soil, soilq,soh, aoh, beans, pest, area.gps, total.area, org.fert,
              crop.count, maize.share, seed.type, inter.crop, org.fert,
              irrigation, slope)

rm(db0, db2, x)

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
g5 <- ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
        stat_smooth()

# facet on soil type
g5.1 <- g5 + facet_grid(. ~ soil, scales = 'free')
g5.2 <- g5 + facet_grid(. ~ irrigation, scales = 'free')
g5.3 <- g5 + facet_grid(. ~ seed.type, scales = 'free')
g5.4 <- g5 + facet_grid(. ~ inter.crop, scales = 'free')
g5.5 <- g5 + facet_grid(. ~ pest, scales = 'free')

# yield against log capital large values make it really hard to judge
# relationship in levels
g1 <- ggplot(subset(db3, !(cap == 0)) , aes(x = log(cap), y = yield)) +
        geom_point() + theme_bw()

# yield against lab (note that this is only using the family labour)
g2 <- ggplot(db3, aes(x = lab, y = yield)) + geom_point() + theme_bw()

# relationship between nitrogen and yield facetted on important indicators
g3 <- ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + facet_grid(. ~ soilq)

g4 <- ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
        geom_point() + theme_bw() + facet_grid(. ~ zone)

# -------------------------------
# create new variables so that it is possible to take logs for analysis
#--------------------------------
db3 <- mutate(db3,
              nitrogen2 = ifelse(nitrogen == 0, nitrogen + 1, nitrogen),
              cap2 = ifelse(cap == 0, cap + 1, cap),
              labF2 = ifelse(labF == 0, labF + 1, labF),
              labH2 = ifelse(labH == 0, labH + 1, labH))

# -------------------------------
# Quadratic spec
# -------------------------------

# basic specification
olsQ0 <- lm(yield ~ nitrogen:maize.belt + I(0.5 * nitrogen^2):maize.belt +
                   labF + labH + log(cap2),
           data = db3)

# add agronomic variables
olsQ1 <- lm(yield ~ nitrogen:maize.belt + I(0.5 * nitrogen^2):maize.belt +
                    labF + labH + log(cap2) + area.gps + pest +
                    maize.share + slope + seed.type + irrigation + soil +
                    beans + inter.crop + org.fert + log(total.area) +
                    factor(crop.count),
            data = db3)

# add household characteristics: age and sex
olsQ2 <- lm(yield ~ nitrogen:maize.belt + I(0.5 * nitrogen^2):maize.belt +
                    labF + labH + log(cap2) + area.gps + pest +
                    maize.share + slope + seed.type + irrigation + soil +
                    beans + inter.crop + org.fert + log(total.area) +
                    factor(crop.count) + aoh + soh,
            data = db3)


stargazer(olsQ, olsQ1, olsQ2, type = 'text')

# -------------------------------
# Cobb-Douglas spec
# -------------------------------

# basic specification
olsCD0 <- lm(log(yield) ~ log(nitrogen2):maize.belt + log(cap2) + log(labF2) +
                    log(labH2), 
            data = db3)

# add agronomic indicator variables
olsCD1 <- lm(log(yield) ~ log(nitrogen2):maize.belt + log(cap2) + log(labF2) +
                     log(labH2) + maize.share + slope + seed.type + irrigation +
                     soil + beans + inter.crop + org.fert + log(total.area) +
                     factor(crop.count), 
             data = db3)

# add household characteristics: age and sex
olsCD2 <- lm(log(yield) ~ log(nitrogen2):maize.belt + log(cap2) + log(labF2) +
                     log(labH2) + maize.share + slope + seed.type + irrigation +
                     soil + beans + inter.crop + org.fert + log(total.area) +
                     factor(crop.count) + aoh + soh,
             data = db3)

stargazer(olsCD0, olsCD1, olsCD2, type = 'text')

# ------------------------------
# translog spec - less restrictive than the Cobb-Douglas
# ------------------------------

# basic specification
olsTL0 <- lm(log(yield) ~ log(nitrogen2):maize.belt + log(cap2) + log(labF2) +
                    log(labH2) + I(0.5 * log(nitrogen2)^2) +
                    I(0.5 * log(cap2)^2) + I(0.5 * log(labF2)^2) + 
                    I(0.5 * log(labH2)^2) + I(log(nitrogen2) * log(cap2)) +
                    I(log(nitrogen2) * log(labF2)) +
                    I(log(nitrogen2) * log(labH2)) +
                    I(log(cap2) * log(labF2)) + I(log(cap2) * log(labH2)) +
                    I(log(labF2) * log(labH2)),
            data = db3)

# add agronomic indicator variables

olsTL1 <- lm(log(yield) ~ log(nitrogen2):maize.belt + log(cap2) + log(labF2) +
                    log(labH2) + I(0.5 * log(nitrogen2)^2) +
                    I(0.5 * log(cap2)^2) + I(0.5 * log(labF2)^2) + 
                    I(0.5 * log(labH2)^2) + I(log(nitrogen2) * log(cap2)) +
                    I(log(nitrogen2) * log(labF2)) +
                    I(log(nitrogen2) * log(labH2)) +
                    I(log(cap2) * log(labF2)) + I(log(cap2) * log(labH2)) +
                    I(log(labF2) * log(labH2)) + maize.share + slope +
                    seed.type + irrigation + soil + beans + inter.crop +
                    org.fert + log(total.area) + factor(crop.count),
            data = db3)

# add household characteristics: age and sex

olsTL2 <- lm(log(yield) ~ log(nitrogen2):maize.belt + log(cap2) + log(labF2) +
                    log(labH2) + I(0.5 * log(nitrogen2)^2) +
                    I(0.5 * log(cap2)^2) + I(0.5 * log(labF2)^2) + 
                    I(0.5 * log(labH2)^2) + I(log(nitrogen2) * log(cap2)) +
                    I(log(nitrogen2) * log(labF2)) +
                    I(log(nitrogen2) * log(labH2)) +
                    I(log(cap2) * log(labF2)) + I(log(cap2) * log(labH2)) +
                    I(log(labF2) * log(labH2)) + maize.share + slope +
                    seed.type + irrigation + soil + beans + inter.crop +
                    org.fert + log(total.area) + factor(crop.count) + aoh + soh,
            data = db3)

stargazer(olsTL0, olsTL1, olsTL2, type = 'text')

# ------------------------------
# goodness of fit of models
# ------------------------------

hist(residuals(olsQ0), 15, col ="red")
hist(residuals(olsQ1), 15, col ="red")
hist(residuals(olsQ2), 15, col ="red")

# LEFT SKEWED
hist(residuals(olsCD0), 15, col ="red")
hist(residuals(olsCD1), 15, col ="red")
hist(residuals(olsCD2), 15, col ="red")

# LEFT SKEWED RESIDUALS
hist(residuals(olsTL0), 15, col ="red")
hist(residuals(olsTL1), 15, col ="red")
hist(residuals(olsTL2), 15, col ="red")

# ------------------------------
# Stochastic frontiers - Cobb-Douglas spec
# ------------------------------

sfaCD0 <- sfa(log(yield) ~ log(nitrogen) + log(cap2) + log(labF) + log(labH),
             data = db3)

sfaCD1 <- sfa(log(yield) ~ log(nitrogen2) + log(cap2) + log(labF2) +
                     log(labH2) + maize.share + slope + seed.type + irrigation +
                     soil + beans + inter.crop + org.fert + log(total.area) +
                     factor(crop.count), 
             data = db3)

sfaCD2 <- sfa(log(yield) ~ log(nitrogen2) + log(cap2) + log(labF2) +
                     log(labH2) + maize.share + slope + seed.type + irrigation +
                     soil + beans + inter.crop + org.fert + log(total.area) +
                     factor(crop.count) + aoh + soh,
             data = db3)

# ------------------------------
# Stochastic frontiers - translog spec
# ------------------------------

# basic specification
sfaTL0 <- sfa(log(yield) ~ log(nitrogen2) + log(cap2) + log(labF2) +
                     log(labH2) + I(0.5 * log(nitrogen2)^2) +
                     I(0.5 * log(cap2)^2) + I(0.5 * log(labF2)^2) + 
                     I(0.5 * log(labH2)^2) + I(log(nitrogen2) * log(cap2)) +
                     I(log(nitrogen2) * log(labF2)) +
                     I(log(nitrogen2) * log(labH2)) +
                     I(log(cap2) * log(labF2)) + I(log(cap2) * log(labH2)) +
                     I(log(labF2) * log(labH2)),
             data = db3)

# add agronomic indicator variables

sfaTL1 <- sfa(log(yield) ~ log(nitrogen2) + log(cap2) + log(labF2) +
                     log(labH2) + I(0.5 * log(nitrogen2)^2) +
                     I(0.5 * log(cap2)^2) + I(0.5 * log(labF2)^2) + 
                     I(0.5 * log(labH2)^2) + I(log(nitrogen2) * log(cap2)) +
                     I(log(nitrogen2) * log(labF2)) +
                     I(log(nitrogen2) * log(labH2)) +
                     I(log(cap2) * log(labF2)) + I(log(cap2) * log(labH2)) +
                     I(log(labF2) * log(labH2)) + maize.share + slope +
                     seed.type + irrigation + soil + beans + inter.crop +
                     org.fert + log(total.area) + factor(crop.count),
             data = db3)

# add household characteristics: age and sex

sfaTL2 <- sfa(log(yield) ~ log(nitrogen2) + log(cap2) + log(labF2) +
                     log(labH2) + I(0.5 * log(nitrogen2)^2) +
                     I(0.5 * log(cap2)^2) + I(0.5 * log(labF2)^2) + 
                     I(0.5 * log(labH2)^2) + I(log(nitrogen2) * log(cap2)) +
                     I(log(nitrogen2) * log(labF2)) +
                     I(log(nitrogen2) * log(labH2)) +
                     I(log(cap2) * log(labF2)) + I(log(cap2) * log(labH2)) +
                     I(log(labF2) * log(labH2)) + maize.share + slope +
                     seed.type + irrigation + soil + beans + inter.crop +
                     org.fert + log(total.area) + factor(crop.count) + aoh + soh,
             data = db3)

# still to add - wald test between the cobb-douglas and translog functions
