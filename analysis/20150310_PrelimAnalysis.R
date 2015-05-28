# PROJECT: DFID Yield Gap and IPOP
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Purpose
#' Preliminary analysis using yield gap framework for DFID kick off workshop 10-12/03/2015 
#' Only analysis for 2010
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "ggplot2", "stringr", "gdata", "car", "tidyr", "RColorBrewer", "plyr", "dplyr")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("stargazer", "frontier")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET WORKING DIRECTORY
wdpath<-"D:\\Dijk158\\Dropbox\\Michiel_research\\MicroIPOPCode\\TZAYG"
setwd(wdpath)

# SOURCE functions
source('winsor.R')

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# DATA PREPARATION
# load data that is already prepared
db0 <- read.csv("database_2010.csv")
db0$y2_hhid <- as.character(db0$y2_hhid)

# create a total capital variable and a total plot size variable.
# first make all NA's in capital variables equal to zero so it is possible to
# sum them. Also change the seed type variable. 
db0$own.sh[is.na(db0$own.sh)] <- 0
db0$own.sh[is.na(db0$rent.sh)] <- 0
db0$seed.type <- revalue(db0$seed.type, c('None purchased' = 'TRADITIONAL'))

# create a total labour variable.
# first make all NA's in capital variables equal to zero so it is possible to
# sum them. 
db0$fam.lab.days[is.na(db0$fam.lab.days)] <- 0
db0$hir.lab.days[is.na(db0$hir.lab.days)] <- 0

# Create variables
db1 <- mutate(db0, tot.cap.sh = own.sh + rent.sh, 
                    tot.lab.days = fam.lab.days + hir.lab.days) %>%
  ddply(.(y2_hhid), transform, total.area = sum(area.gps, na.rm = TRUE))

# create per hectacre values for yield, nitrogen, capital and labour
db1 <- mutate(db1, yield = output.kg.new/area.gps,
              nitrogen = nitrogen.kg/area.gps,
              cap = tot.cap.sh/area.gps,
              lab = tot.lab.days/area.gps,
              pest = pest.kg/area.gps)

# restrict attention to plots that do have some yield, i.e. only focus on plots
# that have maize grown on them
db2 <- db1[!is.na(db1$yield),]

# following Sheahan (2011) restrict attention to plots which do not have a cash crop
# and where maize makes up at least 25% of the total area of the plot 
db2 <- filter(db2, !cash.crop, maize.share >= 25)

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
db2 <- subset(db2, area.gps <= 10 & area.gps >= 0.01)

g2 <- ggplot(subset(db2, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth()

# a second solution is to winsor values to remove values that are still
# considered suspect. This is a conservative approach because values that are 
# removed are replaced by the values at whatever quantile winsoring is performed
# as restrictions have already been imposed on the yield and nitrogen variables
# winsoring is carried out at the 0.01% level on these variables
x0 <- c('yield', 'nitrogen', 'cap', 'lab', 'pest')
db2 <- winsor4(db2, x0, 0.01)

g3 <- ggplot(subset(db2, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth()


# finally make a selection of the variables included in analysis and remove
# earlier dataframes that are no longer needed
db3 <- dplyr::select(db2, y2_hhid, plotnum, region, yield, nitrogen, cap, lab,
              soil, soilq,soh, aoh, beans, pest, area.gps, total.area, org.fert,
              crop.count, maize.share, seed.type, inter.crop, org.fert,
              irrigation, slope)

rm(db0, db2)

# make a maize.belt variable based on the regions that have the highest maize yields
# ASK TOM => tricky way of doing it in this way. 
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

# yield against log capital large values make it really hard to judge
# relationship in levels
ggplot(subset(db3, !(cap == 0)) , aes(x = log(cap), y = yield)) +
  geom_point() + theme_bw()

# yield against lab
ggplot(db3, aes(x = lab, y = yield)) + geom_point() + theme_bw()

# relationship between nitrogen and yield facetted on important indicators
ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
  geom_point() + theme_bw() + facet_grid(. ~ soilq)

ggplot(subset(db3, !(nitrogen == 0)), aes(x = nitrogen, y = yield)) +
  geom_point() + theme_bw() + stat_smooth() + facet_grid(. ~ zone, scale="free")

# Dataset with plots that use fertilizer
db4 <- subset(db3, !(nitrogen==0))


# Regression Analysis
# Further analysis needed for adding log(x+1) - search on the internet
olsQ1 <- lm(yield ~ nitrogen:maize.belt + I(nitrogen^2):maize.belt +
              lab + log(cap+1) + log(area.gps) + pest +
              maize.share + slope + seed.type + irrigation + soil +
              beans + inter.crop + org.fert + log(total.area) +
              factor(crop.count) + aoh + soh,
            data = db3)

olsQ1b <- lm(yield ~ nitrogen + I(nitrogen^2) +
               lab + log(cap+1) + log(area.gps) + pest +
               maize.share + slope + seed.type + irrigation + soil +
               beans + inter.crop + org.fert + log(total.area) +
               factor(crop.count) + aoh + soh,
             data = db4)

stargazer(olsQ1, olsQ1b, type="text")

# Full quadratic
olsFQ <- lm(yield ~ cap + lab + nitrogen +
                  I( 0.5 * cap^2 ) + I( 0.5 * lab^2 ) + I( 0.5 * nitrogen^2 ) +
                  I( cap * lab ) + I( cap * nitrogen ) + I( lab * nitrogen ),
                 data = db4 )
stargazer(olsFQ, type="text")

sfaFQ <- sfa(yield ~ cap + lab + nitrogen +
              I( 0.5 * cap^2 ) + I( 0.5 * lab^2 ) + I( 0.5 * nitrogen^2 ) +
              I( cap * lab ) + I( cap * nitrogen ) + I( lab * nitrogen ),
            data = db4 )
# Does not solve => quadratic not suitable for SFA

# Cobb Douglas
olsCD1 <- lm(log(yield) ~ log(nitrogen+1) + log(cap+1) + log(lab+1) +
                maize.share + slope + seed.type + irrigation +
                soil + beans + inter.crop + org.fert + log(total.area) +
                factor(crop.count) + aoh + soh,
              data = db3)


olsCD1b <- lm(log(yield) ~ log(nitrogen+1) + log(cap+1) + log(lab+1) +
               maize.share + slope + seed.type + irrigation +
               soil + beans + inter.crop + org.fert + log(total.area) +
               factor(crop.count) + aoh + soh,
             data = db4)

# CD and quadratic hybrid

olsCDQ1 <- lm(log(yield) ~ nitrogen + I(nitrogen^2) + log(cap+1) + log(lab+1) +
                maize.share + slope + seed.type + irrigation +
                soil + beans + inter.crop + org.fert + log(total.area) +
                factor(crop.count) + aoh + soh,
              data = db3)



olsCDQ1b <- lm(log(yield) ~ nitrogen + I(nitrogen^2) + log(cap+1) + log(lab+1) +
                maize.share + slope + seed.type + irrigation +
                soil + beans + inter.crop + org.fert + log(total.area) +
                factor(crop.count) + aoh + soh,
              data = db4)


stargazer(olsCD1, olsCD1b, olsCDQ1, olsCDQ1b, type="text")

# Stochastic frontier
sfaCD1 <- sfa(log(yield) ~ log(nitrogen+1) + log(cap+1) + log(lab+1) +
                maize.share + slope + seed.type + irrigation +
                soil + beans + inter.crop + org.fert + log(total.area) +
                factor(crop.count) + aoh + soh,
              data = db3)

sfaCD1b <- sfa(log(yield) ~ log(nitrogen+1) + log(cap+1) + log(lab+1) +
                 maize.share + slope + seed.type + irrigation +
                 soil + beans + inter.crop + org.fert + log(total.area) +
                 factor(crop.count) + aoh + soh,
               data = db4)

# stochastic frontier hybrid
sfaCDQ1 <- sfa(log(yield) ~ nitrogen + I(nitrogen^2) + log(cap+1) + log(lab+1) +
                maize.share + slope + seed.type + irrigation +
                soil + beans + inter.crop + org.fert + log(total.area) +
                factor(crop.count) + aoh + soh,
              data = db3)


sfaCDQ1b <- sfa(log(yield) ~ nitrogen + I(nitrogen^2) + log(cap+1) + log(lab+1) +
                 maize.share + slope + seed.type + irrigation +
                 soil + beans + inter.crop + org.fert + log(total.area) +
                 factor(crop.count) + aoh + soh,
               data = db4)

# simple hybrid model so we do not have to evaluate so much at the mean
# Leaving out non-significant values.
# Model selected for DFID kick off.
sfaCDQ1c <- sfa(log(yield) ~ nitrogen + I(nitrogen^2) + log(cap+1) + log(lab+1) +
                  maize.share + log(total.area), 
                data = db4)


# simple CD model so we do not have to evaluate so much at the mean
# Leaving out non-significant values.
olsCD1c <- lm(log(yield) ~ nitrogen + I(nitrogen^2) + log(cap+1) + log(lab+1) +
                  maize.share + log(total.area),
                data = db4)

# Summarize results
summary(sfaCDQ1c, extraPar = TRUE) #calculates the above
lrtest(sfaCDQ1c) # OLS is rejected in favour of sfa
          
# Profit maximizing yield analysis
# ASK Tom for fertilizer prices and maize prices
# Assume arbitrary data now.
pfert <- 4542
pmaize <- 500

# Compute fertilizer demand function for Cobb Douglas.
# Short run demand function has the following form (see sheets for example)
# Pfert = elastfert * qfert^(elasticityfert-1) * qother^elasticity.other * pmaize
# Pfert = constant * qfert^(elasticityfert-1)
# meanlab <- mean(db4$lab)
# meancap <- mean(db4$cap)
# meanyield <- mean(db4$yield)
# elastfert <- coef(sfaCD1)["log(nitrogen + 1)"]
# elastcap <- coef(sfaCD1)["log(cap + 1)"]
# elastlab <- coef(sfaCD1)["log(lab + 1)"]
# const <- elastfert * meanlab^elastlab * meancap^elastcap * pmaize
# db4 <-mutate(db4, pfertcalc = const * nitrogen^(elastfert-1))
#  
# ggplot(data=db4) +
#   geom_point(aes(x = nitrogen, y = pfertcalc)) +
#   theme_bw() + labs(title = 'yield response to nitrogen')
  
# Compute fertilizer demand function for hybrid function.
# Tomoya Matsumoto & Takashi Yamano (2009)
meanyield <- mean(db4$yield)
meannitrogen <- mean(db4$nitrogen, na.rm=TRUE)
anitrogen <- coef(sfaCDQ1c)["nitrogen"]
bnitrogen <- coef(sfaCDQ1c)["I(nitrogen^2)"]

db4 <-mutate(db4, pfertcalc = meanyield * pmaize * (anitrogen + bnitrogen *2 * nitrogen))
MPP = mean(db4$yield) * (anitrogen + 2 * bnitrogen * meannitrogen)

# Draw demand curve
ggplot(data=db4) +
  geom_point(aes(x = nitrogen, y = pfertcalc)) +
  theme_bw() + labs(title = 'yield response to nitrogen')

# Plots that produce in area with diminishing marginal returns
negmv <- filter(db4, pfertcalc <0)

# Profit maximizing nitrogen level
profitmaxnitrogen <- ((pfert/(meanyield * pmaize))- anitrogen)/(2*bnitrogen)

# Profit mazimizing yield level at the average
profitmaxyield <- exp(
  coef(sfaCDQ1c)["(Intercept)"] + 
  coef(sfaCDQ1c)["nitrogen"] * profitmaxnitrogen +
  coef(sfaCDQ1c)["I(nitrogen^2)"] * (profitmaxnitrogen^2) +
  coef(sfaCDQ1c)["log(cap + 1)"] * mean(log(db4$cap+1)) +
  coef(sfaCDQ1c)["log(lab + 1)"] * mean(log(db4$lab+1)) +
  coef(sfaCDQ1c)["maize.share"] * mean(db4$maize.share) +
  coef(sfaCDQ1c)["log(total.area)"] * mean(log(db4$total.area))
)

# Add profit mazimizing yield per plot
db4 <- mutate(db4, yieldprofmax =  exp(
                  coef(sfaCDQ1c)["(Intercept)"] + 
                  coef(sfaCDQ1c)["nitrogen"] * profitmaxnitrogen +
                  coef(sfaCDQ1c)["I(nitrogen^2)"] * (profitmaxnitrogen^2) +
                  coef(sfaCDQ1c)["log(cap + 1)"] * log(cap+1) +
                  coef(sfaCDQ1c)["log(lab + 1)"] * log(lab+1) +
                  coef(sfaCDQ1c)["maize.share"] * maize.share +
                  coef(sfaCDQ1c)["log(total.area)"] * log(total.area))
)



# Add frontier line defined at the average
db4 <- mutate(db4, yieldfitmean =  exp(
                                  coef(sfaCDQ1c)["(Intercept)"] + 
                                  coef(sfaCDQ1c)["nitrogen"] * nitrogen +
                                  coef(sfaCDQ1c)["I(nitrogen^2)"] * (nitrogen^2) +
                                  coef(sfaCDQ1c)["log(cap + 1)"] * mean(log(db4$cap+1)) +
                                  coef(sfaCDQ1c)["log(lab + 1)"] * mean(log(db4$lab+1)) +
                                  coef(sfaCDQ1c)["maize.share"] * mean(db4$maize.share) +
                                  coef(sfaCDQ1c)["log(total.area)"] * mean(log(db4$total.area)))
              )

# Add CD (average) line defined at the average
db4 <- mutate(db4, yieldfitmeanCD =  exp(
  coef(olsCD1c)["(Intercept)"] + 
    coef(olsCD1c)["nitrogen"] * nitrogen +
    coef(olsCD1c)["I(nitrogen^2)"] * (nitrogen^2) +
    coef(olsCD1c)["log(cap + 1)"] * mean(log(db4$cap+1)) +
    coef(olsCD1c)["log(lab + 1)"] * mean(log(db4$lab+1)) +
    coef(olsCD1c)["maize.share"] * mean(db4$maize.share) +
    coef(olsCD1c)["log(total.area)"] * mean(log(db4$total.area)))
)

# Compute economic yield gap
db4$economicgap <- db4$yield/db4$yieldprofmax

# Compare CD and SFA lines
ggplot(data=db4) +
  geom_line(aes(x = nitrogen, y = yieldfitmean), colour="red") +
  geom_line(aes(x = nitrogen, y = yieldfitmeanCD), colour="green") +
  geom_point(aes(x = nitrogen, y = yield)) +
  #geom_point(data = subset(db4, crop.count==1), aes(x = nitrogen, y = yield), colour="red") +
  theme_bw() + labs(title = 'yield response to nitrogen')

# Analyse efficiency scores
# obtain efficiency scores
db4$eff <- as.numeric(efficiencies(sfaCDQ1c)) # as numeric needed otherwise unwanted attributes are added.

# Some plots using log scale to investigate eff
ggplot(data=db4, aes(x = yield, y = eff)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth() + scale_x_continuous(trans = "log")

ggplot(data=db4, aes(x = nitrogen, y = eff)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth() + scale_x_continuous(trans = "log")

# calculate efficiency on basis of fitted value on the frontier
# This is not the same as eff? To investigate why?!!
# There is a perfect relation though, so we probaly miss a log or something
db4$yieldfit <- as.numeric(exp(fitted(sfaCDQ1c)))
db4$eff2 <- db4$yield/db4$yieldfit

ggplot(data=db4) +
  geom_point(aes(x = nitrogen, y = eff)) +
  geom_point(aes(x = nitrogen, y = eff2), colour="red") +
  theme_bw() + labs(title = 'yield response to nitrogen')

ggplot(data=db4) +
  geom_point(aes(x = eff2, y = eff)) +
  theme_bw() + labs(title = 'yield response to nitrogen')

# Add GYGA data
SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)

# Get GYGA map
GYGApath <- "D:\\Dijk158\\Dropbox\\Michiel_research\\Micro_IPOP\\Data\\Spatial\\GYGA"
dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA.Africa<-readOGR(dsn, layer = "CZ_AFRGYGACNTRY")
projection(GYGA.Africa) # check projection
GYGA.Africa <- spTransform(GYGA.Africa, CRS("+proj=longlat +datum=WGS84"))

# Get GYGA
GYGA.TZA.yield.data <- read.xls(paste(GYGApath, "GygaTanzania.xlsx", sep="\\"), , sheet=2)

# Select data for maize
GYGA.TZA.yield.data <- subset(GYGA.TZA.yield.data, CROP=="Rainfed maize")

# Cut out TZA from GYGA map
GYGA.TZA<-GYGA.Africa[GYGA.Africa$REG_NAME=="Tanzania",]

# Link yield gap data
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.

GYGA.TZA.data <- as(GYGA.TZA, "data.frame")
GYGA.TZA.data$id <-row.names(GYGA.TZA.data)
GYGA.TZA.data <- merge(GYGA.TZA.data, GYGA.TZA.yield.data[,c(2:8)], by.x=c("ID"), by.y=c("CLIMATEZONE_ID"), all.x=TRUE, sort=FALSE)
row.names(GYGA.TZA.data)<-GYGA.TZA.data$id
GYGA.TZA <- SpatialPolygonsDataFrame(as(GYGA.TZA, "SpatialPolygons"),
                                     data=GYGA.TZA.data)

# Get y2_hhid-GIS link
LSMSdatapath <- "D:\\Dijk158\\Dropbox\\Michiel_research\\Micro_IPOP" 
HH.geo <- read.dta(paste(LSMSdatapath, "/Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta", sep=""),
                   convert.factors = TRUE)
plot.geo <- read.dta(paste(LSMSdatapath, "/Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/Plot.Geovariables_Y2.dta", sep=""),
                     convert.factors = TRUE)

# Create list of plots, hh, eas and coordinates
geo.base <- left_join(plot.geo, HH.geo) %>%
  transmute(y2_hhid, ea_id, plotnum, lat = lat_modified, lon = lon_modified)

# Create spatial points 
standardproj<-"+proj=longlat +datum=WGS84"
geo.coord <- geo.base %>% 
  dplyr::select(lon, lat) %>%
  SpatialPoints(., proj4string=CRS(standardproj))

# Extract GYGA data and link to GIS coordinates of plots/households/communities
geo.GYGA <- extract(GYGA.TZA, geo.coord) %>% 
  cbind(geo.base,.) %>%
  dplyr::select(y2_hhid, plotnum, YW)

# Merge Yield potential with maize plot database used for analysis
db5 <- left_join(db4, geo.GYGA)

# Add geo location of plot
db5 <- left_join(db5, geo.base)

# A large number of plots have missing YW values because region is not covered by GYGA.
# We assume for the moment that average YW is reasonable proxy for missing information
db5 <- mutate(db5, YW2 = ifelse(is.na(YW), mean(YW, na.rm = TRUE), YW))

# Calculate additional yield and production when moving from actual yield to (1) frontier, (2) profit maximizing, (3) yield potential
# calculate how additional yield when moving to the frontier and becoming efficient
# calculate everything in tons
# Calculate yield gaps
# NB plots that do not use fertilizer are completely disregarded!!
# We could add additional production of plots that move from 0 to profit maximizing nitrogen use

# Calculate different yield gaps
db5 <- mutate(db5, yg3 = yield*100/(YW2*1000),
                  yg2 = yield*100/yieldprofmax,  
                  yg1 = ifelse(yieldfit - yield>0, yield*100/yieldfit, NA),
                  yg1b = eff*100,
                  yg2b = (1/eff)*yield/yieldprofmax,
                  y3b = yieldprofmax/(YW2*1000))

# Calculate additional production
db5 <- mutate(db5, prod = yield * area.gps/1000,
              addyieldeff = ifelse(yieldfit - yield>0, yieldfit - yield, 0),
              addprodeff = addyieldeff * area.gps/1000,
              addyieldprofmax = yieldprofmax - yieldfit,
              addprodprofmax = addyieldprofmax * area.gps/1000,
              addnitrogenprofmax = profitmaxnitrogen - nitrogen,
              addyieldpotential = YW2*1000 - yieldprofmax,
              addprodpotential = addyieldpotential * area.gps/1000
)


#' NB there are a few negative values for addyieldpotential, which means that profitmaximizing yield is higher than
#' potential which should not be possible => check this
#' Note: some plots operate on the right hand side of the profit maximizing yield but before 
#' the point where nitrogen use is maximized. For these plots, less nitrogen means less yield.
#' Other plots might operate past the yield maximizing point (around 125 kg N) and operating 
#' at the profit maximizing point can mean more yield or less yield depending on how much negative
#' marginal returns of nitrogen are involved.
# NB check whether it is possible to upscale production estimates to national representative figures.

###########################
# Graphs for presentation

# Estimated frontier
ggplot(data=db4) +
  geom_line(aes(x = nitrogen, y = yieldfitmean/1000), colour="red") +
  geom_point(aes(x = nitrogen, y = yield/1000)) +
  theme_classic() + 
  scale_x_continuous(limits = c(0, 175)) +
  labs(x = " Nitrogen (kg)" ,y = "Yield (tons)") + 
  geom_point(aes(x=profitmaxnitrogen, y=profitmaxyield/1000), colour="red", size=5) 

# Check abline where profit is maxmized
#geom_abline(slope=pfert/(pmaize*meanyield/1000), intercept=(profitmaxyield/1000)-(pfert/(pmaize*1000)*profitmaxnitrogen))
#profitmaxnitrogen <- ((pfert/(meanyield * pmaize))- anitrogen)/(2*bnitrogen)
#(profitmaxnitrogen * (2*bnitrogen) + anitrogen) * meanyield =

ggsave("graphs\\frontier.png", height = 150, width = 200, type = "cairo-png", units="mm")

# http://www.r-bloggers.com/waterfall-plots-in-r/
# Create database for waterfall plot
wf.df <- data.frame(category = c("Current \n production", "Closing technical \n efficiency gap", "Closing economic \n yield gap (1)", "Closing economic \n yield gap (2)", "Closing technology \n yield gap", "Potential yield\n production"),
                                production = c(round(sum(db5$prod)),
                                                round(sum(db5$addprodeff)),
                                                round(sum(db5$addprodprofmax[db5$addprodprofmax > 0])),
                                                round(sum(db5$addprodprofmax[db5$addprodprofmax < 0])),
                                                round(sum(db5$addprodpotential)),
                                                0))
wf.df$value <- c(wf.df$production[c(1:5)], sum(wf.df$production))
wf.df$share <- wf.df$value/wf.df$value[6]*100
wf.df$sector <- wf.df$category

#'Waterfall plot
waterfall <- function(df, offset=0.3) {
  
  library(ggplot2)
  library(scales)
  library(dplyr)
  
  ## Add the order column to the raw data frame and order appropriately
  df <- df %>% mutate(order=as.numeric(category)) %>% arrange(order)
  
  ## The last value needs to be negated so that it goes down to
  ## zero.  Throws a warning if the cumulative sum doesn't match.
  last.id <- nrow(df)
  df$value[last.id] <- -df$value[last.id]
  
  ## Calculate the cumulative sums
  df <- df %>% mutate(cs1=cumsum(value))
  
  ## Throw a warning if the values don't match zero as expected
  final_value <- tail(df$cs1, 1)
  if (final_value!=0) {
    warning(sprintf("Final value doesn't return to 0.  %.2d instead.", final_value))
  }
  
  ## Calculate the max and mins for each category and sector
  df <- transform(df, min.val=c(0, head(cs1, -1)),
                  max.val=c(head(cs1, -1), 0))    
  df <- df %>% group_by(order, category, sector, value, cs1) %>%
    summarize(min=min(min.val, max.val), max=max(min.val, max.val))
  
  ## Create the lines data frame to link the bars
  lines <- df %>% group_by(order) %>% summarize(cs=max(cs1))
  lines <- with(lines, data.frame(x=head(order, -1),
                                  xend=tail(order, -1),
                                  y=head(cs, -1),
                                  yend=head(cs, -1)))
  
  
  ## Add the offset parameter
  df <- transform(df, offset=offset)
  
  ## Make the plot    
  gg <- ggplot() +
    geom_segment(data=lines, aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed")  +
    geom_rect(data=df, aes(xmin=order - offset,
                           xmax=order + offset, 
                           ymin=min,
                           ymax=max, fill=sector)) +
    scale_x_continuous(breaks=unique(df$order), labels=unique(df$category)) +
    scale_y_continuous(labels = comma)
  
  return(gg)
}

# Create waterfall plot
wf.df$category <- factor(wf.df$category, levels=unique(wf.df$category))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## Determines the spacing between columns in the waterfall chart
offset <- 0.3
gg <- waterfall(wf.df, offset=offset) +
  coord_cartesian(ylim=c(0, 1500)) +
#   scale_fill_manual(guide="none", values=c(rgb(81, 34, 112, max=255),
#                                            rgb(125, 96, 153, max=255),
#                                            rgb(116, 173, 226, max=255),
#                                            rgb(17, 135, 146, max=255),
#                                            rgb(69, 171, 183, max=255),
#                                            rgb(17, 135, 146, max=255))) +
  scale_fill_manual(guide="none", values=cbPalette)+
  #annotate("text", x=6, y=838, label="China", colour="white") +
#   annotate("text", x=2 + offset, y=1000,
#            hjust=1, vjust=1,
#            size=3,
#            label="n = 328 maize plots",
#            fontface="italic") +
   labs(x="", y="Maize production (tons)") +
  theme_classic()
  


print(gg)
ggsave(plot = gg, "graphs\\waterfall.png", height = 150, width = 200, type = "cairo-png", units="mm")

# Distribution of yield gaps
yieldgap <- db5 %>%
  dplyr::select(yg1b, yg2, yg3) %>%
  gather(yieldgap, value, yg1b:yg3)

ggplot(data=subset(yieldgap, value<=100), aes(x=yieldgap, y=value, fill=yieldgap)) +
  geom_boxplot() +
  stat_boxplot(geom ='errorbar') +
  guides(fill=FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  theme_classic() +
  labs(x="", y="Yield gap (%)") +
  scale_x_discrete(breaks=c("yg1b", "yg2", "yg3"), labels=c("Techical efficiency \n gap", "Profit efficiency \n gap", "Potential yield \n gap"))

ggsave("graphs\\yieldgapdistribution.png", height = 150, width = 200, type = "cairo-png", units="mm")

# Maps with GYGA yield potential and plot information
# transform shapefile in dataframe for ggplot. rownames are used as ID for the countries. Using ID gives strange results. 
# Additional data is linked back again
GYGA.TZA.fort<- fortify(GYGA.TZA) 
GYGA.TZA.fort <- merge(GYGA.TZA.fort, GYGA.TZA.data, by="id")
GYGA.TZA.fort$yieldclass<-cut(GYGA.TZA.fort$YW, breaks=c(0,1,2,3,4,5,6,7))
db3b <- left_join(db3, geo.base)
AverageYield <- ddply(db3b,.(ea_id, lon, lat, region), summarize, AverageYield = mean(yield/1000, na.rm=TRUE))
#AverageYield <- ddply(db5,.(ea_id, lon, lat, region), summarize, AverageYield = mean(yield/1000, na.rm=TRUE))
# NB check why kilimanjaro does not show up in graph. Used to be there, or not?

GYGA.p<-ggplot()+
  geom_polygon(data=GYGA.TZA.fort, aes(x=long, y=lat, group=group, fill=yieldclass), colour="black")+
  geom_polygon(data=subset(GYGA.TZA.fort, is.na(YW)), aes(x=long, y=lat, group=group), fill="white", colour="black") +
  scale_fill_discrete(name="Potential water\nlimited yield (tons)") +
  geom_point(data=AverageYield, aes(x=lon, y=lat, size=AverageYield), colour="black")+
  scale_size_continuous(name="Average yield (tons)") +
  coord_equal()+
  labs(x="", y="")+
  theme_classic()+
  theme(legend.key=element_blank())
GYGA.p

ggsave("graphs\\yieldgapmap.png", height = 150, width = 200, type = "cairo-png", units="mm")


