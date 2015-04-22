# -------------------------------------
#' Information on the prices that farmers
#' received for their crops can be found
#' in wave 2 agricultural questionnaire
#' section 5
#' 
#' There is not a direct market producer
#' price, but we do know the quantity of
#' a good that a farmer sold and the price
#' he got for the full amount. This can be
#' used to construict a full series.
# -------------------------------------

setwd( "c:/Users/morle001/Dropbox/Micro_IPOP" )
library( haven )

# read in the price data
price_data <- read_dta( "./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC5A.dta")

# length( unique( price_data$y2_hhid ) ) # total number of households in price data
# length( unique( price_data$zaocode ) ) # number of different crops sold: exactly 50
# class( price_data$zaocode ) # labelled!

# select out only maize with a zaocode of 11 and also when the crop is actually sold
maize_price <- subset(price_data, zaocode==11 & ag5a_01==1) %>%
        select(y2_hhid:ag5a_19)

# focus only on the first customer and check the variances of the 
# prices between the different groups
maize_price2 <- transmute(maize_price, y2_hhid, ag5a_04_1, unit_price=ag5a_06/ag5a_05)
ggplot(maize_price2) + geom_histogram(aes(x=unit_price)) + facet_wrap(~ag5a_04_1)

# does not seem to be a huge difference between groups
ggplot(maize_price2) + geom_point(aes(x=seq(1, nrow(maize_price2), by=1), y= unit_price, size=ag5a_04_1))

# might also be good to then split by region as well. And see if some regions
# rely on different networks than others. 

# here I look at the second customer and what price they got for their maize
maize_price3 <- transmute(maize_price, y2_hhid, ag5a_04_2, unit_price2=ag5a_11/ag5a_10)
summary(maize_price3)

# now consider the prices split down by regions
HQSECA <- read.dta('C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_A.dta', convert.factors = TRUE) 
y2commlink <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2COMDTA/y2commlink.dta", convert.factors = TRUE)

y2_hhid.region <- unique(select(HQSECA, y2_hhid, region)) %>% 
        mutate(region = factor(region, labels = levels(factor(y2commlink$id_01))))

maize_price2_region <- left_join(maize_price2, y2_hhid.region)

# now check the split by region first by size
ggplot(maize_price2_region) + geom_point(aes(x=seq(1, nrow(maize_price2), by=1), y= unit_price, size=ag5a_04_1)) + facet_wrap(~region, scales='free')

# then by colour
ggplot(maize_price2_region) + geom_point(aes(x=seq(1, nrow(maize_price2), by=1), y= unit_price, colour=ag5a_04_1, size=ag5a_04_1)) + facet_wrap(~region, scales='free')

# does not seem to be a clear pattern

# now try just splitting by region and doing a histogram
ggplot(maize_price2_region) + geom_histogram(aes(x=unit_price)) + facet_wrap(~region, scales='free')

# make a table splitting down the prices by region and calculating means
# and standard deviation and number of obs
by_region <- group_by(maize_price2_region, region) %>% summarise(obs = n(),
                                                                 avg=mean(unit_price, na.rm=TRUE),
                                                                 sd=sd(unit_price, na.rm=TRUE))

# what about substantial diffrences between the price paid to
# second and first customer. Look for large deviations
maize_price4 <- left_join(maize_price2_region, maize_price3)
maize_price4$dev <- ifelse(is.na(maize_price4$unit_price2), NA, maize_price4$unit_price - maize_price4$unit_price2 )
head(maize_price4)
summary(maize_price4)

# look at deviations between the unit price given to customer 1 and customer 2 
maize_price4[!is.na(maize_price4$unit_price2),] # there are some strange deviations that don't make sense
# deviations suggest care should be taken when combining the prices because a region like Tanga
# could end up with strange results. 

# combine the prices paid to the consumers by taking the average between the two

test <- maize_price4[!is.na(maize_price4$unit_price2),]
good <- test$dev %in% c(-100:100) | is.na(test$dev)
test$unit_price2[!good] <- NA

test$comp <- (test$unit_price + test$unit_price2)/2
test2 <- left_join(maize_price4, select(test, y2_hhid, comp))
test2$compcomp <- ifelse(is.na(test2$comp), test2$unit_price, test2$comp)



by_region2 <- group_by(test2, region) %>% summarise(obs = n(),
                                                    avg=mean(compcomp, na.rm=TRUE),
                                                    sd=sd(compcomp, na.rm=TRUE))

summarise(test2, mean(compcomp, na.rm=TRUE)) # average producer price for the whole of the country is 297.5
