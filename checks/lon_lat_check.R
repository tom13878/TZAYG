# relationship between year one and year two household ID's
# testing whether we can use the same GPS coordinates
# recall that there is a random offset. Is this the same across
# years?

setwd('C:/locris')

library(foreign)
library(dplyr)
library(plyr)

geo.vars2 <- read.dta('C:/Users/Tomas/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta',
                     convert.factors = TRUE)
HHy2 <- read.dta('C:/Users/Tomas/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_B.dta',
                   convert.factors = TRUE)

length(unique(HHy2$hhid_2008)) # 3168 unique households 

# get dataframe with unique hhid's across both years plus longitude and latitude
IDs <- HHy2[, c('y2_hhid', 'hhid_2008')]
IDs <- unique(IDs)
geo2 <- geo.vars2[, c('y2_hhid', 'lon_modified', 'lat_modified')]
log <- IDs$y2_hhid %in% geo2$y2_hhid
table(log) # 7 households which do not have geo coordinates

hh.geo <- merge(IDs, geo2, by = 'y2_hhid')
names(hh.geo)[3:4] <- c('lon', 'lat')
with(hh.geo, plot(lon, lat))

# compare with year 1 to see which geo vars are the same and which are different
geo.vars1 <- read.dta('C:/Users/Tomas/Dropbox/Micro_IPOP/Data/Tanzania/2008_09/Stata/TZNPS1_consdta/HH.Geovariables_Y1.dta',
                     convert.factors = TRUE)
all(geo.vars1$hhid %in% hh.geo$hhid_2008)
log2 <- geo.vars1$hhid %in% hh.geo$hhid_2008
table(log2) # 84 households in year 1 data not in year 2 data
geo1 <- geo.vars1[, c('hhid', 'lon_modified', 'lat_modified')][log2, ]
names(geo1)[2:3] <- c('lon', 'lat')

# compare the two dataframes to check that the GPS coordinates are much the same
# start by picking a single household
# TODO {tom morley}: check that the household geo coordinates across the two years are
# genuinely the same
subset(hh.geo, hhid_2008 == "01010140020171")
subset(geo1, hhid == '01010140020171') # identical in this case between the two years

# merge all geo1 with hh.geo on all three variables. check the number of rows of this df
names(hh.geo)[2] <- 'hhid'
test.join <- inner_join(geo1, hh.geo[, c('hhid', 'lon', 'lat')])
subset(geo1, hhid == '55020180210078')
test.ply <- ddply(test.join, .(hhid), summarize, count = length(hhid))
weird <- subset(test.ply, count == 2) # 424 observations have been duplicated
                                      # visual inspection confirms they are identical

# save file which contains longitude and latitude for year 1, for every household recorded in
# year 2.
write.csv(hh.geo, 'C:/Users/Tomas/Dropbox/Micro_IPOP/Analysis/Cleaned_data/lon_lat.csv')


# TODO {tom morley}: need to find out which households are missing from year 1 but are present
# in year 2. in particular are there some regions or zones that are missing?


