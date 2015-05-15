# -------------------------------------
#' first attempt at estimating a probit model
#' with the Tanzania data - following broadly 
#' Ricker-Gilbert et-al (2011)
#' 
#' This analysis is performed at the household,
#' not the plot level
# -------------------------------------

library(haven)
library(ggplot2)
library(dplyr)

# start by grabbing the geo data for the households
# from waves 2 and three. Presumably this will be the 
# the same in both waves at least some of the time
# ------------------wave 2-------------
filepath1 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2GEODTA"
setwd(filepath1)
geo <- read_dta("HH.Geovariables_Y2.dta")
geo <- select(geo, hhid=y2_hhid, dist2market=dist03, dist2HQ=dist05, ann_temp=clim01,
              precip=clim03, elevation=soil01, slope=soil02)
geo$hhid <- as.character(as.numeric(geo$hhid))

# ---------------wave3-----------------
# looks like a variable for household distance to main
# road is available in year3 even if not available for
# year2
filepath2 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath2)
geo2 <- read_spss("HouseholdGeovars_Y3.sav")
geo2 <- select(geo2, y3_hhid, dist2market=dist03, dist2HQ=dist05, ann_temp=clim01,
               precip=clim03, elevation=soil01, slope=soil02)



# -----------------panel_key problems --------------------
# look out for duplicates - there are households in wave 2 
# that split-up to form more than one household in wave 3
# Additionally there are households in wave 2 that do not 
# have a corresponding wave 3 hhid because they were not sampled
# and some wave 1 households were sampled in wave 3 but not in
# wave 2. For this analysis, what is important is that for every 
# wave 3 household we have a corresponding wave 2 household id. 
# but this can probably be relaxed after reading up on
# CRE models with unbalanced panels.

# first count how many missing values we have in each year
# panel_key <- read_spss("NPSY3.PANEL.KEY.SAV")
# panel_key <- unique(select(panel_key, y1_hhid, y2_hhid, y3_hhid))
# panel_key$y1_hhid <- zap_empty(panel_key$y1_hhid)
# panel_key$y2_hhid <- zap_empty(panel_key$y2_hhid)
# panel_key$y3_hhid <- zap_empty(panel_key$y3_hhid)
# table(is.na(panel_key$y1_hhid)); table(is.na(panel_key$y2_hhid)); table(is.na(panel_key$y3_hhid))

# -----------subsidized fert-----------
# calculate the quantity of subsidized 
# fertilizer received by a household

panel <- read.csv("M:/TZAYG/data/panel.csv")
panel$hhid <- as.character(panel$hhid)

# calculate the quantity of subed fertilizer
sub_fert <- select(panel, hhid, y3_hhid, plotnum, year, voucher1, voucher2)
sub_fert$v1 <- ifelse(sub_fert$voucher1 %in% "YES", TRUE, FALSE)
sub_fert$v2 <- ifelse(sub_fert$voucher2 %in% "YES", TRUE, FALSE)
sub_fert$qsub <- (sub_fert$v1 + sub_fert$v2)*50
sub_fert <- group_by(sub_fert, hhid, year) %>% summarise(qsub=sum(qsub))

# -----------residency----------------
# Our measure of social capital is the 
# number of years that the household 
# head has lived in the community. We
# expect this to be correlated with the
# total quantity of subsidized fertilizer
# received because the longer the household
# head has lived in the commuity the
# longer they have had to stew in the soup
# of village corruption

# -------------wave2-------------------
# the variable hh_b25 is the number of years that the household member
# has been resident in the community
filepath3 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2HH1DTA"
setwd(filepath3)
residency1 <- read_dta("HH_SEC_B.dta")
residency1 <- select(residency1, y2_hhid, indidy2, hh_b05, residency=hh_b25)
residency1 <- subset(residency1, hh_b05 %in% 1) # filter on household head
residency1 <- select(residency1, hhid=y2_hhid,  residency, -indidy2, -hh_b05)
residency1$residency <- as.numeric(residency1$residency)

# # ------------wave 3-------------------
# in the Ricker-Gilbert (2011) paper residency is 
# defined as years resident in the first wave of the
# survey
# # the variable hh_b25 is the number of years that the household member
# # has been resident in the community
# setwd(filepath2)
# residency2 <- read_spss("HH_SEC_B.SAV")
# residency2 <- select(residency2, y3_hhid, indidy3, hh_b05, residency=hh_b26)
# residency2 <- subset(residency2, hh_b05 %in% 1) # filter on household head

# ----------land holdings--------------
# calculate the land holdings of the farmer
# note that here you are actually looking
# only at the farmers plots for maize. finding
# all of the plotsize would require more info
# for the third wave, or a change in how you
# calculate the area measurments. Work with what
# you have and hope for the best

# --------------wave 2-----------------
filepath4 <- "C:/Users/morle001/Dropbox/Micro_IPOP/Data/Plot_size"
setwd(filepath4)
areas_w2 <- read_dta("areas_tza_y2_imputed.dta") %>% select(hhid=case_id, area=area_gps_mi_50)
areas_w2$hhid <- as.character(as.numeric(areas_w2$hhid))
areas_w2$area <- as.numeric(areas_w2$area)
by_hhid_w2 <- group_by(areas_w2, hhid) %>% summarise(land_area=sum(area, na.rm=TRUE))

# ------------wave3--------------------
areas_w3 <- read.csv("M:/cleaned_data/2012/areas_w3.csv")
by_hhid_w3 <- group_by(areas_w3, y3_hhid) %>% summarise(land_area=sum(area_gps_imputed, na.rm=TRUE))

# ---------------other data -----------
# get data on household head age and sex
# from the tanzania panel. Will also need
# info on regions. 
HH_info <- select(panel, hhid, year, age, sex) %>% unique()
HH_info1 <- filter(HH_info, year==2010) %>% select(hhid, age, sex)


# get comm_link info - use the wave 2 regions, because we should have
# wave 2 id's for everything
# filepath5 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2COMDTA"
# setwd(filepath3)
# comm_link <- read_dta("y2commlink.dta")
# comm_link <- select(comm_link, hhid = y2_hhid, region=id_01)
# comm_link$hhid <- as.character(as.numeric(comm_link$hhid))
# comm_link$region <- as_factor(comm_link$region)
# table(tb$hhid %in% comm_link$hhid)

# --------put everything together------
# step 1 is to get rid of any Ghost
# households ---- CREEPY!!!
geo$hhid <- as.character(as.numeric(geo$hhid))
# geo2$y3_hhid <- as.character(as.numeric(geo2$y3_hhid))
panel_key$y2_hhid <- as.character(as.numeric(panel_key$y2_hhid))
# panel_key$y3_hhid <- as.character(as.numeric(panel_key$y3_hhid))
residency1$y2_hhid <- as.character(as.numeric(residency1$y2_hhid))
# residency2$y3_hhid <- as.character(as.numeric(residency2$y3_hhid))

# need to give every dataframe 
# indexed by a wave 3 hhid a wave 2 hhid
# we can do this with the panel_key file 
# but need to be careful with losing households.
# Just now focus only on those households in
# wave 3 with a year 2 hhid. 
setwd(filepath2)
panel_key <- read_spss("NPSY3.PANEL.KEY.SAV")
panel_key <- unique(select(panel_key, y3_hhid, y2_hhid))
panel_key$y2_hhid <- zap_empty(panel_key$y2_hhid)
panel_key$y2_hhid[is.na(panel_key$y2_hhid)] <- panel_key$y3_hhid[is.na(panel_key$y2_hhid)]
# check that every household in the geo data has
# an entry in the panel_key
all(geo2$y3_hhid %in% panel_key$y3_hhid) # TRUE

# join the geo information with a y2_hhid
geo2 <- left_join(geo2, panel_key)
# Lots of missing ones - not impossible
table(is.na(geo2$y2_hhid))
# # drop the NAs
# geo2 <- filter(geo2, !is.na(y2_hhid))
geo2 <- select(geo2, -y3_hhid, hhid=y2_hhid, everything())


# do the same for the residency wave3 dataframe
all(residency2$y3_hhid %in% panel_key$y3_hhid) # TRUE
residency2 <- left_join(residency2, panel_key)
table(is.na(residency2$y2_hhid))
residency2 <- filter(residency2, !is.na(y2_hhid))
residency2 <- select(residency2, -y3_hhid, hhid=y2_hhid, everything())

# and the same for ldn holdings in wave 3
all(by_hhid_w3$y3_hhid %in% panel_key$y3_hhid) # TRUE
by_hhid_w3 <- left_join(by_hhid_w3, panel_key)
table(is.na(by_hhid_w3$y2_hhid))

# by_hhid_w3 <- filter(by_hhid_w3, !is.na(y2_hhid))
by_hhid_w3 <- select(by_hhid_w3, -y3_hhid, hhid=y2_hhid, everything())

# now make some year variables for everything apart
# from the HH_info dataframe and the sub_fert
# variables
geo$year <- 2010
geo2$year <- 2012
by_hhid_w2$year <- 2010
by_hhid_w3$year <- 2012


# now rbind the pairs of geo and residency dataframes
geo_tot <- rbind(geo, geo2)

# select out unecessary variables inresidency
# residency1 <- select(residency1, hhid=y2_hhid, year, residency, -indidy2, -hh_b05)
# residency2 <- select(residency2, hhid, year, residency)
# residency_tot <- rbind(residency1, residency2)

# total land holdings
land_hold <- rbind(by_hhid_w2, by_hhid_w3)

################################################################################
# now join everything together with the subsidized fertilizer as the basic unit
# first remove the single NA value for hhid in sub-fert. And also think about
# what varies over time and what does not. follwoing Ricker-Gilbert et al (2011)
# we only want the age  and residency of the household in the first year. This will
# look a bit strange because it will seem as though the household head has not aged
# although two years have passed. Allow y3_hhids that do not have year2 counterpart
# and try pooling everything together. We want to use the most information possible
# these are the guys that we have information on in wave 2
tp <- left_join(sub_fert, residency1)
tp <- left_join(tp, HH_info1)

# the guys we want to merge together any connections between years but any that
# are left over just add as new lines with their y3_hhid as the identofication number
tp <- left_join(tp, land_hold)
tp <- left_join(tp, geo_tot)

# this isn't great but try it anyway
tp$residency <- ifelse(tp$residency %in% 99, tp$age, tp$residency)
tp$wave3 <- ifelse(tp$year==2012, 1, 0)
tp <- select(tp, everything(), -year)

# add averages for CRE model

# pooled tobit results are not bad but need to do a CRE model to control
# for unobseraved heterogeneity
t2 <- tobit(qsub ~ residency, data=tp)
t1 <- tobit(qsub ~ residency + wave3 + age + sex + land_area + ann_temp +
                    precip + dist2market + dist2HQ + slope + elevation, data=tp)
# OK so the Ricker -Gilbert paper says that we should be including the 
# average of the time varying variables in the tobit model. try and do this by using
# the whole panel and include year effects
################################################################################
# Actual Analysis
# do a pooled tobit with year effects dummies

# do a CRE tobit with year