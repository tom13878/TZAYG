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
library(stringr)
library(plyr)
library(dplyr)

# -----------subsidized fert-----------
# calculate the quantity of subsidized 
# fertilizer received by a household

panel <- read_dta("M:/TZAYG/data/panel.dta")
panel$y3_hhid <- as_factor(panel$y3_hhid)

# calculate the quantity of subed fertilizer for each household
sub_fert <- select(panel, hhid, y3_hhid, plotnum, year, voucher1, voucher2)
sub_fert$voucher1 <- as_factor(sub_fert$voucher1)
sub_fert$voucher2 <- as_factor(sub_fert$voucher2)

sub_fert$v1 <- ifelse(sub_fert$voucher1 %in% "YES", TRUE, FALSE)
sub_fert$v2 <- ifelse(sub_fert$voucher2 %in% "YES", TRUE, FALSE)

sub_fert$qsub <- (sub_fert$v1 + sub_fert$v2)*50
sub_fert <- ddply(sub_fert, .(hhid, year), summarise, qsub=sum(qsub))

# in order to construct a panel we need a household identifier across the two 
# years. Most of the time we use the y2_hhid but in some cases a surveyed in
# wave 2 was not surveyed in wave 3. In which case we split the subed fert
# database
wav3 <- grep("-", sub_fert$hhid)
sub_fert_wav3 <- sub_fert[wav3,] 
sub_fert <- sub_fert[-wav3,] # now this one only contains those guys that were surveyed in both years
# add a leading zero to make joins possible with other data frames
sub_fert$hhid <- ifelse(str_length(sub_fert$hhid) < 16, paste("0", sub_fert$hhid, sep=""), sub_fert$hhid)

#--------------geo data----------------
# ------------------wave 2-------------
filepath1 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2GEODTA"
setwd(filepath1)
geo <- read_dta("HH.Geovariables_Y2.dta")
geo <- select(geo, hhid=y2_hhid, dist2market=dist03, dist2HQ=dist05, ann_temp=clim01,
              precip=clim03, elevation=soil01, slope=soil02)

# ---------------wave3-----------------
# looks like a variable for household distance to main
# road is available in year3 even if not available for
# year2
filepath2 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath2)
geo2 <- read_dta("HouseholdGeovars_Y3.dta")
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

# but there are also the households that did not have a wave2 household ID 
# these are in sub_fert_wav3 and need to be joined with the wave 3 residency
# information
setwd(filepath2)
residency2 <- read_dta("HH_SEC_B.dta")
residency2 <- select(residency2, y3_hhid, indidy3, hh_b05, residency=hh_b26)
residency2 <- subset(residency2, hh_b05 %in% 1) # filter on household head
residency2 <- select(residency2, y3_hhid,  residency, -indidy3, -hh_b05)


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
areas_w2$area <- as.numeric(areas_w2$area)
by_hhid_w2 <- ddply(areas_w2, .(hhid), summarise, land_area=sum(area, na.rm=TRUE))

# ------------wave3--------------------
areas_w3 <- read.csv("M:/cleaned_data/2012/areas_w3.csv")
by_hhid_w3 <- group_by(areas_w3, y3_hhid) %>% summarise(land_area=sum(area_gps_imputed, na.rm=TRUE))

# -------------household data -----------
# get data on household head age and sex
# from the tanzania panel. Following 
# ricker-gilbert we want the wave 2 info
# except when we a household was not 
# surveyed in both years
HH_info <- select(panel, hhid, y3_hhid, year, age, sex) %>% unique()
HH_info$sex <- as_factor(HH_info$sex)

HH_info1 <- select(HH_info, hhid, age, sex) %>% filter(str_length(hhid) > 8)
HH_info1$hhid <- ifelse(str_length(HH_info1$hhid) < 16, paste("0", HH_info1$hhid, sep=""), HH_info1$hhid)
table(sub_fert$hhid %in% HH_info1$hhid)

HH_info2 <- select(HH_info, hhid, age, sex) %>% filter(str_length(hhid) <= 9) 
table(sub_fert_wav3$hhid %in% HH_info$hhid) # all TRUE!

# ----------- region data -------------

filepath5 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2COMDTA"
setwd(filepath5)
comm_link <- read_dta("y2commlink.dta")
comm_link <- select(comm_link, hhid = y2_hhid, region=id_01)
comm_link$region <- as_factor(comm_link$region) %>% tolower()
# not all households have a region but apparently that is OK because all
# households in the sub-fert dataframe have a region
all(sub_fert$hhid %in% comm_link$hhid) 

setwd(filepath2)
comm_link2 <- read_dta("HH_SEC_A.dta")
comm_link2 <- select(comm_link2, y3_hhid, region=hh_a01_2)
comm_link2$region <- factor(comm_link2$region) %>% tolower()

# spelling of regions across years is probably different just to make things
# more difficult.
table(unique(comm_link$region) %in% unique(comm_link2$region))
tableunique(comm_link2$region) %in% unique(comm_link$region)) 

# ----------- panel key ---------------
# need to give every dataframe 
# indexed by a wave 3 hhid a wave 2 hhid
# we can do this with the panel_key file 
# but need to be careful with losing households.
# Just now focus only on those households in
# wave 3 with a year 2 hhid. 
setwd(filepath2)
panel_key <- read_dta("NPSY3.PANEL.KEY.dta")
panel_key <- unique(select(panel_key, y3_hhid, y2_hhid))
panel_key$y2_hhid <- zap_empty(panel_key$y2_hhid)
panel_key$y3_hhid <- zap_empty(panel_key$y3_hhid)
panel_key <- (panel_key[!(is.na(panel_key$y2_hhid) & is.na(panel_key$y3_hhid)),])
# panel_key$y2_hhid[is.na(panel_key$y2_hhid)] <- panel_key$y3_hhid[is.na(panel_key$y2_hhid)]

# check that every household in the geo data has
# an entry in the panel_key
all(geo2$y3_hhid %in% panel_key$y3_hhid) # TRUE

# join the geo information with a y2_hhid
geo2 <- left_join(geo2, panel_key)
# Lots of missing ones - not impossible
table(is.na(geo2$y2_hhid))
# # drop the NAs
# geo2 <- filter(geo2, !is.na(y2_hhid))
geo2 <- select(geo2, y3_hhid, hhid=y2_hhid, everything())

# and the same for ldn holdings in wave 3
all(by_hhid_w3$y3_hhid %in% panel_key$y3_hhid) # TRUE
by_hhid_w3 <- left_join(by_hhid_w3, panel_key)
table(is.na(by_hhid_w3$y2_hhid))

# by_hhid_w3 <- filter(by_hhid_w3, !is.na(y2_hhid))
by_hhid_w3 <- unique(select(by_hhid_w3, y3_hhid, hhid=y2_hhid, everything()))

################################################################################
# now join everything together with the subsidized fertilizer as the basic unit
# first remove the single NA value for hhid in sub-fert. And also think about
# what varies over time and what does not. following Ricker-Gilbert et al (2011)
# we only want the age  and residency of the household in the first year. This will
# look a bit strange because it will seem as though the household head has not aged
# although two years have passed. Allow y3_hhids that do not have year2 counterpart
# and try pooling everything together. We want to use the most information possible
# these are the guys that we have information on in wave 2

# steps to join information together:
# 1. join sub_fert with household info
tp <- left_join(sub_fert, HH_info1)

# 2. join sub_fert_wav3 with household info 2
tp2 <- left_join(sub_fert_wav3, HH_info2)

# 3. split households by year and join with geo information
tp_10 <- subset(tp, year %in% 2010)
tp_12 <- subset(tp, year %in% 2012)

tp_10 <- left_join(tp_10, geo, by="hhid")
tp_12 <- left_join(tp_12, select(geo2, -y3_hhid, everything()), by="hhid")
tp2 <- left_join(tp2, unique(select(geo2, -hhid, everything())), by=c("hhid"="y3_hhid"))

# 4. join tp_10 and tp_12 with area information
tp_10 <- left_join(tp_10, by_hhid_w2)
tp_12 <- left_join(tp_12, unique(select(by_hhid_w3, -y3_hhid, everything())))

# increase in number of rows for tp_12 but this is because of households
# that have split and so have the same wave2 hhid. 
check <- group_by(tp_12, hhid) %>% summarise(N=n())
nrow(check[check$N>1, ]) # 429

# 5. Join tp2 with area measuements 
tp2 <- left_join(tp2, unique(select(by_hhid_w3, -hhid, everything())), by=c("hhid"="y3_hhid"))

# 6. bind tp_10 and tp_12 back together and then add residency information
tp <- rbind(tp_10, tp_12)
table(tp$hhid %in% residency1$hhid) # TRUE
tp <- left_join(tp, residency1)

# 5. join tp2 with residency information from wave3
tp2 <- left_join(tp2, residency2, by=c("hhid"="y3_hhid"))

# 5. bind tp and tp2 together
tp <- rbind(tp, tp2)

# 7. where residency is equal to 99 replace with age of household head
tp$residency <- ifelse(tp$residency %in% 99, tp$age, tp$residency)
tp$y12 <- ifelse(tp$year %in% 2012, 1, 0)

#--------- Analysis -------------------
library(AER)

# pooled tobit model with year dummies
t1 <- tobit(qsub ~ residency + y12 + age + sex + land_area + ann_temp +
                    precip + dist2market + dist2HQ + slope + elevation, data=tp)


# add in averages of time varying variables for CRE model
tp <- ddply(tp, .(hhid), transform, land_area_hat=mean(land_area, na.rm=TRUE),
            precip_hat=mean(precip, na.rm=TRUE),
            ann_temp_hat=mean(ann_temp, na.rm=TRUE))

# tobit model including time varying variables with averages
t2 <- tobit(qsub ~ residency + y12 + age + sex + land_area + ann_temp +
                    precip + dist2market + dist2HQ + slope + elevation +
                    land_area_hat + precip_hat + ann_temp_hat, data=tp)
