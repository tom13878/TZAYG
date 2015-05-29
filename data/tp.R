# -------------------------------------
# produce a dataframe foe the first stage
# tobit regression of the analysis following
# Liverpool_Tasie et al (2014)
# 
# this is followed by tobit analysis
# the residuals of which will be included
# in a final model
#
# some cleaning will also be required for
# variables like yield and nitrogen
# application. And inflation will also
# need to be considered.
# -------------------------------------

library(haven)
library(stringr)
library(ggplot2)
library(plyr)
detach(package:dplyr)
library(dplyr)


# start by reading in the panel and selecting important variables
panel <- read_dta("M:/TZAYG/data/panel.dta")
panel$y3_hhid <- as_factor(panel$y3_hhid)
panel$plotnum <- as_factor(panel$plotnum)
tp <- select(panel, hhid, plotnum, output_kg_old, output_kg_new, maize_share, crop_count, nitrogen_kg, irrig, org, pest, sex, age, own_sh, rent_sh, area_gps_imputed,
             maize_price, nitrogen_price, year, y3_hhid, fam_lab_days, hir_lab_days, phosphorous_kg, cash_crop)

# 1. need to get monocropping variable
filepath1 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2AGRDTA"
setwd(filepath1)
mono_crop_10 <- read_dta("AG_SEC4A.dta")
mono_crop_10 <- mono_crop_10[mono_crop_10$zaocode %in% 11, c("y2_hhid", "plotnum", "ag4a_04")]
mono_crop_10$mono_crop <- ifelse(mono_crop_10$ag4a_04 %in% 1, 1, 0)
mono_crop_10 <- select(mono_crop_10, -ag4a_04, hhid=y2_hhid, plotnum, mono_crop)

filepath2 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath2)
mono_crop_12 <- read_dta("AG_SEC_4A.dta")
mono_crop_12 <- mono_crop_12[mono_crop_12$zaocode %in% 11, c("y3_hhid", "plotnum", "ag4a_04")]
mono_crop_12$mono_crop <- ifelse(mono_crop_12$ag4a_04 %in% 1, 1, 0)
mono_crop_12 <- select(mono_crop_12, -ag4a_04)

# 2. need to calculate a district level or region level nitrogen price so get
#    region data
region_names <- c("dodoma", "arusha", "kilimanjaro", "tanga", "morogoro", "pwani",
                  "dar es salaam", "lindi", "mtwara", "ruvuma", "iringa", "mbeya",
                  "singida", "tabora", "rukwa", "kigoma", "shinyanga", "kagera",
                  "mwanza", "mara", "manyara", "kaskazini unguja", "kusini unguja",
                  "mjini magharibi", "kaskazini pemba", "kusini pemba")
hhid_region <- read_dta("C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_A.dta")
hhid_region <- select(hhid_region, y2_hhid, region)
hhid_region$region <- factor(hhid_region$region, labels=region_names)
hhid_region <- rename(hhid_region, hhid=y2_hhid)

filepath4 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath4)
comm_link2 <- read_dta("HH_SEC_A.dta")
comm_link2 <- select(comm_link2, y3_hhid, region=hh_a01_2)
comm_link2$region <- factor(comm_link2$region) %>% tolower()

# 3. need to combine the owned and rented capital and deflate year 3
tp$own_sh <- ifelse(tp$year %in% 2010, tp$own_sh*1.16, tp$own_sh)
tp$rent_sh <- ifelse(tp$year %in% 2010, tp$rent_sh*1.16, tp$rent_sh)
tp$own_sh <- ifelse(is.na(tp$own_sh), 0, tp$own_sh)
tp$rent_sh <- ifelse(is.na(tp$rent_sh), 0, tp$rent_sh)
tp$assets <- tp$own_sh + tp$rent_sh

# 4. need to add in the geo information
filepath5 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2GEODTA"
setwd(filepath5)
geo <- read_dta("HH.Geovariables_Y2.dta")
geo <- select(geo, hhid=y2_hhid, dist2market=dist03, dist2HQ=dist05, ann_temp=clim01,
              precip=clim03, elevation=soil01, slope=soil02)


filepath6 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath6)
geo2 <- read_dta("HouseholdGeovars_Y3.dta")
geo2 <- select(geo2, y3_hhid, dist2market=dist03, dist2HQ=dist05, ann_temp=clim01,
               precip=clim03, elevation=soil01, slope=soil02)

# 5. need to get a variable for mechanization
filepath7 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2AGRDTA"
setwd(filepath7)
tool_10 <- read_dta("AG_SEC11.dta")
tool_10$itemcode <- as_factor(tool_10$itemcode) %>% tolower()
tool_10$y2_hhid <- as.character(tool_10$y2_hhid)
tool_10$ag11_01 <- as.integer(tool_10$ag11_01)
tool_10 <- select(tool_10, y2_hhid, itemcode, ag11_01) %>% filter(itemcode %in% "tractor")
tool_10 <- select(tool_10, -itemcode)

filepath8 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath8)
tool_12 <- read_dta("AG_SEC_11.dta")
tool_12$itemid <- as_factor(tool_12$itemid) %>% tolower()
tool_12$y3_hhid <- as.character(tool_12$y3_hhid)
tool_12$ag11_01 <- as.integer(tool_12$ag11_01)
tool_12 <- select(tool_12, y3_hhid, itemid, ag11_01) %>% filter(itemid %in% "tractor")
tool_12 <- select(tool_12, -itemid)

# 6. need to make a year and sex dummy and irrigation, organic fertilizer and pest
#    dummy
tp$sex <- ifelse(tp$sex %in% 1, 1, 0)
tp$y12 <-ifelse(tp$year %in% 2012, 1, 0)
tp$org <- ifelse(tp$org %in% 2, 1, 0)
tp$irrig <- ifelse(tp$irrig %in% 2, 1, 0)
tp$pest <- ifelse(tp$pest %in% 2, 1, 0)

# 9. need to make residency information for instrument
filepath9 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2HH1DTA"
setwd(filepath9)
residency1 <- read_dta("HH_SEC_B.dta")
residency1 <- select(residency1, y2_hhid, indidy2, hh_b05, residency=hh_b25)
residency1 <- subset(residency1, hh_b05 %in% 1) # filter on household head
residency1 <- select(residency1, hhid=y2_hhid,  residency, -indidy2, -hh_b05)
residency1$residency <- as.numeric(residency1$residency)

# but there are also the households that did not have a wave2 household ID 
# these are in sub_fert_wav3 and need to be joined with the wave 3 residency
# information
filepath10 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath10)
residency2 <- read_dta("HH_SEC_B.dta")
residency2 <- select(residency2, y3_hhid, indidy3, hh_b05, residency=hh_b26)
residency2 <- subset(residency2, hh_b05 %in% 1) # filter on household head
residency2 <- select(residency2, y3_hhid,  residency, -indidy3, -hh_b05)

# 10. need to inflate nitrogen and maize prices
tp$nitrogen_price <- ifelse(tp$year %in% 2010, tp$nitrogen_price*1.16, tp$nitrogen_price)
tp$maize_price <- ifelse(tp$year %in% 2010, tp$maize_price*1.16, tp$maize_price)

# 11. need to calculate nitrogen application and get rid of outliers somehow.
#     make all 0s NAs otherwise get infinite values. Then change back
#     also do this for the maize yield and the plot yield

qplot(nitrogen_kg, data=tp)
qplot(output_kg_old, data=tp)
qplot(output_kg_new, data=tp)
tp$nitrogen_kg[tp$nitrogen_kg %in% 0 ] <- NA
tp$area_gps_imputed[tp$area_gps_imputed %in% 0 ] <- NA
tp <- mutate(tp, nitrogen=nitrogen_kg/area_gps_imputed,
             phosph=phosphorous_kg/area_gps_imputed,
             plot_yld=output_kg_new/area_gps_imputed,
             maize_yld=output_kg_old/area_gps_imputed)

tp$nitrogen[tp$nitrogen > 700] <- NA
tp$maize_yld[tp$maize_yld > 6000] <- NA
tp$plot_yld[tp$plot_yld > 6000] <- NA

# winsor function for getting rid of outliers
# winsor5 <- function(var, multiple = 3){
#         if(length(multiple) != 1 || multiple <= 0){
#                 stop('bad choice of multiple')
#         }
#         med <- median(var, na.rm = TRUE)
#         var <- var - med
#         # note scale factor is 1.4826 for normal data assumption
#         sc <- mad(var, center = 0, na.rm = TRUE)* multiple
#         print(sc)
#         var[var > sc] <- sc
#         var <- var + med
#         return( var )
# }

winsor4.1 <- function(var, fraction=0.01){
        if (length(fraction) != 1 || fraction < 0 || fraction > 0.1) {
                stop('bad choice of fraction!!')
        } 
        lim <- quantile(var, probs = 1-fraction, na.rm = TRUE)
        var[var > lim] <- lim
        var
}



tp$nitrogen <- winsor4.1(tp$nitrogen)
tp$plot_yld <- winsor4.1(tp$plot_yld)
tp$maize_yld <- winsor4.1(tp$maize_yld)
tp$nitrogen[is.na(tp$nitrogen)] <- 0
tp <- tp[!(tp$cash_crop %in% 1),]
# tp <- tp[!(tp$area_gps_imputed < 0.01 & tp$area_gps_imputed > 10), ]

# also might as well clean the assets variable while we are at it
# and put together the labour - combine hired and family labour
tp <- mutate(tp, lab=fam_lab_days+hir_lab_days)

# 12. get land holdings information
filepath11 <- "C:/Users/morle001/Dropbox/Micro_IPOP/Data/Plot_size"
setwd(filepath11)
areas_w2 <- read_dta("areas_tza_y2_imputed.dta") %>% select(hhid=case_id, area=area_gps_mi_50)
areas_w2$area <- as.numeric(areas_w2$area)
areas_w2 <- ddply(areas_w2, .(hhid), summarise, area=sum(area, na.rm=TRUE))

areas_w3 <- read.csv("M:/cleaned_data/2012/areas_w3.csv")
areas_w3 <- group_by(areas_w3, y3_hhid) %>% summarise(area=sum(area_gps_imputed, na.rm=TRUE))

# 13. get a urban/rural variable so we only focus on rural tanzania maize farmers.
filepath12 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2HH1DTA"
setwd(filepath12)
rural_w1 <- read_dta("HH_SEC_A.dta") %>% select(hhid=y2_hhid, rural=y2_rural)

filepath13 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath13)
rural_w2 <- read_dta("HH_SEC_A.dta") %>% select(y3_hhid, rural=y3_rural)


# 13. need to make a panel with all the variables here
#     need to split along year lines as well

# select just those household with a wave 3 hhid
tp2 <- tp[str_length(tp$hhid) < 9,]

# and select out the wave3 households with wave 3 hhid from the main panel
wav3 <- grep("-", tp$hhid)
tp <- tp[-wav3,]

# now split the panel into twp pieces depeding on year
tp_10 <- tp[tp$year %in% 2010, ]
tp_12 <- tp[tp$year %in% 2012, ]

tp_10$hhid <- ifelse(str_length(tp_10$hhid) < 16, paste("0", tp_10$hhid, sep=""), tp_10$hhid)
tp_12$hhid <- ifelse(str_length(tp_12$hhid) < 16, paste("0", tp_12$hhid, sep=""), tp_12$hhid)

# first step is to get regions 
tp_10 <- left_join(tp_10, hhid_region)
comm_link2$y3_hhid <-as.character(comm_link2$y3_hhid)
tp_12 <- left_join(tp_12, comm_link2, by="y3_hhid")
tp2 <- left_join(tp2, comm_link2, by=c("hhid"="y3_hhid"))

# merge in the geo information to the three new dataframes
tp_10 <- left_join(tp_10, geo, by="hhid")
tp_12 <- left_join(tp_12, geo2, by="y3_hhid")
tp2 <- left_join(tp2, geo2, by=c("hhid"="y3_hhid"))

# merge in the land holding information
tp_10 <- left_join(tp_10, areas_w2, by="hhid")
tp_12 <- left_join(tp_12, areas_w3, by="y3_hhid")
tp2 <- left_join(tp2, areas_w3, by=c("hhid"="y3_hhid"))

# merge in the mechanization and monocropping variables
tp_10 <- left_join(tp_10, tool_10, by=c("hhid"="y2_hhid"))
tp_10$plotnum <- as.character(tp_10$plotnum)
tp_10 <- left_join(tp_10, mono_crop_10, by=c("hhid", "plotnum"))

tp_12 <- left_join(tp_12, tool_12, by="y3_hhid")
tp_12 <- left_join(tp_12, mono_crop_12, by=c("y3_hhid", "plotnum"))

tp2 <- left_join(tp2, tool_12, by=c("hhid"="y3_hhid"))
mono_crop_12 <- rename(mono_crop_12, hhid=y3_hhid)
tp2 <- left_join(tp2, mono_crop_12, by=c("hhid", "plotnum"))

# add residency information
tp_10 <- left_join(tp_10, residency1)
tp_12 <- left_join(tp_12, residency2, by="y3_hhid")
tp2 <- left_join(tp2, residency2, by=c("hhid"="y3_hhid"))

# add the rural information
tp_10 <- left_join(tp_10, rural_w1)
tp_12 <- left_join(tp_12, rural_w2, by="y3_hhid")
tp2 <- left_join(tp2, rural_w2, by=c("hhid"="y3_hhid"))
# now rbind everything together and remove variables that you do not need
tp_12 <- tp_12[, names(tp_10)]
tp_12$region <- as.factor(tp_12$region)
tp_10$residency <- as.factor(tp_10$residency)
tp_10$y3_hhid <- as.character(tp_10$y3_hhid)
tp2$region <- as.factor(tp2$region)
tp2$y3_hhid <- as.character(tp2$y3_hhid)
tp2$residency <- as.integer(tp2$residency)
tp <- rbind(tp_10, tp_12, tp2)
tp <- select(tp, -plotnum, -nitrogen_kg, -own_sh, -rent_sh, -area_gps_imputed,
             -nitrogen_price, -y3_hhid, -output_kg_new, -output_kg_old,
             -fam_lab_days, -hir_lab_days, -phosphorous_kg, -cash_crop)
tp <- rename(tp, mech=ag11_01)
tp <- mutate(tp, maize_share=maize_share/100)


# replace residency values of 99 with the age of the household head
tp$residency <- ifelse(tp$residency %in% 99, tp$age, tp$residency)
tp$north <- ifelse(tp$region %in% "kilimanjaro", 1, 0)
tp$south <- ifelse(tp$region %in% c("ruvuma", "mbeya", "iringa"), 1, 0)



tp <- select(tp, hhid, year, y12, region, plot_yld, maize_yld, nitrogen, lab, assets, everything())
bad <- grep("-", tp$hhid)
tp <- tp[-bad,]
tp <- tp[tp$rural %in% 1,]

# get rid of everything you do not need
x=ls()
x <-x[!(x=="tp")]
rm(list=x)

# write_dta(tp, "M:/TzAYG/data/tp.dta")
