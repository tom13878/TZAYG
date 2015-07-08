# political info
# aim - demonstrate that the probability of receiving a voucher is greater in 
# wards where the ward is dominated politically by the rulling party of 
# tanzania

library(ggplot2)

# for maps
library(raster)

# for data wrangling 
library(haven)
library(data.table)
library(reshape2)
library(plyr)
detach(package:dplyr)
library(dplyr)

# step 1. - get teh voucher information from the second wave of survey
setwd("M:/TZAYG/data/2010")
vouch <- read_dta("plot_variables_w2.dta") %>% select(y2_hhid, voucher1, voucher2)
vouch <- melt(vouch, id.vars="y2_hhid") %>% unique
vouch <- ddply(vouch, .(y2_hhid), summarise, v=sum(value %in% 1))

# step 2. - get the political information from community survey
fp2 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Stata/TZNPS2COMDTA"
setwd(fp2)

pol <- read_dta("COMSEC_CI.dta") %>% select(region=id_01, district=id_02, ward=id_03, position=cm_i01,
                                            sex=cm_i02, resident=cm_i05, party=cm_i06, village=cm_i07, religion=cm_i10)

pol$position <- as_factor(pol$position)
pol$sex <- as_factor(pol$sex)
pol$party <- as_factor(pol$party)
pol$religion <- ifelse(pol$religion %in% 1, "TDL", ifelse(pol$religion %in% 2, "CHRSTN", ifelse(pol$religion %in% 3, "MSLM", "OTHR")))

# step3. join voucher information with gps 
setwd("N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Stata/TZNPS2GEODTA")
gps <- read_dta("HH.Geovariables_Y2.dta") %>% select(y2_hhid, lon=lon_modified, lat=lat_modified)

vouch <- left_join(gps, vouch)

# step 4. join political information with gps

# a. get the hh info first
filepath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Stata/TZNPS2HH1DTA"
setwd(filepath)        
HHA <- read_dta("HH_SEC_A.dta")
HHA <- unique(select(HHA, clusterid, region, district, ward))

# b. join gps and hh
setwd("N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Stata/TZNPS2COMDTA")
eaoff <- read_dta("TZY2.EA.Offsets.dta") %>% select(clusterid, lon=lon_modified, lat=lat_modified)
ea_hh <- left_join(HHA, eaoff) %>% select(-clusterid)

# c. join gps/hh with the pol data on region, district and ward only
ea_hh_pol <- left_join(pol, ea_hh) %>% na.omit %>% unique

# step 5. get a map of tanzania
tanzania <- getData('GADM', country = "TZA", level = 3)

# step 6. overlay to get the ward names
pol_mat <- cbind(ea_hh_pol$lon, ea_hh_pol$lat)
voucher_mat <- cbind(vouch$lon, vouch$lat)        

llCRS <- CRS(proj4string(tanzania))

sp_pol <- SpatialPoints(pol_mat, llCRS)
sp_vouch <- SpatialPoints(voucher_mat, llCRS)

# overlay selection

sel_pol <- over(sp_pol, tanzania)
sel_vouch <- over(sp_vouch, tanzania)

complete_pol <- cbind(ea_hh_pol, select(sel_pol, NAME_1, NAME_2, NAME_3))
complete_vouch <- cbind(vouch, select(sel_vouch, NAME_1, NAME_2, NAME_3))

# now calculate pol information by ward
by_ward_pol <- group_by(complete_pol, NAME_1, NAME_3) %>%
        summarise(ccm=all(party %in% "CCM"),
                  nobs=n(),
                  nccm=sum(party %in% "CCM"),
                  nothr=sum(!(party %in% "CCM")),
                  nccm_pc=sum(party %in% "CCM")/n(),
                  nothr_pc=sum(!(party %in% "CCM"))/n())

complete_vouch <- select(complete_vouch, -lon, -lat, -NAME_2)

x <-c("by_ward_pol", "complete_vouch")
rm(list=ls()[!(ls() %in% x)])

# join the voucher and pol information together by region. And make a new variable
# that scales for the number of people survesyed in an area
vpol <- left_join(complete_vouch, by_ward_pol)
vpol <- mutate(vpol, intensity=cut(nccm_pc, breaks=seq(0, 1, 0.05)))
vpol <- mutate(vpol, v=ifelse(is.na(v), 0, v))
vpol <- mutate(vpol, voucher=ifelse(!(v %in% 0), 1, 0))

i <- group_by(vpol, NAME_1, intensity) %>% summarise(N=sum(v, na.rm=TRUE)) %>% na.omit()
ggplot(data=i, aes(x=intensity, y=N)) + geom_bar(stat="identity") + facet_wrap(~NAME_1)

# next step - a more formal analysis, logit or probit or tobit model, but what about lots of zeroes?

# read in HH info to get more regressors - note that we are not looking at just maize guys now
setwd("M:/TZAYG/data/")
tp <- read_dta("tp.dta")
tp$frt <- ifelse(tp$nitrogen %in% 0, 0, 1)
tp$zone <- as_factor(tp$zone)

x <- select(tp, hhid, y12, pest, frt, ngn_prc, mech, sex, age, zone, rural, residency) %>% unique()
x2 <- left_join(x, vpol, by=c("hhid"="y2_hhid"))

# the first two models are looking at explaining whether a household
# does or does not receive a voucher based on the "intensity" of CCM
# support in their village. 
myprobit <- glm(voucher ~ nccm_pc + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 ,
                family = binomial(link = "probit"), data = x2)

mylogit <- glm(voucher ~ nccm_pc + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 ,
               family = binomial(link = "logit"), data = x2)

# the second two models look at whether the household receives a 
# voucher based on whether the household is situated in a 
# completely dominated CCM area. 
myprobit2 <- glm(voucher ~ ccm + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 + zone,
                family = binomial(link = "probit"), data = x2)

mylogit2 <- glm(voucher ~ ccm + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 + zone,
               family = binomial(link = "logit"), data = x2)

# also just a simple linear probability model for binary response
mylpm <- lm(voucher ~ nccm_pc + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 + zone, data=x2)
mylpm2 <- lm(voucher ~ ccm + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 + zone, data=x2)

# and finally probit, logit and lpm including ccm as a binary variable
myprobit3 <- glm(voucher ~ nccm_pc + ccm + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 + zone,
                 family = binomial(link = "probit"), data = x2)

mylogit3 <- glm(voucher ~ nccm_pc + ccm + sex + age +pest + frt + ngn_prc + mech + residency + rural + y12 + zone,
                family = binomial(link = "logit"), data = x2)

# evaluate the cumulative distribution function at the averages of the 
# x values PEA or APE for both the logit and probit

PAE <- coef(myprobit)["(Intercept)"] + coef(myprobit)["nccm_pc"]*mean(x2$nccm_pc, na.rm=TRUE) + coef(myprobit)["sex"]*mean(x2$sex, na.rm=TRUE) +
        coef(myprobit)["age"]*mean(x2$age, na.rm=TRUE) + coef(myprobit)["pest"]*mean(x2$pest, na.rm=TRUE)  + coef(myprobit)["frt"]*mean(x2$frt, na.rm=TRUE) +
        coef(myprobit)["ngn_prc"]*mean(x2$ngn_prc, na.rm=TRUE) + coef(myprobit)["mech"]*mean(x2$mech, na.rm=TRUE) + coef(myprobit)["residency"]*mean(x2$residency, na.rm=TRUE) +
        coef(myprobit)["rural"]*mean(x2$rural, na.rm=TRUE) + coef(myprobit)["y12"]*mean(x2$y12, na.rm=TRUE)

APE <- mean(coef(myprobit)["(Intercept)"] + coef(myprobit)["nccm_pc"]*x2$nccm_pc + coef(myprobit)["sex"]*x2$sex +
        coef(myprobit)["age"]*x2$age + coef(myprobit)["pest"]*x2$pest + coef(myprobit)["frt"]*x2$frt +
        coef(myprobit)["ngn_prc"]*x2$ngn_prc + coef(myprobit)["mech"]*x2$mech + coef(myprobit)["residency"]*x2$residency +
        coef(myprobit)["rural"]*x2$rural + coef(myprobit)["y12"]*x2$y12, na.rm=TRUE)

# probit and logit scale factors evaluated for PAE
dnorm(PEA)
dlogis(PEA)

# probit and logit scale factors evaluated for APE
dnorm(APE)
dlogis(APE)

# what about some tests? WALD test, LR or LM test

# still to think about
# 1. how does the probit/logit work when the binary variable voucher is overwhelmingly zero?
# 2. sadly the governance section was not included in the third wave of data. Assume that
#    village structure remains the same.
# 3. Because the NAIVS program should be based on producitivity of the household, we are
#    jumping the gun a little bit by estimating a probit/logit model
# 4. Voucher question may be problematic because farmers are only asked whether they used 
#    a voucher on plot, not how many vouchers they actually received. But wave 3 data goes
#    further with a section specifically on fertilizer. Also could make a case that farmers
#    receive only two vouchers because that is how the NAIVS program is supposed to work