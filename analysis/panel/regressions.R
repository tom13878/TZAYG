# regressions
options(scipen=999)

library(broom)
library(haven)
library(data.table)
library(plyr)
detach(package:dplyr)
library(dplyr)

# read in the tp database and get labels for zones and region
tp <- read_dta("M:/TZAYG/data/tp.dta") %>% data.table
tp$region <- as_factor(tp$region)
tp$zone <- as_factor(tp$zone)


# add the averages of all time varying variables for the CRE spec
tp <- ddply(tp, .(hhid), transform, lab_bar=mean(lab, na.rm=TRUE), lassets_bar=mean(lassets, na.rm=TRUE),
            nitrogen_bar=mean(nitrogen, na.rm=TRUE), phosph_bar=mean(phosph, na.rm=TRUE),
            area_bar=mean(area, na.rm=TRUE), crop_count_bar=mean(crop_count, na.rm=TRUE)) %>% data.table


# add in the AEZ from geovariables file and look at Michiel's code
filepath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2010/Stata/TZNPS2GEODTA"
setwd(filepath)
geo <- read_dta("HH.Geovariables_Y2.dta") %>% data.table %>% select(y2_hhid, AEZ=land03)
geo$AEZ <- as.integer(geo$AEZ)

# get the names of these AEZ from Michiel's external file
AEZ_names <- read_dta("M:/TZAYG/AEZ_labels.dta") %>% select(AEZ=AEZ16Code, name=AEZ16Name_short) %>% data.table
AEZ_names$name <- as_factor(AEZ_names$name)
geo <- left_join(geo, AEZ_names)

# NA values correspond to the zanzibar islands so they can go
geo <- na.omit(geo) %>% select(hhid=y2_hhid, AEZ=name)
table(tp$hhid %in% geo$hhid)
geo$AEZ <- as.factor(as.character(geo$AEZ))
tp <- left_join(tp, geo)

# have a quick look at the break down of regions and AEZ by year
tp$zone <- as.factor(as.character(tp$zone))
tst <- group_by(tp, AEZ, zone, year) %>% summarise(N=n())
xtabs(N ~ AEZ + zone, data= tst)
ftable(xtabs(N ~ AEZ + zone + year, data= tst))
             
# read the fert and maize price data for solving for economic optimal - remember to inflate the maize
# prices for wave 2 - also need to winsor off some nitrogen price values and make 0's NAs
winsor4.1 <- function(var, fraction=0.01){
        if (length(fraction) != 1 || fraction < 0 || fraction > 0.1) {
                stop('bad choice of fraction!!')
        } 
        lim <- quantile(var, probs = 1-fraction, na.rm = TRUE)
        var[var > lim] <- lim
        var
}
tp$ngn_prc[tp$ngn_prc %in% 0] <- NA 
tp$ngn_prc <- winsor4.1(tp$ngn_prc, 0.05)
plot(tp$ngn_prc, col=tp$zone)

tp <- ddply(tp, .(zone), transform, ngn_prc_zone=mean(ngn_prc, na.rm=TRUE)) %>% data.table


# for the wave 2 prices there are no market prices for some regions. In this case use the non-market prices
maize_prices_w2 <- read_dta("M:/TZAYG/data/2010/producer_prices_w2.dta") %>% data.table %>% filter(x %in% c("market", "non-market")) %>% mutate(mze_prc=avg_price*1.16, year=2010)
maize_prices_w3 <- read_dta("M:/TZAYG/data/2012/producer_prices_w3.dta") %>% data.table %>% filter(x %in% "market") %>% transmute(region, mze_prc=avg_price, year=2012)

maize_prices_w2$region <- as_factor(maize_prices_w2$region)
maize_prices_w3$region <- as_factor(maize_prices_w3$region)

maize_prices_w2.1 <- filter(maize_prices_w2, x %in% "market")
maize_prices_w2.2 <- filter(maize_prices_w2, x %in% "non-market") %>% filter(!(.$region %in% maize_prices_w2.1$region))

mze_prc <- rbind(maize_prices_w2.1, maize_prices_w2.2) %>% select(region, mze_prc, year) %>% rbind(maize_prices_w3) %>% na.omit

# join maize prices with the panel
tp <- left_join(tp, mze_prc)
tp <- ddply(tp, .(zone, year), transform, ngn_prc_zone=mean(ngn_prc, na.rm=TRUE),
            mze_prc_zone=mean(mze_prc, na.rm=TRUE)) %>% data.table

tp <- ddply(tp, .(AEZ, year), transform, ngn_prc_AEZ=mean(ngn_prc, na.rm=TRUE),
            mze_prc_AEZ=mean(mze_prc, na.rm=TRUE)) %>% data.table

tp <- mutate(tp, rto_zone=mze_prc_zone/ngn_prc_zone, rto_AEZ=mze_prc_AEZ/ngn_prc_AEZ)

prc_rto_zone <- group_by(tp, zone, year) %>% summarise(rto_zone=mean(rto_zone))
prc_rto_AEZ <- group_by(tp, AEZ, year) %>% summarise(rto_AEZ=mean(rto_AEZ))
rm(geo, AEZ_names, maize_prices_w2, maize_prices_w3, mze_prc, tst)

# functions for MAE, AME, MVCR (maybe??)
MAE <- function(beta1, beta2, v, ...){
        return(beta1 + 2*beta2*(mean(v, na.rm=TRUE)))
}

AME <- function(beta1, beta2, v, ...){
        return( mean(beta1 + 2*beta2*v, na.rm=TRUE))
}

MVCR <- function(ratio, v){
        return(v*ratio)
}

AVCR <- function(ratio, v){
        return(v*ratio)
}

optimal <- function(ratio=0, beta1, beta2){
        return((ratio-beta1)/(2*beta2))
}

# look again at some regressions and calculate the optimals, MVCR's etc etc
q <- lm(maize_yld ~ nitrogen:zone + nitrogen2:zone + nitrogen:phosph:zone + nitrogen:precip:zone +
                lab + lab2 + lassets + lassets2 + slope + irrig + elevation + mech + rural + crop_count +
                mono_crop + dist2market + lab_bar + lassets_bar + nitrogen_bar+ y12, data=tp)

MAE_SH <- coef(q)["nitrogen:zoneSouthern Highlands"] + 2*coef(q)["zoneSouthern Highlands:nitrogen2"]*(mean(tp$nitrogen, na.rm=TRUE)) +
        coef(q)["nitrogen:zoneSouthern Highlands:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
        coef(q)["nitrogen:zoneSouthern Highlands:precip"]*(mean(tp$precip, na.rm=TRUE))

MAE_S <- coef(q)["nitrogen:zoneSouthern"] + 2*coef(q)["zoneSouthern:nitrogen2"]*(mean(tp$nitrogen, na.rm=TRUE)) +
        coef(q)["nitrogen:zoneSouthern:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
        coef(q)["nitrogen:zoneSouthern:precip"]*(mean(tp$precip, na.rm=TRUE))

MAE_N <- coef(q)["nitrogen:zoneNorthern"] + 2*coef(q)["zoneNorthern:nitrogen2"]*(mean(tp$nitrogen, na.rm=TRUE)) +
        coef(q)["nitrogen:zoneNorthern:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
        coef(q)["nitrogen:zoneNorthern:precip"]*(mean(tp$precip, na.rm=TRUE))

AME_SH <- with(tp, mean(coef(q)["nitrogen:zoneSouthern Highlands"] + 2*coef(q)["zoneSouthern Highlands:nitrogen2"]*nitrogen +
                                coef(q)["nitrogen:zoneSouthern Highlands:phosph"]*phosph +
                                coef(q)["nitrogen:zoneSouthern Highlands:precip"]*precip, na.rm=TRUE))

AME_S <- with(tp, mean(coef(q)["nitrogen:zoneSouthern"] + 2*coef(q)["zoneSouthern:nitrogen2"]*nitrogen +
                                coef(q)["nitrogen:zoneSouthern:phosph"]*phosph +
                                coef(q)["nitrogen:zoneSouthern:precip"]*precip, na.rm=TRUE))

AME_N <- with(tp, mean(coef(q)["nitrogen:zoneNorthern"] + 2*coef(q)["zoneNorthern:nitrogen2"]*nitrogen +
                                coef(q)["nitrogen:zoneNorthern:phosph"]*phosph +
                                coef(q)["nitrogen:zoneNorthern:precip"]*precip, na.rm=TRUE))

# calculate the optimal amount of nitrogen in the three zones - use the average
# of the phosph and precip values.
op_sh <- (0-(coef(q)["nitrogen:zoneSouthern Highlands"] +
                           coef(q)["nitrogen:zoneSouthern Highlands:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                           coef(q)["nitrogen:zoneSouthern Highlands:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q)["zoneSouthern Highlands:nitrogen2"])

op_sh10 <- (0.27103-(coef(q)["nitrogen:zoneSouthern Highlands"] +
                         coef(q)["nitrogen:zoneSouthern Highlands:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                         coef(q)["nitrogen:zoneSouthern Highlands:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q)["zoneSouthern Highlands:nitrogen2"])

op_sh12 <- (0.23431-(coef(q)["nitrogen:zoneSouthern Highlands"] +
                             coef(q)["nitrogen:zoneSouthern Highlands:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                             coef(q)["nitrogen:zoneSouthern Highlands:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q)["zoneSouthern Highlands:nitrogen2"])


op_s <- (0-(coef(q)["nitrogen:zoneSouthern"] +
                      coef(q)["nitrogen:zoneSouthern:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                      coef(q)["nitrogen:zoneSouthern:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q)["zoneSouthern:nitrogen2"])

op_s10 <- (0.21506-(coef(q)["nitrogen:zoneSouthern"] +
                      coef(q)["nitrogen:zoneSouthern:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                      coef(q)["nitrogen:zoneSouthern:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q)["zoneSouthern:nitrogen2"])

op_s12 <- (0.22189-(coef(q)["nitrogen:zoneSouthern"] +
                      coef(q)["nitrogen:zoneSouthern:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                      coef(q)["nitrogen:zoneSouthern:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q)["zoneSouthern:nitrogen2"])


op_n <- (0-(coef(q)["nitrogen:zoneNorthern"] +
                    coef(q)["nitrogen:zoneNorthern:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                    coef(q)["nitrogen:zoneNorthern:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q)["zoneNorthern:nitrogen2"])

# now try with AEZ instead of zone
q2 <- lm(maize_yld ~ nitrogen:AEZ + nitrogen2:AEZ + nitrogen:phosph:AEZ + nitrogen:precip:AEZ +
                lab + lab2 + lassets + lassets2 + slope + irrig + elevation + mech + rural + crop_count +
                mono_crop + dist2market + lab_bar + lassets_bar + nitrogen_bar+ y12, data=tp)

# only the cool/sub humid zone is significant - do calculations
# look at extension section of agricultural questionnaire for more info on vouchers
# check the assistance and groups section of the household questionnaire

op_subhumid <- (0-(coef(q2)["nitrogen:AEZCool/subhumid"] +
                    coef(q2)["nitrogen:AEZCool/subhumid:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                    coef(q2)["nitrogen:AEZCool/subhumid:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q2)["AEZCool/subhumid:nitrogen2"])

op_subhumid10 <- (0.2416-(coef(q2)["nitrogen:AEZCool/subhumid"] +
                           coef(q2)["nitrogen:AEZCool/subhumid:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                           coef(q2)["nitrogen:AEZCool/subhumid:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q2)["AEZCool/subhumid:nitrogen2"])

op_subhumid12 <- (0.1934-(coef(q2)["nitrogen:AEZCool/subhumid"] +
                                 coef(q2)["nitrogen:AEZCool/subhumid:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
                                 coef(q2)["nitrogen:AEZCool/subhumid:precip"]*(mean(tp$precip, na.rm=TRUE))))/(2*coef(q2)["AEZCool/subhumid:nitrogen2"])

MAE_subhumid <- coef(q2)["nitrogen:AEZCool/subhumid"] + 2*coef(q2)["AEZCool/subhumid:nitrogen2"]*(mean(tp$nitrogen, na.rm=TRUE)) +
        coef(q2)["nitrogen:AEZCool/subhumid:phosph"]*(mean(tp$phosph, na.rm=TRUE)) +
        coef(q2)["nitrogen:AEZCool/subhumid:precip"]*(mean(tp$precip, na.rm=TRUE))

AME_SH <- with(tp, mean(coef(q2)["nitrogen:AEZCool/subhumid"] + 2*coef(q2)["AEZCool/subhumid:nitrogen2"]*nitrogen +
                                coef(q2)["nitrogen:AEZCool/subhumid:phosph"]*phosph +
                                coef(q2)["nitrogen:AEZCool/subhumid:precip"]*precip, na.rm=TRUE))