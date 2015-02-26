# -------------------------------------
#' preparation file for third wave (2012)
#' of Tanzania data. The output of the file
#' is a collection of databases that will be
#' combined in to form a complete cross 
#' section for 2012.
#' 
#' Each section can be run seperately
#' provided that that all the packages 
#' and functions needed are called first
# -------------------------------------

# set working directory and install packages
filepath = 'W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data'
setwd(filepath)

library(foreign)
library(reshape2)
library(gdata)
library(plyr)
library(dplyr)

source("M:/TZAYG/plus.R") 
# -------------------------------------
# household data
# -------------------------------------

# get basic infomation from the household survey: variable indidy3 is the
# identification number of each individual in the household. Only information
# on the HEAD of the household (who is assumed to be the plot manager) is
# required
HHB <- read.spss( 'HH_SEC_B.SAV', to.data.frame = TRUE )
HH <- select( HHB, y3_hhid, indidy3, sex = hh_b02, age = hh_b04, status = hh_b05 )
HH <- ddply( HH, .( y3_hhid ), transform, hh.size=length( indidy3 ) ) 
HH <- filter( HH, status == "HEAD" )

# Get a variable for whether the household is in a rural area or not
HHA <- read.spss( 'HH_SEC_A.SAV', to.data.frame = TRUE )
rural_weight <- select( HHA, y3_hhid, y3_rural, y3_weight )

# calcualte the households capital stocks. quantity refers to the 
# quantity of a particular item owned or rented by the household.
# value refers to the price in shillings for that item if it were 
# to be sold. 
AG11 <- read.spss( 'AG_SEC_11.SAV', to.data.frame = TRUE )
cap <- select( AG11, y3_hhid, itemname, quantity.own=ag11_01, value.own=ag11_02,
               quantity.rent=ag11_07, value.rent=ag11_09 )
cap <- group_by( cap, y3_hhid ) %>%
        summarise( own.sh=plus( quantity.own*value.own ),
                   rent.sh=plus( quantity.rent*value.rent ) )

# join together information on household characteristics and capital
HH.total <- left_join( HH, cap )

# -------------------------------------
# plot level data 
# -------------------------------------

# agricultural questionnaire section 3A contains a lot of plot specific 
# information including fertilizer use.
AG3A <- read.spss('AG_SEC_3A.SAV', to.data.frame = TRUE)

# the plotnumber variable in AG3A has whitespace that needs to be removed
gsub(" ", "", "M1 ", fixed = TRUE) # returns "M1"
AG3A$plotnum <- gsub(" ", "", AG3A$plotnum, fixed = TRUE)

# Select variables that are important for analysis
# fallow has 0 if plot has never been left fallow and 98 if the respondent does
# not know when the last time the plot was left fallow
plot.vars <- select(AG3A, y3_hhid, plotnum, soil = ag3a_10, soilq = ag3a_11,
               erosion = ag3a_13, slope = ag3a_17, irrig = ag3a_18,
               fallow = ag3a_22, fallow.years = ag3a_23, org = ag3a_41,
               orgQ1 = ag3a_42, inorg1 = ag3a_47, inorg.type1 = ag3a_48,
               inorgQ1 = ag3a_49, voucher1 = ag3a_50, inorg2 = ag3a_54,
               inorg.type2 = ag3a_55, inorgQ2 = ag3a_56, voucher2 = ag3a_57,
               pest = ag3a_60, pestQ = ag3a_62_1, pestU = ag3a_62_2, 
               short.rain = ag3a_81, short.rain.crop = ag3a_82)


# calculate the amount of nitrogen that was used per plot - main problem
# is that when two kinds of inorganic fertilizer have been used on the same plot
# it is necessary to calculate the sum of nitrogen in each fertilizer.
plot.vars <- mutate(plot.vars,
               inorg.type1 = factor(inorg.type1,
                                    labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                             "generic NPK (TZA)", "MRP")),
               inorg.type2 = factor(inorg.type2,
                                    labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                             "generic NPK (TZA)", "MRP")))

# select only plot key and fertilizer type - melt the data frame so that the
# observational unit because fertilizer type per plot
s <- select(fert.vars, y3_hhid, plotnum, inorg.type1, inorg.type2)
m <- melt(s, id = c('y3_hhid', 'plotnum')) 

# filter m for eachtype of fertilizer and join this up with the quantity
ty1 <- filter(m, variable == 'inorg.type1') %>%
        left_join(select(plot.vars, y3_hhid, plotnum, quantity = inorgQ1))
ty2 <- filter(m, variable == 'inorg.type2') %>%
        left_join(select(plot.vars, y3_hhid, plotnum, quantity = inorgQ2))
tot <- rbind(ty1, ty2) %>% rename(type = value)

# read in fertilizer composition table
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")
fert.comp <- read.xls( "Data/Other/Fert_comp.xlsx", sheet = 2 ) %>%
        rename( type = Fert_type2 )
setwd(filepath)

fert.comp <- transform( fert.comp, P_share = as.numeric(P_share),
                        K_share = as.numeric( K_share ),
                        N_share = as.numeric( N_share ) )

# join composition table with fertilizer information on type
fert.vars <- left_join( tot, select( fert.comp, type, N_share, P_share, K_share ) )

# calculate Nitrogen share of each fertilizer type
fert.vars <- mutate( fert.vars, N = quantity * (N_share/100),
                     P = quantity * (P_share/100), K = quantity * (K_share/100) )

fert.vars <- group_by( fert.vars, y3_hhid, plotnum ) %>%
        summarise( nitrogen.kg = sum( N, na.rm = TRUE ),
                  phosphorous.kg = sum( P, na.rm = TRUE ),
                  potassium.kg = sum( K, na.rm = TRUE ) )

# get values of fertilizer for each plot split into components.
fert.vars <- select(fert.vars, y3_hhid, plotnum, everything()) %>% 
        ddply(.(y3_hhid, plotnum), summarize,
              nitrogen.kg = sum(N, na.rm = TRUE),
              phosphorous.kg = sum(P, na.rm = TRUE),
              potassium.kg = sum(K, na.rm = TRUE))

# join fertilizer information with other important variables
plot.vars <- left_join(plot.vars, fert.vars)

