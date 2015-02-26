# -------------------------------------
#' preparation file for third wave (2012)
#' of Tanzania data. The output of the file
#' is a collection of databases that will be
#' combined in to form a complete cross 
#' section for 2012.
#' 
#' 
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