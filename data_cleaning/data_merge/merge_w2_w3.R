# -------------------------------------
# merge wave 2 and 3 into a single panel
# -------------------------------------

setwd("M:/TZAYG/data")
library(haven)
library(dplyr)


# ------------wave 2-------------------

# read in all the different data files that you need.
# plots are the basic unit here and we need output more than anything
# else so get that first
output_w2 <- read_dta("./2010/output_index_w2.dta")

# next up get the plot variables
plot_vars_w2 <- read_dta("./2010/plot_variables_w2.dta")

# now grab the area variables
areas_w2 <- read_dta("./2010/areas_w2.dta")

# and finally get the household information
HH_info_w2 <- read_dta("./2010/HH_total_w2.dta")

# now join everything together
section_wave2 <- left_join(output_w2, plot_vars_w2)
section_wave2 <- left_join(section_wave2, areas_w2)
section_wave2 <- left_join(section_wave2, HH_info_w2)

# add a variable for y3_hhid, we will need it later
section_wave2$y3_hhid <- NA

# ------------wave 3 ------------------


# read in all the different data files that you need.
# plots are the basic unit here and we need output more than anything
# else so get that first
output_w3 <- read.csv("./2012/output_index_w3.csv")

# next up get the plot variables
plot_vars_w3 <- read_dta("./2012/plot_variables_w3.dta")

# now grab the area variables - which are in the cleaned
# data directory
areas_w3 <- read.csv("M:/cleaned_data/2012/areas_w3.csv")

# and finally get the household information
HH_info_w3 <- read_dta("./2012/HH_total_w3.dta")
HH_info_w3$y2_hhid <- zap_empty(HH_info_w3$y2_hhid)
# later we will need a y2_hhid, even when one is not 
# available. So where y2_hhid is NA replace with the y3_hhid
HH_info_w3$y2_hhid[is.na(HH_info_w3$y2_hhid)] <- HH_info_w3$y3_hhid[is.na(HH_info_w3$y2_hhid)]

# now join everything together
section_wave3 <- left_join(output_w3, plot_vars_w3)
section_wave3 <- left_join(section_wave3, areas_w3)
section_wave3 <- left_join(section_wave3, HH_info_w3)



# ------------section--------------------

# make sure that all variables are in the right order and get rid of the year 3
# id
# section_wave3 <- select(section_wave3, -y3_hhid)
section_wave3 <- section_wave3[, names(section_wave2)]

# add a year variable to each wave of the section
section_wave2$year <- 2010
section_wave3$year <- 2012

# make sure every variable has the same class
sapply(section_wave3, class) == sapply(section_wave2, class)
section_wave2$price_type2 <- as.numeric(section_wave2$price_type2)
section_wave2$price_type1 <- as.numeric(section_wave2$price_type1)
section_wave2$inorg_price1 <- as.numeric(section_wave2$inorg_price1)
section_wave2$inorg_price2 <- as.numeric(section_wave2$inorg_price2)
section_wave2$y3_hhid <- factor(section_wave2$y3_hhid)
section_wave3$inorg_price2 <- as.numeric(section_wave3$inorg_price2)
section_wave3$cash_crop <- as.integer(ifelse(section_wave3$cash_crop %in% TRUE, 1, 0))
section_wave3$beans <- as.integer(ifelse(section_wave3$beans %in% TRUE, 1, 0))
section_wave3$sex <- as.integer(ifelse(section_wave3$sex %in% "male", 1, 0))
section_wave2$y3_hhid <- as.character(section_wave2$y3_hhid)
section_wave3$inorg_price1 <- as.numeric(section_wave3$inorg_price1)
all(sapply(section_wave3, class) == sapply(section_wave2, class))

# make sure that the y2_hhid variables are both characters and rename them
section_wave2 <- rename(section_wave2, hhid=y2_hhid)
section_wave3 <- rename(section_wave3, hhid=y2_hhid)
section_wave2$hhid <- as.character(section_wave2$hhid)
section_wave3$hhid <- as.character(section_wave3$hhid)

# now check that every factor variable has the same levels so that they can be 
# matched-up
# the soil variable has a level "OTHER (SPECIFY)" in wave3 compared to "OTHER" in wave 2
all(levels(section_wave2$soil)==levels(section_wave3$soil)) # FALSE
levels(section_wave3$soil)[levels(section_wave3$soil)=="OTHER (SPECIFY)"] <- "OTHER"
all(levels(section_wave2$soil)==levels(section_wave3$soil)) # TRUE

# soilq variable seems to be fine
all(levels(section_wave2$soilq)==levels(section_wave3$soilq)) # TRUE

# erosion variable seems to be fine
all(levels(section_wave2$erosion)==levels(section_wave3$erosion)) #TRUE

# slope variable seems to be fine
all(levels(section_wave2$slope)==levels(section_wave3$slope)) # TRUE

# irrigation is also fine
all(levels(section_wave2$irrig)==levels(section_wave3$irrig)) # TRUE

all(levels(section_wave2$org)==levels(section_wave3$org)) # TRUE

all(levels(section_wave2$inorg1)==levels(section_wave3$inorg1)) # TRUE

all(levels(section_wave2$inorg_type1)==levels(section_wave3$inorg_type1)) #TRUE

all(levels(section_wave2$voucher1)==levels(section_wave3$voucher1))

all(levels(section_wave2$inorg2)==levels(section_wave3$inorg2)) # TRUE

all(levels(section_wave2$inorg_type2)==levels(section_wave3$inorg_type2)) #TRUE

all(levels(section_wave2$voucher2)==levels(section_wave3$voucher2))

all(levels(section_wave2$inorg1)==levels(section_wave3$inorg1)) # TRUE

all(levels(section_wave2$inorg_type1)==levels(section_wave3$inorg_type1)) #TRUE

all(levels(section_wave2$pest)==levels(section_wave3$pest))

all(levels(section_wave2$inorg1)==levels(section_wave3$inorg1)) # TRUE

all(levels(section_wave2$inorg_type1)==levels(section_wave3$inorg_type1)) #TRUE

all(levels(section_wave2$pestU)==levels(section_wave3$pestU)) # FALSE
levels(section_wave3$pestU)[levels(section_wave3$pestU) %in% "OTHER (SPECIFY)"] <- NA
all(levels(section_wave2$pestU)==levels(section_wave3$pestU)) 

all(levels(section_wave2$inorg1)==levels(section_wave3$inorg1)) # TRUE

all(levels(section_wave2$inorg_type1)==levels(section_wave3$inorg_type1)) #TRUE

all(levels(section_wave2$voucher1)==levels(section_wave3$voucher1))

all(levels(section_wave2$inorg1)==levels(section_wave3$inorg1)) # TRUE

all(levels(section_wave2$inorg_type1)==levels(section_wave3$inorg_type1)) #TRUE

all(levels(section_wave2$voucher1)==levels(section_wave3$voucher1)) # TRUE

all(levels(section_wave2$short_rain)==levels(section_wave3$short_rain)) # TRUE

# there is no need for the crops to be the same but we should make them all 
# lower case also note that in wave 3 we have "cassava" rather than "casava" in 
# wave 2. Also need to change "OTHER (SPECIFY)" to "OTHER" 
all(levels(section_wave2$short_rain_crop)==levels(section_wave3$short_rain_crop)) # FALSE
levels(section_wave2$short_rain_crop) <- tolower(levels(section_wave2$short_rain_crop))
levels(section_wave3$short_rain_crop) <- tolower(levels(section_wave3$short_rain_crop))
levels(section_wave3$short_rain_crop)[levels(section_wave3$short_rain_crop) %in% "cassava"] <- "casava"
levels(section_wave3$short_rain_crop)[levels(section_wave3$short_rain_crop) %in% "other (specify)"] <- "other"

# one of the levels is "KUSHIRIKIANA NI LA KUKODI" and even google translate doesn't know what
# it means . . . only three observations so I'm just going to drop it. Also change "RENTED IN"
# to "RENTED". And put "SHARED - OWN" to "OWNED", only seven observations
all(levels(section_wave2$owned)==levels(section_wave3$owned)) # FALSE
levels(section_wave2$owned)[levels(section_wave2$owned) %in% "KUSHIRIKIANA NI LA KUKODI"] <- NA
levels(section_wave2$owned)[levels(section_wave2$owned) %in% "RENTED IN"] <- "RENTED"
levels(section_wave2$owned)[levels(section_wave2$owned) %in% "SHARED - OWN"] <- "OWNED"
all(levels(section_wave2$owned)==levels(section_wave3$owned)) # TRUE

all(levels(section_wave2$inorg1)==levels(section_wave3$inorg1)) # TRUE

all(levels(section_wave2$inorg_type1)==levels(section_wave3$inorg_type1)) #TRUE

all(levels(section_wave2$sex)==levels(section_wave3$sex)) # TRUE

# now it should be possible to put everything together

panel <- rbind(section_wave2, section_wave3)

# and write the panel to a file for use later
# write.csv(panel, "panel.csv", row.names=FALSE)
# write_dta(panel, "panel.dta")

