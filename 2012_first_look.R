# ------------------------
# third wave of panel data preparation - 2012: a first look
# ------------------------
# Look at the agricultural data sections
library(foreign)
library(reshape2)
library(data.table)
library(gdata)
library(plyr)
library(dplyr)

# filepath to data
filepath = 'W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data'
setwd(filepath)

# -------------------------
# first look at the agricultural questionnaire data
# -------------------------

# read in agricultural questionnaire section 2 A
AG2A <- data.table(read.spss('AG_SEC_2A.SAV', to.data.frame = TRUE))

# both area measurements are given in acres in year 3
AG2A <- select(AG2A, y3_hhid, plotnum, area.est = ag2a_04, area.gps = ag2a_09)

# read in agricultural questionnaire section 3 A
AG3A <- read.spss('AG_SEC_3A.SAV', to.data.frame = TRUE)

# still to add household and hired labour
AG3A <- select(AG3A, y3_hhid, plotnum, soil = ag3a_10, soilq = ag3a_11,
               erosion = ag3a_13, slope = ag3a_17, irrig = ag3a_18,
               fallow = ag3a_22, fallow.years = ag3a_23, org = ag3a_41,
               orgQ1 = ag3a_42, inorg1 = ag3a_47, inorg.type1 = ag3a_48,
               inorgQ1 = ag3a_49, voucher1 = ag3a_50, inorg2 = ag3a_54,
               inorg.type2 = ag3a_55, inorgQ2 = ag3a_56, voucher2 = ag3a_57,
               pest = ag3a_60, pestQ = ag3a_62_1, pestU = ag3a_62_2, 
               short.rain = ag3a_81, short.rain.crop = ag3a_82)

# read in agricultural questionnaire section 4 A
AG4A <- read.spss('AG_SEC_4A.SAV', to.data.frame = TRUE)

# input and output variables
AG4A <- select(AG4A, y3_hhid, plotnum, zaocode, full.area = ag4a_01,
               inter.crop = ag4a_04, seed.type = ag4a_08, output = ag4a_28)

# calculate the nitrogen component
AG3A <- mutate(AG3A,
               inorg.type1 = factor(inorg.type1,
                                    labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                                         "generic NPK (TZA)", "MRP")),
               inorg.type2 = factor(inorg.type2,
                                    labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                                          "generic NPK (TZA)", "MRP")))

# calculate the nitrogen value on each plot - 2 types of fertilizer per plot
s <- select(AG3A, y3_hhid, plotnum, inorg.type1, inorg.type2)
m <- melt(s, id = c('y3_hhid', 'plotnum')) # melt on the dataframe automatically gives it keys

ty1 <- filter(m, variable == 'inorg.type1') %>%
        left_join(select(AG3A, y3_hhid, plotnum, quantity = inorgQ1))
ty2 <- filter(m, variable == 'inorg.type2') %>%
        left_join(select(AG3A, y3_hhid, plotnum, quantity = inorgQ2))
tot <- rbind(ty1, ty2) %>% rename(type = value)

# read in fertilizer composition table
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")
fert.comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2) %>% rename(type = Fert_type2)

fert.comp <- transform( fert.comp, P_share = as.numeric(P_share),
       K_share = as.numeric(K_share),
       N_share = as.numeric(N_share) )
# fert.comp$P_share <- as.numeric(fert.comp$P_share)
# fert.comp$K_share <- as.numeric(fert.comp$K_share)
# fert.comp$N_share <- as.numeric(fert.comp$N_share)

# join composition table with fertilizer information on type
fert.vars <- left_join(tot, select(fert.comp, type, N_share, P_share, K_share))

# calculate Nitrogen share of each fertilizer type
fert.vars <- mutate( fert.vars, N = quantity * (N_share/100),
                    P = quantity * (P_share/100), K = quantity * (K_share/100) )

# get values of fertilizer for each plot split into components.
fert.vars <- select(fert.vars, y3_hhid, plotnum, everything()) %>% 
        ddply(.(y3_hhid, plotnum), summarize,
              nitrogen.kg = sum(N, na.rm = TRUE),
              phosphorous.kg = sum(P, na.rm = TRUE),
              potassium.kg = sum(K, na.rm = TRUE))

# join fertilizer information with other important variables
plot.IO <- left_join(fert.vars, AG3A)


#-----------------------------------
# try some plots with what we have for 
# are.gps, maize output and fertilizer
#-----------------------------------

setkey(AG4A, zaocode)
maize <- AG4A['MAIZE',]
maize_area <- left_join(maize,plot.IO)
maize_area_fert <- left_join(maize_area, plot.IO)

keycols <- c("y3_hhid", "plotnum")
setkeyv(fert.vars, keycols)
tables()

# for some reason merging is not working - manually checking variables
x <- select(plot.IO, y3_hhid, plotnum, nitrogen.kg)
y <- filter(AG4A, zaocode == "MAIZE", full.area == "YES")

x2 <- x[!x$nitrogen.kg==0, ]

y2 <- y[y$y3_hhid %in% x2$y3_hhid, ]

x2y2 <- left_join(x2, y2)
hist(x2$nitrogen.kg, col = 'grey', border = "white")

# try making a new plot id variable
x2$plotid <- paste(x2$y3_hhid, x2$plotnum, sep = "")
y2$plotid <- paste(y2$y3_hhid, y2$plotnum, sep = "")

# looks like there is whitespace in the plotnum variable like this "M1 " rather
# than "M1"
