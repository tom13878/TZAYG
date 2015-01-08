# 1. find all plots that have maize grown on them
# 2. calculate the yield on each plot
# 3. join with longitude and latitude
# 4. plot a map of tanzania with points of different size depending on yields

setwd('C:/Users/morle001/Dropbox/Micro_IPOP')

library(foreign)
library(dplyr)

# A. read in data on output, area, and logitude and latitude

plot.IO <- read.csv('./Analysis/Cleaned_data/plot_IO_Y2.csv')
areas1 <- AQSEC2A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC2A.dta",
                              convert.factors = TRUE)
areas2 <- read.dta('./Data/Plot_size/areas_tza_y2_imputed.dta')
lon.lat <- read.csv('./Analysis/Cleaned_data/lon_lat.csv')

lon.lat$y2_hhid <- as.character(lon.lat$y2_hhid)
plot.IO$y2_hhid <- as.character(plot.IO$y2_hhid)

# B. find plots where maize is grown

maize <- plot.IO[plot.IO$zaocode == 'Maize' & !(is.na(plot.IO$zaocode)), ]

# C. check if we have longitude and latitude for every household for which we 
#    have maize output

log <- unique(maize$y2_hhid) %in% unique(lon.lat$y2_hhid)
table(log)

# D. check if we have areas for all of these plots

areas12 <- areas1[!is.na(areas1$ag2a_09) & !(areas1$ag2a_09 == 0),]
missing.area <- anti_join(maize, areas12)
nrow(missing.area)

# E. check if these missing areas are in the imputed areas file

summary(areas2)
names(areas2)[1] <- 'y2_hhid'
still.missing <- anti_join(missing.area, areas2)
# still missing areas for 958 plots

# F. construct a dataframe containing all the areas that we have with output

maize1 <- select(maize, y2_hhid, plotnum, output.kg, crop.share) 
area1 <- 
