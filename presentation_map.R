# output map take 3

setwd('C:/Users/morle001/Dropbox/Micro_IPOP')

library(foreign)
library(raster)
library(plyr)
library(dplyr)

# A. read in raw data files
geo.vars <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta",
                     convert.factors = TRUE)
AQSEC4A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC4A.dta",
                                         convert.factors = TRUE)
areas <- read.dta('./Data/Plot_size/areas_tza_y2_imputed.dta')
names(areas)[1] <- 'y2_hhid'

# subset for maize
maize <- filter(AQSEC4A, zaocode == 11)

missing <- anti_join(maize, areas) # only 2 plots are  missing which is great!

maize <- select(maize, y2_hhid, plotnum, total.plot = ag4a_01, crop.share = ag4a_02,
                output.kg = ag4a_15) %>% filter(!(is.na(output.kg)), !(output.kg == 0))

areas <- select(areas, y2_hhid, plotnum, area_gps_orig, area_gps_mi_50) %>% filter(!is.na(area_gps_mi_50))

maize.area <- left_join(maize, areas)
summary(maize.area)

all(maize.area$y2_hhid %in% geo.vars$y2_hhid)
maize.area.coords <- left_join(maize.area, select(geo.vars, y2_hhid, lon = lon_modified, lat = lat_modified))
maize.area.coords <- maize.area.coords[complete.cases(select(maize.area.coords, output.kg, area_gps_mi_50)), ] 

# calculate yield
maize.area.coords <- transform(maize.area.coords, yield = output.kg/area_gps_mi_50)
TZA_map <- getData('GADM', country = "TZA", level = 1) ## get map of Tanzania
plot(TZA_map)
points(maize.area.coords$lon, maize.area.coords$lat)

nrow(unique(select(maize.area.coords, lon, lat))) # 288 communities

test <- ddply(maize.area.coords, .(lon, lat), summarize, avg = mean(yield, na.rm = TRUE))

plot(TZA_map)
points(test$lon, test$lat, col = test$avg, pch = 19)
