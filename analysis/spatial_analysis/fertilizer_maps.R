# second go at fertilizer map

setwd('C:/Users/morle001/Dropbox/Micro_IPOP')

library(foreign)
library(raster)
library(plyr)
library(dplyr)

# A. read in raw data files, GPS, fert and areas
geo.vars <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta",
                     convert.factors = TRUE) %>% select(y2_hhid, lon = lon_modified, lat = lat_modified)
areas <- read.dta('./Data/Plot_size/areas_tza_y2_imputed.dta') %>%
  select(y2_hhid = case_id, plotnum, area_gps_orig, area_gps_mi_50)
fert <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC3A.dta",
                    convert.factors = FALSE) %>% select(y2_hhid, plotnum, fert.kg1 = ag3a_47, fert.kg2 =ag3a_54)
fert$fert.kg1[is.na(fert$fert.kg1)] <- 0 
fert$fert.kg2[is.na(fert$fert.kg2)] <- 0 

fert <- transform(fert, fert.tot = fert.kg1 + fert.kg2)

# plot all the households first in green

## get map of Tanzania
tf <- fortify(TZA_map)

tz1 <- ggplot(tf) + geom_polygon(data=tf, aes(x=long, y=lat, group=group), fill="grey40", colour="black", alpha = .3, size = .1)
tz2 <- tz1 + geom_point(data = geo.vars, aes(x=lon, y=lat), colour="green", size = 1)

# select only the plots where fertilizer is used and plot in blue
fert1 <- filter(fert, !(fert.tot == 0)) %>% left_join(geo.vars)
tz3 <- tz2 + geom_point(data = fert1, aes(x=lon, y=lat), colour="blue", size = 1)
tz4 <- tz3 + coord_map("mercator") 
ggsave("./Analysis/Maps_tables/tan_map_fert1.pdf", width = 20, height = 15, units = "cm")

# remove large values
fert2 <- filter(fert1, !(fert.tot > 500))

# merge fertilizer and areas data; calcualte fert application rates
fert.area <- left_join(fert2, areas)
fert.area <- transform(fert.area, app = fert.tot/area_gps_mi_50) %>% 
  filter(!is.na(area_gps_mi_50))
fert.area2 <- filter(fert.area, !(app > 1000))

# plot
fert.area3 <- ddply(fert.area2, .(lon, lat), summarize, avg = mean(app, na.rm = TRUE))
tz21 <- ggplot(tf) + geom_polygon(data=tf, aes(x=long, y=lat, group=group), fill="grey40", colour="black", alpha = .3, size = .1)
tz22 <- tz21 + geom_point(data = fert.area3, aes(x=lon, y=lat, size=avg), colour="red")
tz23 <- tz22 + coord_map("mercator") + guides(size=guide_legend(reverse=TRUE))
tz24 <- tz23 + scale_size_continuous(name = 'fert\n(kg/ha)')
ggsave("./Analysis/Maps_tables/tan_map_fert2.pdf", width = 20, height = 15, units = "cm")


