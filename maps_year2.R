# maps for year 2
setwd('C:/locris')
library(Hmisc)
library(raster)

# 1. read in output data and select only plots growing maize
plot.IO <- read.csv('C:/Users/Tomas/Dropbox/Micro_IPOP/Analysis/Cleaned_data/plot_IO_Y2.csv')
maize <- filter(plot.IO, zaocode == 'Maize')
maize$y2_hhid <- as.character(maize$y2_hhid)

# 2. decide whether or not to winsor the output data
#    speak to michiel and suggest checking the size of the 
#    plots producing the largest amount of maize with other plots

# 3. get region and zone information - get geo information too
hhid.reg.zone <- read.csv('C:/Users/Tomas/Dropbox/Micro_IPOP/Analysis/Cleaned_data/hhid_reg_zone_y2.csv')
hhid.reg.zone$y2_hhid <- as.character(hhid.reg.zone$y2_hhid)
coords <- read.csv('C:/Users/Tomas/Dropbox/Micro_IPOP/Analysis/Cleaned_data/lon_lat.csv')
coords$y2_hhid <- as.character(coords$y2_hhid)
with(coords, plot(lon, lat))

# 4. check which households have a region and a zone entry and the same for GPS
log <- plot.IO$y2_hhid %in% hhid.reg.zone$y2_hhid
table(log) # every single one
log2 <- plot.IO$y2_hhid %in% coords$y2_hhid
table(log2) #every single 1
length(unique(plot.IO$y2_hhid)) # 2630 unique households

# 5. join maize information with region and zone information
maize2 <- left_join(maize[, c('y2_hhid', 'plotnum', 'output.kg')], hhid.reg.zone)
maize3 <- left_join(maize2, select(coords, y2_hhid, lon, lat))

# 6. create a variable splitting up output into 4 different sizes
maize3$cats <- cut2(maize3$output.kg, g = 4)
maize3.split <- split(maize3, maize3$cats)
small <- maize3.split[[1]]
big <- maize3.split[[4]]
with(small, plot(lon, lat))

# 7. have a look at where the big producer are
table(big$region)

# 7. get tanzania map and plot the farms that produce the smallest amount of maize
TZA_map <- getData('GADM', country = "TZA", level = 1) 

