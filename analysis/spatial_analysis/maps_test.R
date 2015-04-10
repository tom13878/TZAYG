# maps of yield for year 1 and year 2
# year 2 first
setwd('C:/Users/morle001/Dropbox/Micro_IPOP')

library(foreign)
library(dplyr)

# 1. calculate yield on the plots - note the large number of plots which do not
# have a GPS estimate - ** could use the farmers estimate instead
plot.IO <- read.csv('./Analysis/Cleaned_data/plot_IO_Y2.csv')
AQSEC2A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC2A.dta",
                    convert.factors = TRUE)

output <- filter(plot.IO, zaocode == 'Maize') %>% select(y2_hhid, plotnum, output.kg)
output$y2_hhid <- as.character(output$y2_hhid)
area <- select(AQSEC2A, y2_hhid, plotnum, area = ag2a_09)

maize <- left_join(output, area)
maize <- transform(maize, area = area*0.40468564224)
maize <- filter(maize, !(area == 0), !(output.kg == 0))
maize$yield <- maize$output.kg/maize$area
summary(maize)
hist(maize$yield, breaks = 50, col = 50, main = 'maize yield', xlab = 'yield')

# get ea identifier - and create dataframe with unique lon and lat with ea_id
# failing that just split on lon and lat using ddply
lon.lat <- read.csv('./Analysis/Cleaned_data/lon_lat.csv')
lon.lat$y2_hhid <- as.character(lon.lat$y2_hhid)
x <- left_join(maize, select(lon.lat, y2_hhid, lon, lat))
com <- ddply(x, .(lon, lat), summarize, avg = mean(yield, na.rm = TRUE))

# make a map with ggplot
TZA_map <- getData('GADM', country = "TZA", level = 1) ## get map of Tanzania
tf = fortify(TZA_map)
ggplot(tf) + geom_polygon(aes(long, lat, fill = id), colour = 'black') + coord_map("mercator") +
  geom_point(data = com, aes(x = lon, y = lat, colour = avg, size = 5))


# 2. merge in with the longitude and latitude
lon.lat <- read.csv('./Analysis/Cleaned_data/lon_lat.csv')
lon.lat$y2_hhid <- as.character(lon.lat$y2_hhid)
maize <- left_join(maize, select(lon.lat, y2_hhid, lon, lat))

# get ea identifier and split to calauclate the average yield per community
maize <- left_join(maize, select(plot.geo, y2_hhid, ea_id))
maize.com <- ddply(maize, .(ea_id), )

# TODO {tom morley}: use google maps to find out where clustered communities are