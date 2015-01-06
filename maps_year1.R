# maps for presentation: year 1
# Start with year one and write code to create a map of the amount of maize
# produced in each region/zone, also accopanied by some tables
setwd("C:/Users/morle001/Dropbox/Micro_IPOP")
source("M:/presentation/winsor_vector.R")

# maize output by farm
plot.IO <- read.csv('./Analysis/Cleaned_data/plot_IO_Y1.csv')
plot.IO$hhid <- as.character(plot.IO$hhid)
maize <- plot.IO[plot.IO$zaocode == "Maize", ]
hist(maize$output.kg, col = "red", breaks = 50)
maize.winsor <- winsor(maize$output.kg, 0.95)
hist(maize.winsor, col = "red", breaks = 50)
maize.winsor <- data.frame(cbind(maize$hhid, maize.winsor))
names(maize.winsor) <- c("hhid", "maize.output")

# read in hhid, region and zone file
hhid.reg.zone <- read.csv('./Analysis/Cleaned_data/hhid_reg_zone_y1.csv')
hhid.reg.zone$hhid <- as.character(hhid.reg.zone$hhid)

test <- maize.winsor$hhid %in% hhid.reg.zone$hhid
table(test) # 102 observations drop out: below code finds which ones
missing <- anti_join(maize.winsor, hhid.reg.zone) # no GPS coordinates for these. check against year 2 GPS data

maize.winsor <- maize.winsor[test,]
maize.winsor <- left_join(maize.winsor, hhid.reg.zone)

# map of regions by output - get a whole bunch of maps packages
library(raster)
library(ggplot2)
x <- c("rgdal", "dplyr", "ggmap", "RColorBrewer", "rgeos")
lapply(x, library, character.only = TRUE)
library(maptools)
library(dplyr)

TZA_map <- getData('GADM', country = "TZA", level = 1) ## get map of Tanzania
output.region <- ddply(maize.winsor, .(region), summarize, output = sum(maize.output, na.rm = TRUE))
names(output.region)[1] <- "NAME_1"
TZA_map@data <- left_join(TZA_map@data, output.region)

# different spellings across data sources, make changes manually
# really just dar es salaam and the islands
test <- as.character(output.region$NAME_1) %in% unique(TZA_map@data$NAME_1)
output.region$NAME_1[!test]
NAME_1 <- as.character(output.region$NAME_1)
NAME_1[NAME_1 == "Dar es salaam"] <- 'Dar-Es-Salaam'
NAME_1[NAME_1 == "MJINI/MAGHARIBI UNGUJA"] <- 'Zanzibar West'
NAME_1[NAME_1 == "KASKAZINI PEMBA"] <- 'Kaskazini-Pemba'
NAME_1[NAME_1 == "KASKAZINI UNGUJA"] <- 'Kaskazini-Unguja'
NAME_1[NAME_1 == "KUSINI PEMBA"] <- 'Kusini-Pemba'
NAME_1[NAME_1 == "KUSINI UNGUJA"] <- 'Zanzibar South and Central'
test2 <- NAME_1 %in% unique(TZA_map@data$NAME_1)
table(test2)

output.region2 <- data.frame(cbind(NAME_1, output.region$output))
names(output.region2) <- c("NAME_1", "output")
output.region2$NAME_1 <- as.character(output.region2$NAME_1)
output.region2$output <- as.integer(as.character(output.region2$output)) # watch out for coercion rules in R

TZA_map@data <- left_join(TZA_map@data, output.region2)
tf <- fortify(TZA_map, region = "NAME_1")
names(tf)[7] <- "NAME_1"
tf <- left_join(tf, TZA_map@data)

# make some nice colours
cols <- brewer.pal(3, "Greys")
pal <- colorRampPalette(cols)

# ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output))

ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output), colour = "black") +
  scale_color_hue() + coord_map("mercator") + theme_bw()

# below works only for discrete - hence factor output
# ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = factor(output)), colour = "black") +
#  scale_fill_hue() + coord_map("mercator") + theme_bw()

# ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = factor(output)), colour = "black") +
#  scale_fill_manual(values = pal(26)) + coord_map("mercator") + theme_bw()

# ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output), colour = "black") +
#  scale_color_grey() + coord_map("mercator") + theme_bw()

# bad palette - grey is also default where there is no data!
ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output), colour = "black") +
  scale_fill_gradient(low = 'white', high = 'brown') + coord_map("mercator") + theme_bw()

# add to this plot the points of households which have a certain level of output
# create a categorical variable for doing this going back to maize production
# need a good way of doing this - probably best weighted by number of farmers
# producing at each level - start by just splitting in teo and assume that 75% of 
# farmers are small guys
maize.winsor$cats <- cut2(maize.winsor$maize.output, g = 4)
# get coordinates of each household
geo.vars <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1_consdta/HH.Geovariables_Y1.dta",
                     convert.factors = TRUE)
maize.geo <- select(geo.vars, hhid, lat = lat_modified, lon = lon_modified)
maize_comp <- inner_join(maize.winsor, maize.geo)
maize_split <- split(maize_comp, maize_comp$cats)
small.guys <- maize_split[[1]]
medium.small.guys <- maize_split[[2]]
medium.big.guys <- maize_split[[3]]
big.guys <- maize_split[[4]]
# add cordinates to the map of tanzania with ggplot
ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output), colour = "black") +
  scale_fill_gradient(low = 'white', high = 'grey20') + coord_map("mercator") + theme_bw() +
  geom_point(data = small.guys, aes(x = lon, y = lat, colour = 'red', size = 3))
# lots of points missing from northern and also from some of the other regions.
# not sure why!! also plot does not make much sense, so split on cat and make more than one plot
ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output), colour = "black") +
  scale_fill_gradient(low = 'white', high = 'grey20') + coord_map("mercator") + theme_bw() +
  geom_point(data = medium.small.guys, aes(x = lon, y = lat, colour = 'red', size = 3))

ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output), colour = "black") +
  scale_fill_gradient(low = 'white', high = 'grey20') + coord_map("mercator") + theme_bw() +
  geom_point(data = medium.big.guys, aes(x = lon, y = lat, colour = 'red', size = 3))

ggplot(tf) + geom_polygon(aes(long, lat, group = group, fill = output), colour = "black") +
  scale_fill_gradient(low = 'white', high = 'grey20') + coord_map("mercator") + theme_bw() +
  geom_point(data = big.guys, aes(x = lon, y = lat, colour = 'red', size = 3))

# TODO {tom morley}: go back over the winsoring part and join up with zone and region first
# find out which regions are being winsored - also which regions you do not have data for and
# why?

# TODO {tom morley}: same analysis for the year 2 data - also work on getting the year
# 2 hhids to fix up some year 1 problems

# TODO {tom morley}: work on the fertilizer map that you want to present
