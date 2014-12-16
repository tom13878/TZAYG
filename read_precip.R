# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Code for reading in nc files containing precipitation values. These values are then extracted for
# the rainy season in 2008 and 2010
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
setwd("C:/Users/morle001/Dropbox/Micro_IPOP")
# 1. get the ncdf and raster packages from library and read the nc data directly as a
#    raster object. In this case we read in a list 
library(ncdf)
library(raster)
library(dplyr)

# 2.read in the precipitation data and the geo.vars data for 2008 and 2010
precip.raster <- stack("./Data/Spatial/Climate_data/cru_ts3.22.2001.2010.pre.dat.nc")

# 3. cut out tanzania from raster object
TZA.map <- getData('GADM', country = "TZA", level = 1)
TZA.precip.raster <- crop(precip.raster, TZA.map)

# 3. selecting the correct years and months for the long rainy season in Tanzania, 2008 and 2010

x <- slot(slot(TZA.precip.raster, "data"), "values")
dimnames(x)

mar.y1 <- raster(TZA.precip.raster, layer = 87)
apr.y1 <- raster(TZA.precip.raster, layer = 88)
may.y1 <- raster(TZA.precip.raster, layer = 89)

mar.y2 <- raster(TZA.precip.raster, layer = 111)
apr.y2 <- raster(TZA.precip.raster, layer = 112)
may.y2 <- raster(TZA.precip.raster, layer = 113)

precip.raster.y1 <- stack(mar.y1, apr.y1, may.y1)
precip.raster.y2 <- stack(mar.y2, apr.y2, may.y2)

nlayers(precip.raster.y1); nlayers(precip.raster.y2)
slot(precip.raster.y1, "layers"); slot(precip.raster.y2, "layers")
names(precip.raster.y1); names(precip.raster.y1)

# read in the geo data for 2008 and 2010
hhid.lon.lat.y1 <- read.csv("./Analysis/Cleaned_data/hhid_lon_lat_y1.csv")
hhid.lon.lat.y2 <- read.csv("./Analysis/Cleaned_data/hhid_lon_lat_y2.csv")

coord.y1 <- select(hhid.lon.lat.y1, lon, lat)
coord.y2 <- select(hhid.lon.lat.y2, lon, lat)

precip.vals.y1 <- extract(precip.raster.y1, coord.y1)
precip.vals.y2 <- extract(precip.raster.y2, coord.y2)

precip.vars.y1 <- data.frame(cbind(hhid.lon.lat.y1, precip.vals.y1)) %>% 
  rename(pre.mar.y1 = X2008.03.16,
         pre.apr.y1 = X2008.04.16,
         pre.may.y1 = X2008.05.16)
precip.vars.y2 <- data.frame(cbind(hhid.lon.lat.y2, precip.vals.y2)) %>% 
  rename(pre.mar.y2 = X2010.03.16,
         pre.apr.y2 = X2010.04.16,
         pre.may.y2 = X2010.05.16)

head(precip.vars.y1, 20)
head(precip.vars.y2, 20)

write.csv(precip.vars.y1, "./Analysis/Cleaned_data/precip_vars_y1.csv", row.names = FALSE)
write.csv(precip.vars.y2, "./Analysis/Cleaned_data/precip_vars_y2.csv", row.names = FALSE)

# some plots
plot(precip.raster.y1, 1)
plot(TZA.map, add = TRUE)
points(coord.y1, col = "blue")
# try looking at a single community 
points(35.85, -5.086, col = "red")
# query with the click function to look at the raster precipitation at the point of interest
click(precip.raster.y1)
# from the map have a look at points in the region (36 - 38, -9 - -7) which should have high pre values
sub.vals <- subset(coord.y1, lon > 36 & lon < 38 & lat > -9 & lat < -7)
points(sub.vals, col = "red")
click(precip.raster.y1)












