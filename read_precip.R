setwd("C:/Users/morle001/Dropbox/Micro_IPOP")
# 1. get the ncdf and raster packages from library and read the nc data directly as a
#    raster object. In this
#    case we read in a list 
library(ncdf)
library(raster)

ncin_raster_y1 <- stack("./Data/Spatial/Climate_data/cru_ts3.22.2001.2010.pre.dat.nc")
ncin_raster_y2 <- stack(" ./Data/Spatial/Climate_data/cru_ts3.22.2011.2013.pre.dat.nc")
nlayers(ncin_raster_y1); nlayers(ncin_raster_y2)
nrow(ncin_raster_y1); nrow(ncin_raster_y2)
ncol(ncin_raster_y1); ncol(ncin_raster_y2)

# 2. raster objects can be cropped usinga spatial object, in this case the spatial
#    object is a SpatialPolygonsDataFrame making it possible to cut Tanzania 
#    from the raster.
TZA_map <- getData('GADM', country = "TZA", level = 1)
raster_TZA_y1 <- crop(ncin_raster_y1, TZA_map)
raster_TZA_y2 <- crop(ncin_raster_y2, TZA_map)
str(raster_TZA_y1); str(raster_TZA_y2)


# 3. selecting the correct years and months - The long rainy season is (I think)
#    March, April and MAy in Tanzania
# x <- slot(slot(raster_TZA_y1, "data"), "values")[, c("X2010.03.16")] extract single collection of values
# y <- slot(slot(raster_TZA_y2, "data"), "values")
dimnames(x); dimnames(y)
Y1_Mar <- raster(raster_TZA_y1, layer = 99)
Y1_Apr <- raster(raster_TZA_y1, layer = 100)
Y1_May <- raster(raster_TZA_y1, layer = 101)
Y2_Mar <- raster(raster_TZA_y2, layer = 3)
Y2_Apr <- raster(raster_TZA_y2, layer = 4)
Y2_May <- raster(raster_TZA_y2, layer = 5)
Y1_precip <- stack(Y1_Mar, Y1_Apr, Y1_May)
Y2_precip <- stack(Y2_Mar, Y2_Apr, Y2_May)
nlayers(Y1_precip); nlayers(Y2_precip)
slot(Y1_precip, "layers"); slot(Y2_precip, "layers")
names(Y1_precip); names(Y2_precip)

# read in the geo data for year 1
geo.vars.y1 <- read.csv("./Analysis/Cleaned_data/geo_vars_y1.csv")

geo.var.mat <- select(geo.vars.y1, lon, lat)
vals <- extract(Y1_precip, geo.var.mat)
precip.vals <- data.frame(cbind(geo.var.mat, vals))

# join back into geo variable
geo.vars.y1 <- inner_join(geo.vars.y1, unique(precip.vals)) %>% rename(precip1 = X2009.03.16,
                                                                 precip2 = X2009.04.16,
                                                                 precip3 = X2009.05.16)
head(select(geo.vars.y1, hhid, lon, lat, precip1, precip2:precip3), 20)

# read in the geo data for year 2
geo.vars.y2 <- read.csv("./Analysis/Cleaned_data/geo_vars_y2.csv")

geo.var.mat <- select(geo.vars.y2, lon, lat)
vals <- extract(Y2_precip, geo.var.mat)
precip.vals <- data.frame(cbind(geo.var.mat, vals))

# join back into geo variable
geo.vars.y2 <- inner_join(geo.vars.y2, unique(precip.vals)) %>% rename(precip1 = X2011.03.16,
                                                                    precip2 = X2011.04.16,
                                                                    precip3 = X2011.05.16)
head(select(geo.vars.y2, y2_hhid, lon, lat, precip1, precip2:precip3), 20)
