# TZA fertilizer assessment
# R code to obtain rainfall data from Noaa

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("RCurl", "R.utils", "GSIF", "XML", "plotKML", "raincpc")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
# Note that rainfall data will be saved automatically in the wd.
#wd.path <- "D:\\Dijk158\\Dropbox\\Michiel_research\\Micro_IPOP\\Data\\Spatial\\Rainfall\\"
wd.path <- "D:\\Data\\IPOP\\Rainfall"
# SOURCE FUNCTIONS

# ANALYSIS
# Obtain data
iso3c <- "TZA"
data.path = paste(wd.path, "/", iso3c, sep="") 
if (!file.exists(data.path)) dir.create(path = data.path)
setwd(data.path)
# Download data
# cpc_get_rawdata(2011, 3, 1, 2011, 5, 31) 

# Read data, add projection and change coordinates to -180/180 instead of 0/360
# The 0/360 projection is often used in CC models and data but not relevant here.
prep.rainfall.f <- function(y, m, d){
  layer.date <- paste("Date", y, m, d, sep="_")
  TMP <- cpc_read_rawdata(y, m, d)
  TMP <- rotate(TMP)
  standardproj<-"+proj=longlat +datum=WGS84"
  projectRaster(TMP, crs=standardproj) # check if this works and if necessary because it might take a long time
  names(TMP) <- layer.date 
  return(TMP)
}

# Read filenames and get dates
filenames <- list.files()
# Select bin datafiles and get date information from binfiles
bindates <- filenames[grep(pattern=".bin", filenames)] %>% str_extract(., "\\d+")
Rainfall <- lapply(bindates, function(x) {
                prep.rainfall.f(as.numeric(str_sub(x, 1, 4)),
                                as.numeric(str_sub(x, 5, 6)),
                                as.numeric(str_sub(x, 7, 8))
                                )}) %>%
                do.call(stack,.)
writeRaster(Rainfall, "DailyRainfall30110301_20110531", overwrite=TRUE)
Rainfall <- brick("DailyRainfall30110301_20110531")

# Crop TZA from world rainfall data map
TZA.map <- getData('GADM', country="TZA", level=0)
TZA.map <- spTransform(TZA.map, CRS("+proj=longlat +datum=WGS84"))
Rainfall.TZA <- crop(Rainfall, TZA.map)
plot(Rainfall.TZA)
