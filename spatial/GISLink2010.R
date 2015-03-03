# TZA fertilizer assessment
# R code to link spatial data with LSMS-ISA Household data

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("RCurl", "R.utils", "GSIF", "XML", "plotKML")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
#wdpath<-"W:/LEI/Internationaal Beleid  (IB)/Projecten/2271000300 IPOP 3 (new code in 2014-228250008)/Spatial analysis/Data/SoilGrids"
wdpath <- "D:\\Dijk158\\Dropbox\\Michiel_research\\Micro_IPOP"
setwd(wdpath)

# SOURCE FUNCTIONS

# ANALYSIS
# Get y2_hhid-GIS link
HH.geo <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta",
                   convert.factors = TRUE)
plot.geo <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/Plot.Geovariables_Y2.dta",
                  convert.factors = TRUE)

# Create list of plots, hh, eas and coordinates
geo.base <- left_join(plot.geo, HH.geo) %>%
                      transmute(y2_hhid, ea_id, plotnum, lat = lat_modified, lon = lon_modified)

# Create spatial points 
standardproj<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
geo.coord <- geo.base %>% 
              dplyr::select(lon, lat) %>%
              SpatialPoints(., proj4string=CRS(standardproj))

# Load raster stack with soil data
soil.var <- c(paste("ORCDRC_sd", c(1,2,3), "_M", sep=""), paste("BLD_sd", c(1,2,3), "_M", sep=""), paste("CRFVOL_sd", c(1,2,3), "_M", sep=""))
iso3c <-"TZA"
soil.path <- "\\Data\\Spatial\\SoilGrids\\Data"
soil.stack <- stack(paste(wdpath, stack.path, paste(soil.var, "_", iso3c,".tif", sep=""), sep="/"))

# Remove missing values (indicated by -9999 and 255) and values below zero
#cellStats(soil.stack, stat="range")
soil.stack[soil.stack==255 | soil.stack < 0] <- NA

# Extract soil data and link to GIS coordinates of plots/households/communities
geo.db <- extract(soil.stack, geo.coord) %>% 
          cbind(geo.base,.)

# Extract rainfall data and linkl to GIS coordinates of plots/housholds
rainfall.stack<-stack(paste"test")