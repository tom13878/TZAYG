# PROJECT: IPOP
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to link spatial data with LSMS-ISA TZA Household data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr", "tidyr")
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

# PREPARE LSMS SPATIAL DATAPOINTS
# Get y2_hhid-GIS link
LSMSpath <- "W:\\LEI\\Internationaal Beleid  (IB)\\Projecten\\2285000066 Africa Maize Yield Gap\\SurveyData\\Tanzania"

HH.geo <- read.dta(paste(LSMSpath, "./2010/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta", sep="\\"),
                   convert.factors = TRUE)
plot.geo <- read.dta(paste(LSMSpath, "./2010/Stata/TZNPS2GEODTA/Plot.Geovariables_Y2.dta", sep="\\"),
                  convert.factors = TRUE)

# Create list of plots, hh, eas and coordinates
geo.base <- left_join(plot.geo, HH.geo) %>%
                      transmute(y2_hhid, ea_id, plotnum, lat = lat_modified, lon = lon_modified)

# Create spatial points 
standardproj<-"+proj=longlat +datum=WGS84"
geo.coord <- geo.base %>% 
              dplyr::select(lon, lat) %>%
              SpatialPoints(., proj4string=CRS(standardproj))

# RAINFALL DATA
# Extract rainfall data and link to GIS coordinates of plots/housholds
rainfallpath <- "D:\\Data\\IPOP\\Rainfall\\TZA"
rainfall <-brick(paste(rainfallpath, "DailyRainfall30110301_20110531", sep="\\"))

# extract data
geo.rainfall <- raster::extract(rainfall, geo.coord) %>%
    cbind(geo.base,.)

# SOIL DATA
# Load raster stack with soil data
soilpath <- "D:\\Data\\IPOP\\AFSIS\\Download\\TZA"
Soil <- brick(paste(soilpath, "TZA_SoilData", sep="/"))
plot(Soil[[4]])

Soil[[4]]
check <- subset(geo.soil, TZA_af_BLD_T__M_sd1_250m==0)
points(check$lon, check$lat, col='black', pch=20, cex=0.75)
# extract data
geo.soil <- raster::extract(Soil, geo.coord) %>%
  cbind(geo.base,.)

# Calculate carbon stock
# standard depths specified in GSIF package
sds <- get("stsize", envir = GSIF.opts)

geo.carbonstock <- geo.soil %>%
                    gather(raster, value, -y2_hhid:-lon) %>%
                    mutate(variable = str_sub(raster, 8, 10), sds = str_sub(raster, -8, -6)) %>%
                    dplyr::select(-raster) %>%
                    spread(variable, value) %>%
                    mutate(cs = OCSKGM(ORC, BLD, CRF, 5, 10, 100, 5))
                    group_by(sds) %>%


                    

# Carbon stock for three depths
for(j in c(1,2,3)){
    Country1km@data[,paste("SOC_sd", j, "_M", sep="")] <- OCSKGM(Country1km@data[,paste("ORCDRC_sd", j, "_M_",iso3c, sep="")], Country1km@data[,paste("BLD_sd", j, "_M_", iso3c, sep="")], Country1km@data[,paste("CRFVOL_sd", j, "_M_", iso3c, sep="")], sds[j]*100) # sds from m to cm
    #Country1km@data[,paste("SOC_sd2", j, "_M", sep="")] <- soc(Country1km@data[,paste("ORCDRC_sd", j, "_M_",iso3c, sep="")], Country1km@data[,paste("BLD_sd", j, "_M_", iso3c, sep="")], Country1km@data[,paste("CRFVOL_sd", j, "_M_", iso3c, sep="")], sds[j])
  }
  ## aggregate values:
  Country1km$SOC <- rowSums(Country1km@data[,paste("SOC_sd", c(1,2,3), "_M", sep="")], na.rm=TRUE)
  return(Country1km)
}



CS.TZA<-CarbonStock.f("TZA")

Plot.CS.f<-function(file, iso3c){
  # cut of outliers
  rg <- quantile(file$SOC, c(0.01, 0.99), na.rm=TRUE)
  at <- expm1(seq(log1p(rg[1]), log1p(rg[2]), length.out=20))
  
  # get polygoon of country
  dlpath = paste(getwd(), "/", iso3c, sep="") 
  if (!file.exists(dlpath)) dir.create(path = dlpath)
  if(file.exists(paste(iso3c, "_adm0.Rdata", sep=""))){
    load(paste(iso3c, "_adm0.Rdata", sep=""))
  }else{
    country.sp=getData('GADM', country=iso3c, level=0, path=dlpath)
  }
  # change projection to standard WGS84
  country.sp<- spTransform(country.sp, CRS("+proj=longlat +datum=WGS84"))
  country.sp <- as(country.sp, "SpatialLines")
  bnd <- list("sp.lines", country.sp)
  
  data(R_pal)
  file$SOCf <- ifelse(file$SOC<rg[1], rg[1], ifelse(file$SOC>rg[2], rg[2], file$SOC))
  spplot(file["SOCf"], at=at, col.regions=R_pal[["soc_pal"]], sp.layout=bnd, scales=list(draw=TRUE), main="Total soil carbon stock (0\22630 cm) in kg per square meter")
}

Plot.CS.f(CS.TZA, "TZA")


# GYGA DATA
GYGApath <- "D:\\Dijk158\\Dropbox\\Michiel_research\\Micro_IPOP\\Data\\Spatial\\GYGA"

# Get GYGA map
dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA.Africa<-readOGR(dsn, layer = "CZ_AFRGYGACNTRY")

# Cut out TZA from GYGA map
GYGA.TZA<-GYGA.Africa[GYGA.Africa$REG_NAME=="Tanzania",]
GYGA.Africa <- spTransform(GYGA.Africa, CRS("+proj=longlat +datum=WGS84"))

# Select data for maize
# Get GYGA
GYGA.TZA.yield.data <- read.xls(paste(GYGApath, "GygaTanzania.xlsx", sep="\\"), , sheet=2)
GYGA.TZA.maize.data <- subset(GYGA.TZA.yield.data, CROP=="Rainfed maize")

# Link yield gap data
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.
GYGA.TZA.data <- as(GYGA.TZA, "data.frame")
GYGA.TZA.data$id <-row.names(GYGA.TZA.data)
GYGA.TZA.data <- merge(GYGA.TZA.data, GYGA.TZA.maize.data[,c(2:8)], by.x=c("ID"), by.y=c("CLIMATEZONE_ID"), all.x=TRUE, sort=FALSE)
row.names(GYGA.TZA.data)<-GYGA.TZA.data$id
GYGA.TZA <- SpatialPolygonsDataFrame(as(GYGA.TZA, "SpatialPolygons"),
                                     data=GYGA.TZA.data)

# Extract data
geo.GYGA <- raster::extract(GYGA.TZA, geo.coord) %>%
  dplyr::select(CROP, YA, YW, YW.YA, YP, YP.YA) %>%
  cbind(geo.base,.)


# BIND ALL SPATIAL INFORMATION
