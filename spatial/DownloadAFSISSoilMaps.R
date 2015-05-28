# PROJECT: IPOP
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# # http://www.isric.org/data/AfSoilGrids250m
# Purpose: Download spatial precipitation, soil, etc data and prepare country files for yield analysis
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages<- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
lapply(SpatialPackages, library, character.only = TRUE)

# SET WORKING DIRECTORY
wdpath<-"D:\\Data\\IPOP\\AFSIS\\Download"
setwd(wdpath)

# DOWNLOAD AFRICA SOILGRIDS
# Download Soilgrids: http://gsif.isric.org/doku.php?id=wiki%3Atutorial_soilgrids
# soilgrids:soilgrids are password and username
sg.ftp <- "ftp://soilgrids:soilgrids@ftp.soilgrids.org/data/AF/recent/"
filenames = getURL(sg.ftp, ftp.use.epsv = FALSE, dirlistonly = TRUE, userpwd = "soilgrids")
filenames = strsplit(filenames, "\r*\n")[[1]]
targetnames <- c("ORCDRC", "BLD", "CRFVOL")
targetdepth <- c("sd1", "sd2", "sd3")
targetfiles <-filenames[grep(pattern=paste(targetnames, collapse="|"), filenames)]
targetfiles <-targetfiles[grep(pattern=paste(targetdepth, collapse="|"), targetfiles)]
l_ply(targetfiles,function(x) if(!file.exists(x)){try(download.file(paste(sg.ftp, x, sep=""),x))})

# unzip tif files
tifFiles<-targetfiles[grep(pattern=".gz", targetfiles)]
l_ply(tifFiles, gunzip, skip=TRUE)
tifFiles<-llply(tifFiles,function(x) strsplit(x,".gz")[[1]][1])
llply(tifFiles,GDALinfo)

# SUBSET FILES FOR A TARGET COUNTRY AND WRITE AS RASTERSTACK
# Set target country
targetcountry <-"TZA"

# CREATE COUNTRY FOLDER AND SET WORKING DIRECTORY
wdpath<-"D:\\Data\\IPOP\\AFSIS\\Download"
countrypath = paste(getwd(), "/", targetcountry, sep="") 
if (!file.exists(countrypath)) dir.create(path = dlpath)
setwd(countrypath)

# Obtain country coordinates for target country
Get.country.coordinates.f <- function(iso3c){

  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, "_adm0.Rdata", sep="")
  if(file.exists(paste(countrypath, targetfile, sep="/"))){
    load(paste(countrypath, targetfile, sep="/"))
  } else {
    gadm=getData('GADM', country=iso3c, level=0, path=dlpath)
  }

  # change projection 
  #projection <- "+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +units=m +ellps=WGS84 +datum=WGS84"
  projection <- "+proj=longlat +datum=WGS84"
  country.sp <- spTransform(gadm, CRS(projection))
  coordinates <- as.vector(bbox(country.sp))
  return(coordinates)
}

coord <- Get.country.coordinates.f(targetcountry)

# Extract information for target country
gdal_setInstallation()
AfricaSoilFiles <- list.files(path = wdpath, pattern = "\\.tif$") 
CountrySoilData <- lapply(AfricaSoilFiles, function(x) {
                  gdalwarp(srcfile=paste(wdpath, x, sep="\\"), dstfile=paste(targetcountry, x, sep="_"), 
                  t_srs="+proj=longlat +datum=WGS84",
                  te=coord, overwrite=TRUE, output_Raster=TRUE, verbose=TRUE)
                }
              ) %>% do.call(stack,.)

# Write raster as brick
writeRaster(CountrySoilData, paste(countrypath, paste(targetcountry, "_SoilData", sep=""), sep="\\", overwrite=TRUE)
CountrySoilData <- brick(paste(countrypath, paste(targetcountry, "_SoilData", sep=""), sep="\\"))

###
# Define basefile, crop coordinates and projections
TZA=getData('GADM', country="TZA", level=0)
coord <- as.vector(bbox(TZA))
CRFVOLraster <-raster("af_CRFVOL_T__M_sd1_250m.tif")
ProjectionAFSIS <- "+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +units=m +ellps=WGS84 +datum=WGS84"
Baseprojection <- "+proj=longlat +datum=WGS84"
Targetcoordinates <- 

# option 1: using Rgdalwarp
Rgdalraster <-  gdalwarp(srcfile=CRFVOLraster, dstfile="TZA_Rgdalwarp.tif", 
                t_srs=Baseprojection,
                te=coord, overwrite=TRUE, output_Raster=TRUE, verbose=TRUE)
plot(Rgdalraster)

# Option 2: using Raster
TZA_new_projection <- spTransform(TZA, CRS(ProjectionAFSIS))
RasterRaster <- crop(CRFVOLraster, TZA_new_projection)
RasterRaster <- projectRaster(RasterRaster, crs=Baseprojection)
plot(RasterRaster)


geo.Rgdalraster <- raster::extract(testcrop, Targetcoordinates)
geo.RasterRaster <- raster::extract(testcrop, Targetcoordinates)
geo.compare <- cbind(geo.Rgdalraster, geo.RasterRaster)
