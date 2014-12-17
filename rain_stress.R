#````````````````````````````````````````````````````````
# test script for calcualting a precipitation value
# TODO (tom morley): loop over files and extract precip values
#                    use values to calculate a stress variable
#                    find and plot the weather stations for which
#                    data is accesible
# ````````````````````````````````````````````````````````


library(raster)
library(XML)
library(dplyr)

setwd("D:/precip")


coords.fwt <- read.fwf("./isd-history.txt",widths=c(6, 1, 5, 1, 30, 2, 3, 2, 1, 7, 1 , 7, 1, 8, 1, 8, 1, 8),sep=";",skip=21,fill=T)
coords <- data.frame(ID = paste(as.factor(coords.fwt[, 1])), WBAN = paste(as.factor(coords.fwt[, 3])),
                     Station.Name = paste(as.character(coords.fwt[, 5])), Country = paste(as.character(coords.fwt[,6])),
                     Lat=as.numeric(paste(coords.fwt$V10)),Lon=as.numeric(paste(coords.fwt$V12)),
                     BEGIN = paste(as.character(coords.fwt$V16)), END = paste(as.character(coords.fwt$V18)))

# just have a look at some of the TZA values and try loading in a single TZA file
TZA.locations <- subset(coords, Country == "TZ")

get.file.names <- function(df) {
  files <- paste(df$ID, df$WBAN, "2008.op.gz", sep = "-")
}

check.exists <- function(x) {
  log <- ifelse(file.exists(x), TRUE, FALSE)
  x <- x[log]
}

files <- check.exists(get.file.names(TZA.locations))

convert.dates <- function(df) {
  # look back over dates function from lectures 
}

head(coords)
# country code for tanzania is TZ
TZA.coords <- subset(coords, Country == "TZ")
TZA.map <- getData('GADM', country = "TZA", level = 1)
TZA.coords.mat <- as.matrix(select(TZA.coords, Lon, Lat))
plot(TZA.map)
points(TZA.coords.mat, col = "blue", pch = 19)

# now you want to create a loop which gets the right files containing the correct data
readLines(gzfile("D:/precip/Data/Files/637330-99999-2008.op.gz"), 10)
x <- read.table(gzfile("D:/precip/Data/Files/638940-99999-2008.op.gz", open = "rt"), sep = "", header = FALSE, skip = 1, colClasses = classes)

dir.create(paste(getwd(),"/Data/Files",sep=""))
untar(paste(getwd(),"/Data/","gsod_2008.tar",sep=""),exdir=paste(getwd(),"/Data/Files",sep=""))

classes <- c(rep("factor",3),rep("numeric",14),rep("factor",3),rep("numeric",2))
data.test <- read.table(gzfile("D:/precip/Data/Files/010010-99999-2008.op.gz", open="rt"), sep="",header=FALSE,skip=1,colClasses=classes)

     