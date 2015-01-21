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


coords.fwt <- read.fwf("./isd-history.txt",
                       widths = c(6, 1, 5, 1, 30, 2, 3, 2, 1, 7, 1 , 7, 1, 8, 1, 8, 1, 8),
                       sep = ";", skip = 21, fill = TRUE)
coords <- data.frame(ID = paste(as.factor(coords.fwt[, 1])), WBAN = paste(as.factor(coords.fwt[, 3])),
                     Station.Name = paste(as.character(coords.fwt[, 5])), Country = paste(as.character(coords.fwt[,6])),
                     Lat=as.numeric(paste(coords.fwt$V10)),Lon=as.numeric(paste(coords.fwt$V12)),
                     BEGIN = paste(as.character(coords.fwt$V16)), END = paste(as.character(coords.fwt$V18)))

TZA.coords <- subset(coords, Country == "TZ")
TZA.map <- getData('GADM', country = "TZA", level = 1)
TZA.coords.mat <- as.matrix(select(TZA.coords, Lon, Lat))
plot(TZA.map)
points(TZA.coords.mat, col = "blue", pch = 19)

TZA.locations <- subset(coords, Country == "TZ")

get.file.names <- function(df, year) {
  files <- paste(df$ID, df$WBAN, paste(year, ".op.gz", sep = ""), sep = "-")
}

check.exists <- function(fil) {
  files <- paste(paste(getwd(), "/Data/Files/", fil, sep = ""))
  file.exists(files)
}

files.y1 <- get.file.names(TZA.locations, "2008")[check.exists(get.file.names(TZA.locations, "2008"))]
# file.y2 <- get.file.names(TZA.locations, "2010")[check.exists(get.file.names(TZA.locations, "2010"))]

get.precip.data <- function(file.vec, classes){
  pre.dat <- data.frame(station = character(), date = character(), precip = numeric())
  for (i in 1:length(file.vec)) {
    data <- read.table(gzfile(paste(getwd(), "/Data/Files/", file.vec[i], sep = ""), open = "rt"),
                       sep = "", header = FALSE, skip = 1, colClasses = classes)
    data <- select(data, station = V1, date = V3, precip = V20)
    pre.dat <- rbind(pre.dat, data)  
  }
  pre.dat
}

classes <- c(rep("factor",2), rep("character", 1), rep("numeric",14), rep("factor",3), rep("numeric",2))
precip.y1 <- get.precip.data(files.y1, classes)
precip.y1$date <- as.POSIXlt(strptime(precip.y1$date, format = "%Y%m%d"))
long.rains <- subset(precip.y1, date >= as.POSIXlt("2008-03-01") & date <= as.POSIXlt("2008-05-31"))


````````````````````````````````````````````````````````````````````````````````````````````````````
TZA.coords$BEGIN <- as.POSIXlt(strptime(TZA.coords$BEGIN, format = "%Y%m%d"))
TZA.coords$END <- as.POSIXlt(strptime(TZA.coords$END, format = "%Y%m%d"))
complete.station <- subset(TZA.coords, END >= 2010-03-01)
complete.station$count <- complete.station$END - complete.station$BEGIN
# next stage is to select the stations for which we have data covering the correct period of time.
# then plot those stations to see which regions we have data for
# Then decide what to do next!!






     