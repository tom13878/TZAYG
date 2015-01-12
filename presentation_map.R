# tanzania yield map

setwd('C:/Users/morle001/Dropbox/Micro_IPOP')

library(foreign)
library(raster)
library(plyr)
library(ggplot2)
library(Grid2Polygons)
library(rgdal)
library(dplyr)

# A. read in raw data files
geo.vars <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta",
                     convert.factors = TRUE) %>% select(y2_hhid, lon = lon_modified, lat = lat_modified)
AQSEC4A <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2AGRDTA/AG_SEC4A.dta",
                                         convert.factors = TRUE)
areas <- read.dta('./Data/Plot_size/areas_tza_y2_imputed.dta') %>%
        select(y2_hhid = case_id, plotnum, area_gps_orig, area_gps_mi_50)

# subset for maize
maize <- filter(AQSEC4A, zaocode == 11)

maize <- select(maize, y2_hhid, plotnum, total.plot = ag4a_01, crop.share = ag4a_02,
                output.kg = ag4a_15) %>% filter(!(is.na(output.kg)), !(output.kg == 0))

areas <- select(areas, y2_hhid, plotnum, area_gps_orig, area_gps_mi_50) %>% filter(!is.na(area_gps_mi_50))

maize.area <- left_join(maize, areas)
summary(maize.area)

all(maize.area$y2_hhid %in% geo.vars$y2_hhid)
maize.area.coords1 <- left_join(maize.area, select(geo.vars, y2_hhid, lon, lat))
maize.area.coords2 <- maize.area.coords1[complete.cases(select(maize.area.coords1, output.kg, area_gps_mi_50)), ] 

# calculate yield
maize.area.coords3 <- transform(maize.area.coords2, yield = output.kg/area_gps_mi_50)
maize.area.coords4 <- ddply(maize.area.coords3, .(lon, lat), summarize, avg = mean(yield, na.rm = TRUE))
maize.area.coords5 <- filter(maize.area.coords4, !(avg > 2500)) # get rid of strange values!

# make a plot in ggplot2
TZA_map <- getData('GADM', country = "TZA", level = 1) ## get map of Tanzania
tf <- fortify(TZA_map)

tz1 <- ggplot(tf) + geom_polygon(data=tf, aes(x=long, y=lat, group=group), fill="grey40", colour="black", alpha = .3, size = .1)
tz2 <- tz1 + geom_point(data = maize.area.coords5, aes(x=lon, y=lat, size=avg), colour="red")
tz3 <- tz2 + coord_map("mercator") + scale_size_continuous(name = 'maize yield\n(kg/ha)')
tz4 <- tz3 + guides(size=guide_legend(reverse=TRUE))
ggsave("./Analysis/Maps_tables/tan_map_yield.pdf", width = 20, height = 15, units = "cm")

# get the AEZ map
AEZ16 <- readGDAL("./Data/Spatial/AEZ_CODE_2/AEZ_CODE.asc")
AEZ16Polygon <- Grid2Polygons(AEZ16)
AEZ16Data <- AEZ16Polygon@data
names(AEZ16Data) <- "AEZ16Code"
AEZ16Data$id <- row.names(AEZ16Data)
AEZ16fort <- fortify(AEZ16Polygon)
AEZ16fort <- merge(AEZ16fort, AEZ16Data, by=c("id"))
AEZ16Code <- read.csv("./Data/Spatial/AEZ_CODE_2/AEZ16Code.csv")
AEZ16fort <- merge(AEZ16fort, AEZ16Code, by=c("AEZ16Code"))

# Select tanzania
#TZA.google.map<-get_map(location="Tanzania", zoom=6, source=c("google"))
#TZA.google.map<-ggmap(TZA.google.map, extent="device")
load(".\\Data\\Spatial\\TZA maps\\TZA_adm0.Rdata")
#projection(TZA.GADM) # projection is the same as google maps: WGS84.
gadm<-fortify(gadm)
AEZ16fort.TZA <- subset(AEZ16fort, long >29 & long<40.4 & lat>-12 & lat< -.7)

TZA.map <- ggplot()+geom_polygon(data=AEZ16fort.TZA, aes(x=long, y=lat, group=group, fill=AEZ16Name_short))
TZA.map2 <- TZA.map + geom_polygon(data=tf, aes(x=long, y=lat, group = group), alpha = 0, colour = 'black')
TZA.map3 <- TZA.map2 + geom_point(data = maize.area.coords5, aes(x=lon, y=lat, size=avg), colour="black")
TZA.map4 <- TZA.map3 + coord_map("mercator") + scale_size_continuous(name = 'maize yield\n(kg/ha)') 

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#TZA.map<-TZA.map+scale_fill_brewer(palette="Set1")
TZA.map5 <- TZA.map4 + scale_fill_manual(values=cbPalette, name="AEZ")
ggsave("./Analysis/Maps_tables/tan_map_yield_AEZ.pdf", width = 20, height = 15, units = "cm")
