# -------------------------------------
#' make something mapable for the GYGA guys
#' GYGA guys need some outcomes. Use 
#' data on output and areas in waves two
#' and three to make some nice maps of 
#' yields and stuff.
#' ------------------------------------
setwd("M:/TZAYG")

library(haven)
library(ggplot2)
library(ggmap)
library(dplyr)

# ----------wave 2---------------------
output_w2 <- read.csv("data/2010/plot_output_w2.csv")

# filter for only maize
output_maize_w2 <- subset(output_w2, zaocode=="maize" & total_plot=="YES")

# grab areas from areas file
areas_w2 <- read.csv("data/2010/areas_w2.csv")

# join areas with the output data on just maize plots and remove any areas that
# are NA for imputed area
output_maize_w2 <- left_join(output_maize_w2, areas_w2) %>% filter(!is.na(area_gps_imputed))

# need to find out what the unit is for the area measurement. 
# seems to be some very large and messy values
qplot(output_kg, data=output_maize_w2)

# for now just get rid of anything that is too big (> 5000 say)
output_maize_w2$output_kg[output_maize_w2$output_kg > 5000] <- NA # lost 2 values

# calculate the yield on each plot
output_maize_w2 <- mutate(output_maize_w2, maize_yield=output_kg/area_gps_imputed)

# need to check this because it is strange!
qplot(maize_yield, data=output_maize_w2)

# for now just get rid of anyyield that is too crazy (> 6000 say)
output_maize_w2$maize_yield[output_maize_w2$maize_yield > 6000] <- NA # lost 9 values

# read in gps coordinates to add to a map
filepath <- "C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta"
gps <- read_dta(filepath)

# change the y2_hhid variable class to a character vector
output_maize_w2$y2_hhid <- as.character(output_maize_w2$y2_hhid)

# add the GPS to the output and yield values. Missing lon and lat for a lot of values
output_maize_w2 <- left_join(output_maize_w2, select(gps, y2_hhid,ea_id, lon=lon_modified, lat=lat_modified))
table(is.na(output_maize_w22$ea_id))

# could be ghost households! CREEPY!
all(output_maize_w2$y2_hhid %in% gps$y2_hhid) #FALSE
all(output_maize_w2$y2_hhid %in% as.character(as.numeric(gps$y2_hhid))) #TRUE
gps$y2_hhid <- as.character(as.numeric(gps$y2_hhid))

# and try again
output_maize_w2 <- left_join(output_maize_w2, select(gps, y2_hhid,ea_id, lon=lon_modified, lat=lat_modified))
table(is.na(output_maize_w2$ea_id))

# now we have gps for all of the differnt points it is time to get some nice 
# maps. Load the raster package and get a few nice maps of Tanzania. Probably
# map 3 is best because it is most detailed

library(raster)

TZA_map <- getData('GADM', country = "TZA", level = 1)
TZA_map2 <- getData('GADM', country = "TZA", level = 2)
TZA_map3 <- getData('GADM', country = "TZA", level = 3)


# have a look at what is inside these shapefiles
head(TZA_map@data)
head(TZA_map2@data)
head(TZA_map3@data)

TZA_map3[1,]
nrow(TZA_map3) # 2800 polygons referring to the geographical locations. 
TZA_map3[2800,]

str(TZA_map[1,])

# make a spatial points object
gps_mat <- cbind(output_maize_w2$lon, output_maize_w2$lat)
row.names(gps_mat) <- 1:nrow(gps_mat)

# check the projection in TZA_map3 and use it as the projection in the spatial
# points object so that the map and points match up
proj4string(TZA_map3)
llCRS <- CRS(proj4string(TZA_map3))
sp <- SpatialPoints(gps_mat, llCRS)

# overlay the map with the points
sel <- over(sp, TZA_map3)

# column bind everything together and find the average per each of these
# districts (actually one level below districts)
complete <- cbind(output_maize_w2, sel)
complete <- select(complete, NAME_3, maize_yield)
complete <- unique(complete)
by_district <- group_by(complete, NAME_3) %>% summarise(mean_yield=mean(maize_yield, na.rm=TRUE), N=n())
by_district <- by_district[!by_district$NAME_3=="n.a. ( 1030)",]
by_district <- by_district[by_district$mean_yield < 2000,]

# left join this with the spatial dataframe
TZA_map3@data <- left_join(TZA_map3@data, by_district)

# now try and fortify the data so that GGplot can use it
tf <- fortify(TZA_map3, region="PID")
tf <- rename(tf, PID=id)
TZA_map3@data$PID <- as.character(TZA_map3@data$PID)
lf <- left_join(tf, TZA_map3@data)

# try plotting the data - fingers crossed!
gg <- ggplot(lf) + geom_polygon(aes(long, lat, group = group, fill = mean_yield), color="#0e0e0e", size=0.15)

# OK that kinda worked but now you need a better looking map
# try another way of plotting
devtools::source_gist("33baa3a79c5cfef0f6df") # random map gist from github
gg <- gg + theme_map()
gg <- gg + scale_fill_gradientn(colours=c("#ffffff", brewer.pal(n=9, name="YlOrRd")),
                           na.value="#ffffff")

# try another plot but this time match up on region not district. So really
# just change above code by replacing NAME_3 with NAME_2

complete <- cbind(output_maize_w2, sel)
complete <- select(complete, NAME_2, maize_yield)
complete <- unique(complete)
by_comm <- group_by(complete, NAME_2) %>% summarise(mean_yield=mean(maize_yield, na.rm=TRUE), N=n())
by_comm <- by_comm[!by_comm$NAME_2=="n.a. ( 1030)",]
by_comm <- by_comm[by_comm$mean_yield < 2000,]

# relaod the TZA_map3 again
TZA_map3 <- getData('GADM', country = "TZA", level = 3)

# left join this with the spatial dataframe
TZA_map3@data <- left_join(TZA_map3@data, by_comm)

# now try and fortify the data so that GGplot can use it
tf <- fortify(TZA_map3, region="PID")
tf <- rename(tf, PID=id)
TZA_map3@data$PID <- as.character(TZA_map3@data$PID)
lf <- left_join(tf, TZA_map3@data)

# and the same map as before
gg <- ggplot(lf) + geom_polygon(aes(long, lat, group = group, fill = mean_yield), color="#0e0e0e", size=0.15)
gg <- gg + theme_map()
gg <- gg + scale_fill_gradientn(colours=c("#ffffff", brewer.pal(n=9, name="YlOrRd")),
                                na.value="#ffffff")



# still need to clean up data, seems that a single outlier can make a big
# difference in these plots
