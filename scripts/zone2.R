y2commlink <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2COMDTA/y2commlink.dta" ,
                       convert.factors = TRUE)
geo.vars <- read.dta("./Data/Tanzania/2010_11/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta",
                     convert.factors = TRUE)
geo.vars <- left_join(geo.vars, select(y2commlink, y2_hhid, region = id_01)) %>%
        rename(lat = lat_modified, lon = lon_modified)

geo.vars$zone[geo.vars$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
geo.vars$zone[geo.vars$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
geo.vars$zone[geo.vars$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
geo.vars$zone[geo.vars$region %in% c("Singida","Dodoma")] <- "Central"
geo.vars$zone[geo.vars$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
geo.vars$zone[geo.vars$region %in% c("Pwani","Morogoro", "Dar es Salaam")] <- "Eastern"
geo.vars$zone[geo.vars$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
geo.vars$zone[geo.vars$region %in% c("Kaskazini Unguja", "Kusini Unguja", "Mjini Magharibi",
                                     "Kaskazini Pemba", "Kusini Pemba")] <- "Zanzibar"

geo.vars$zone <- factor(geo.vars$zone)
hhid.reg.zone <- select(geo.vars, y2_hhid, region, zone) %>% arrange(zone)
reg.zone <- unique(select(geo.vars, y2_hhid, region, zone)) %>% arrange(zone)
hhid.lon.lat <- select(geo.vars, y2_hhid, lon, lat)
# write.csv(geo.vars, "./Analysis/Cleaned_data/geo_vars_y2.csv", row.names = FALSE)
# write.csv(hhid.reg.zone, "./Analysis/Cleaned_data/hhid_reg_zone_y2.csv", row.names = FALSE)
# write.csv(reg.zone, "./Analysis/Cleaned_data/reg_zone_y2.csv", row.names = FALSE)
write.csv(hhid.lon.lat, "./Analysis/Cleaned_data/hhid_lon_lat_y2.csv", row.names = FALSE)