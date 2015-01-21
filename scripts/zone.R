#' get zone for some analysis
HQSECA1 <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1HHDTA_E/SEC_A_T.dta",
                    convert.factors = TRUE)
geo.vars <- read.dta("./Data/Tanzania/2008_09/Stata/TZNPS1_consdta/HH.Geovariables_Y1.dta",
                     convert.factors = TRUE)
geo.vars <- left_join(geo.vars, select(HQSECA1, hhid, region)) %>%
        rename(lat = lat_modified, lon = lon_modified)

geo.vars$zone[geo.vars$region %in% c("Kagera","Mwanza", "Mara")] <- "Lake"
geo.vars$zone[geo.vars$region %in% c("Shinyanga","Kigoma", "Tabora")] <- "Western"
geo.vars$zone[geo.vars$region %in% c("Arusha","Kilimanjaro", "Manyara", "Tanga")] <- "Northern"
geo.vars$zone[geo.vars$region %in% c("Singida","Dodoma")] <- "Central"
geo.vars$zone[geo.vars$region %in% c("Rukwa", "Mbeya","Iringa")] <- "Southern Highlands"
geo.vars$zone[geo.vars$region %in% c("Pwani","Morogoro", "Dar es salaam")] <- "Eastern"
geo.vars$zone[geo.vars$region %in% c("Lindi","Ruvuma", "Mtwara")] <- "Southern"
geo.vars$zone[geo.vars$region %in% c("KASKAZINI UNGUJA", "KUSINI UNGUJA", "MJINI/MAGHARIBI UNGUJA",
                                     "KASKAZINI PEMBA", "KUSINI PEMBA")] <- "Zanzibar"

geo.vars$zone <- factor(geo.vars$zone)
