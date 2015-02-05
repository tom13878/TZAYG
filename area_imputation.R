"""
One issue with the data is that the GPS measurements for plot area in the
2008 data are incomplete. One solution is to use the gps meausurements that do
exist for 2008 and those in 2010 to impute values for the missing measurements
"""

# probably a good idea to get rid of all values above a certain amount as well
# so called top coding to reduce the sensitivity of a regression model to the
# large outliers that we may have in the data

random_imp <- function(a){
        missing <- is.na(a)
        observed <- a[!(missing)]
        a[missing] <- sample(observed, size = sum(missing), replace = TRUE)
}


# area imputation
sr <- filter(db0, year == 2008) %>% select(region, area.gps)
ddply(sr, .(region), summarize, prop = (sum(is.na(area.gps))/length(area.gps))*100)