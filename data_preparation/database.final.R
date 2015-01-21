#' final merge for Tanzania database combining both the years in one. inputs are
#' the databases for the two years and the output is a larger database made from
#' stacking year 1 on top of year 2. 
#' 
#' Still need to change spellings between years for regions

setwd('M:/cleaned_data')
library(dplyr)

database.2008 <- read.csv('./2008/database_2008.csv')
database.2010 <- read.csv('./2010/database_2010.csv')

x <- !(names(database.2008) %in% names(database.2010))
names(database.2008)[x]

y <- !(names(database.2010) %in% names(database.2008))
names(database.2010)[y]

database.2008 <- select(database.2008, -(N_share:K_share), - fert, - fert.kg, - fert.type, - pest.unit, - fert.price)
database.2010 <- select(database.2010, - main.crop, -schl)
database.2010 <- rename(database.2010, hhid = y2_hhid)

x <- order(names(database.2008))
y <- order(names(database.2010))
database.2008 <- database.2008[, x]
database.2010 <- database.2010[, y]

database.2008 <- select(database.2008, hhid, plotnum, region, everything())
database.2010 <- select(database.2010, hhid, plotnum, region, everything())

database.final <- rbind(database.2008, database.2010)

write.csv('./', row.name = FALSE)

