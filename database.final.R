#' final merge for Tanzania database combining both the years in one. inputs are
#' the databases for the two years and the output is a larger database made from
#' stacking year 1 on top of year 2. 
setwd('M:/cleaned_data')

database.2008 <- read.csv('./2008/database_2008.csv')
database.2010 <- read.csv('./2010/database_2010.csv')

database.final <- rbind(database.2008, database.2010)

write.csv('./', row.name = FALSE)