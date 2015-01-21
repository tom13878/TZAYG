
# TZA 2010 merge - merge all year 2 databases into one database.
# include a year variable

# 1. set working directory
# setwd("D:\\Dijk158\\Dropbox\\Michiel research\\Micro_IPOP")
setwd("M:/cleaned_data/2010")

library(dplyr)

# 2. set some options
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# 3. read in data 

hh.total <- read.csv("./household.csv")
plot.vars <- read.csv("./plot_variables.csv")
fert.vars <- read.csv("./fertilizer_variables.csv")
output.maize <- read.csv("./output_maize_y2.csv")

# 4. merge data into a single file

database.2010 <- left_join(plot.vars, fert.vars)
database.2010 <- left_join(database.2010, output.maize)
database.2010 <- left_join(database.2010, hh.total)

database.2010$year <- 2010

# 5. save as a final database

write.csv(database.2010, "./database_2010.csv", row.names = FALSE )


