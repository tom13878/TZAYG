# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# TZA 2008 merge - merge all year 1 databases into one database.
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# 1. set working directory
# setwd("D:\\Dijk158\\Dropbox\\Michiel research\\Micro_IPOP")
setwd('M:/cleaned_data/2008')
library(dplyr)

# 2. set some options
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# 3. read in data
hh.total <- read.csv("./household.csv")
plot.vars <- read.csv("./plot_variables.csv")
fert.vars <- read.csv("./fertilizer_variables.csv")

# still to add output section
# output.maize <- read.csv("./Analysis/Cleaned_Data/output_maize_y1.csv")
 
# 4. merge data into a single file

database.2008 <- left_join(plot.vars, fert.vars)
# database.2008 <- left_join(database.2008, output.maize)
database.2008 <- left_join(database.2008, hh.total)

# 5. save as a final database
   