# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# TZA 2008 merge - merge all year 1 databases into one database.
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# 1. set working directory
# setwd("D:\\Dijk158\\Dropbox\\Michiel research\\Micro_IPOP")
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")

# 2. set some options
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# 3. read in data
geo.vars <- read.csv("./Analysis/Cleaned_data/geo_vars_y1.csv")
hh.total <- read.csv("./Analysis/Cleaned_data/hh_total_y1.csv")
plot.IO <- read.csv("./Analysis/Cleaned_data/plot_IO_Y1.csv")
plot.vars <- read.csv("./Analysis/Cleaned_data/plot_vars_y1.csv")
fert.vars.winsor <- read.csv("./Analysis/Cleaned_data/fert_vars_winsor_y1.csv")
output.maize <- read.csv("./Analysis/Cleaned_Data/output_maize_y1.csv")
 
# 4. merge data into a single file
database.2008 <- left_join(geo.vars, hh.total)
database.2008 <- inner_join(database.2008, plot.vars)
# database.2008 <- inner_join(database.2008, plot.IO) do something with zaocode
database.2008 <- left_join(database.2008, fert.vars.winsor)
database.2008 <- left_join(database.2008, output.maize)


# 5. save as a final database

# I. combine data
#    1. subset the plot input Output data for maize plotsPlot level input-output data with plot level indicators and output data for Maize plots
# Plot.tot <- PlotIO[PlotIO$zaocode == "Maize", ]
# Plot.tot <- merge(Plot.tot, PlotInd, by = c("hhid", "plotnum"))
# Plot.tot <- merge(Plot.tot, Output.Maize, by = c("region", "hhid", "plotnum"))
## Combine all data at various levels - Finish Line
# Total.TZA.Y1 <- merge(Plot.tot,HH1, by = c("hhid"), all.x = TRUE)
# Total.TZA.Y1 <- merge(Total.TZA.Y1, ComInd[, -c(3:6, 54)], by = c("hhid", "region"), all.x = TRUE)
# summary(Total.TZA.Y1)                  

# save(Total.TZA.Y1, file = "./Analysis/TZA 2008/Total.TZA.Y1.RData")          