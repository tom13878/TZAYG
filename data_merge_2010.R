# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# TZA 2010 merge - merge all year 2 databases into one database.
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# 1. set working directory
# setwd("D:\\Dijk158\\Dropbox\\Michiel research\\Micro_IPOP")
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")

library(dplyr)

# 2. set some options
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# 3. read in data
geo.vars <- read.csv("./Analysis/Cleaned_data/geo_vars_y2.csv")
hh.total <- read.csv("./Analysis/Cleaned_data/hh_total_y2.csv")
plot.IO <- read.csv("./Analysis/Cleaned_data/plot_IO_Y2.csv")
plot.vars <- read.csv("./Analysis/Cleaned_data/plot_vars_y2.csv")
fert.vars.winsor <- read.csv("./Analysis/Cleaned_data/fert_vars_winsor_y2.csv")
output.maize <- read.csv("./Analysis/Cleaned_Data/output_maize_y2.csv")

# 4. merge data into a single file

database.2010 <- left_join(geo.vars, hh.total)
database.2010 <- inner_join(database.2010, plot.vars)
# database.2010 <- inner_join(database.2010, plot.IO) do something with zaocode
database.2010 <- left_join(database.2010, fert.vars.winsor)
database.2010 <- left_join(database.2010, output.maize)

# 5. save as a final database
# write.csv(database.2010, "./Analysis/Cleaned_Data/database_2010.csv)









# Combine Plot level data, only select Maize plots
Plot.tot<-subset(Plot, zaocode=="Maize")
Plot.tot<-merge(Plot.tot, Plot2, by=c("y2_hhid", "plotnum"), all.x=TRUE)
Plot.tot<-merge(Plot.tot, OutputI.calc, by=c("y2_hhid", "plotnum"), all.x=TRUE)

# Compare main crop, Maize share and number of crops
check<-Plot.tot[c("y2_hhid","OutputkgOld", "OutputkgNew", "plotnum","Maincrop", "NumbofCrops", "MaizeShare", "Multicropping", "InterCrop")]
check<-check[which(check$Maincrop!="Maize" & check$MaizeShare>25),]
check<-check[complete.cases(check),]
#Plot.tot<-merge(Plot.tot, Plot3, by=c("y2_hhid", "plotnum"), all.x=TRUE)
#Plot.tot$Tree[is.na(Plot.tot$Tree)]<-"NO" # set tree to zero 
#Plot.tot$Tree<-factor(Plot.tot$Tree)

# Combine all data at various levels
Total.TZA<-merge(Plot.tot,HH, by=c("y2_hhid"), all.x=TRUE)
Total.TZA<-merge(Total.TZA, Com, by=c("y2_hhid"), all.x=TRUE)
Total.TZA<-merge(Total.TZA, EAs[,c(1,7,10,11)], by=c("ea_id"), all.x=TRUE)

# Save file
save(Total.TZA,file="Data/Processed/Total.TZA.RData")
summary(Total.TZA)