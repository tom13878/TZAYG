# create a dataframe with the cropcodes in it for year 1 
setwd("c:/Users/morle001/Dropbox/Micro_IPOP")
library(foreign)
library(gdata)
library(dplyr)
# read in cropcodes and price data
colClasses <- c("character", "character", "factor", "factor", "factor", "numeric",
                "numeric", "numeric", "numeric", "factor", "factor")
crop.codes <- read.xls("Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet = 1)
plot.IO <- read.csv("./Analysis/Cleaned_data/plot_IO_Y1.csv", colClasses = colClasses) 

# check which zaocodes match up with cropname and which do not
zaocode <- unique(plot.IO$zaocode)
crop.name <- as.character(crop.codes$CropName)

z <- ifelse(zaocode %in% crop.name, TRUE, FALSE)
table(z)
zaocode[!z]
# [1] Sweet Potatos Simsim        <NA>          Chillies      Other         Irish potatos Sugar Cane    Cassava      
# [9] Cashewnut   

# change crop.codes to reflect the mispellings
# Simsim, the word for sesame in various Semitic languages. Also widely used in Arab-influenced East Africa
crop.name[crop.name == "Irish potatoes"] <- "Irish potatos"
crop.name[crop.name == "Casava"] <- "Cassava"
crop.name[crop.name == "Sweet Potatoes"] <- "Sweet Potatos"
crop.name[crop.name == "Chilies"] <- "Chillies"
crop.name[crop.name == "Cashew nut"] <- "Cashewnut"
crop.name[crop.name == "Sesame"] <- "Simsim"
crop.name[crop.name == "sugar Cane"] <- "Sugar Cane"

# merge with important variables and save to a single csv file
crop.codes <- data.frame(crop.name, crop.codes$zaocode, crop.codes$itemname, crop.codes$CashCrop) %>%
  rename(zaocode = crop.codes.zaocode, itemname = crop.codes.itemname, cash.crop = crop.codes.CashCrop)
# write.csv(crop.codes, "./Analysis/Cleaned_data/crop_codes_y1.csv", row.names = FALSE)

# now check prices itemname and crop.codes itemname
prices <- read.csv("./Analysis/Cleaned_Data/prices_winsor_y1.csv",
                   colClasses = c("factor","factor","numeric", "numeric", "numeric"))
prices.itemname <- unique(prices$itemname)
crop.codes.itemname <- unique(crop.codes$itemname)
# all the itemnames in the c rop codes file are present in the prices file, which means it
# is fine to use this information without any changes
z <- ifelse(crop.codes.itemname %in% prices.itemname, TRUE, FALSE)
table(z)

# Check the same thing for year 2 - ideally have one crop.codes file even if that means repeat
# entries with different spellings
crop.codes.y2 <- read.xls("Data/Tanzania/2010_11/Other/CropCodes.xlsx", sheet = 1)
colClasses <- c("character", "character", "factor", "factor", "factor", "factor", "factor",
                "factor", "numeric", "numeric", "numeric", "numeric", "factor", "factor")
plot.IO.y2 <- read.csv("./Analysis/Cleaned_data/plot_IO_y2.csv", colClasses = colClasses)

zaocode.y2 <- unique(plot.IO.y2$zaocode)
crop.name.y2 <- as.character(crop.codes.y2$CropName)

# the only value that is different is the NA value. So spelling is different across the two years
z <- ifelse(zaocode.y2 %in% crop.name.y2, TRUE, FALSE)
table(z)
zaocode.y2[!z]

# make a single file with both spellings in it
# select the variables you want from the old cropcodes file
crop.codes.y2 <- select(crop.codes.y2, crop.name = CropName, zaocode, itemname, cash.crop = CashCrop)
log <- crop.codes.y2$crop.name %in% c("Irish potatoes", "Casava", "Sweet Potatoes", "Chilies", "Cashew nut",
                                     "Sesame", "sugar Cane")
crop.codes <- rbind(crop.codes, crop.codes.y2[log, ])

# save file
write.csv(crop.codes, "./Analysis/Cleaned_data/crop_codes.csv", row.names = FALSE)
