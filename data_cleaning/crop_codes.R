# -------------------------------------
# The names given to crops in the output
# section of the agricultural questionnaire
# and the market price section of the 
# community questionnaire are different.
# this makes it difficult to match up 
# prices and outputs
#
# This file contains code for making
# a table with corresponding price and
# output names for the different goods.
# -------------------------------------

# ------------ YEAR 1:2008 ------------


# ------------ YEAR 2:2010 ------------




# ------------ YEAR 3:2012 ------------
output <- read.csv("M:/cleaned_data/2012/plot_output_y3.csv")
# get the zaocodes from the output section, all in lower case
zaocode <- unique(tolower(levels(output$zaocode)))

# new variable which will hold a vector of item names for prices
item_name <- zaocode

# price names
item_name[zaocode %in% c("amaranths", "bilimbi", "chilies", "coffee", "cotton",
                         "fiwi", "pigeon pea", "pyrethrum", "seaweed", "sesame",
                         "soyabeans", "sunflower", "water mellon")] <- "mangoes"
item_name[zaocode %in% c("bambara nuts", "groundnut")] <- "groundnuts"
item_name[zaocode %in% c("cucumber", "egg plant", "okra", "pumpkins", "tomatoes")] <- "tomatoes"
item_name[zaocode %in% c("bullrush millet", "finger millet")] <- "millet (grain)"
item_name[zaocode %in% "cassava"] <- "casava fresh"
item_name[zaocode %in% c("chick peas", "cowpeas", "field peas")] <- "peas"
item_name[zaocode %in% c("beans", "green gram")] <- "beans"
item_name[zaocode %in% c("maize", "wheat")] <- "maize (grain)"
item_name[zaocode %in% "paddy"] <- "rice (paddy)"
item_name[zaocode %in% "sorghum"] <- "sorghum (grain)"

cash_crop <- ifelse( zaocode %in% c( "coffee", "cotton", "tobacco"), TRUE, FALSE )

crop_code_y3 <- data.frame( zaocode, item_name, cash_crop )


# write.csv(crop_code_y3,"M:/cleaned_data/crop_code_y3.csv", row.names=FALSE)





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
