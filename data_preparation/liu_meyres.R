#' calcualtion of the Liu-meyres index to produce the output data in maize terms
#' so that we can create a production function
#' get data files from the output of the data processing stage
setwd('M:/cleaned_data')

library(foreign)
library(plyr)
library(reshape2)
library(dplyr)

crop.codes <- read.csv('C:/Users/morle001/Dropbox/Micro_IPOP/Analysis/Cleaned_data/crop_codes.csv')

HQSECA <- read.dta('C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2008_09/Stata/TZNPS1HHDTA_E/SEC_A_T.dta', convert.factors = TRUE) 
hhid.region <- unique(select(HQSECA, hhid, region))
hhid.region$hhid <- as.numeric(hhid.region$hhid)
                      
prices <- read.csv('./2008/prices_winsor_y1.csv')

output <- read.csv('./2008/plot_input_output.csv') 
output <- output[!is.na(output$zaocode), ]

# find plots which contain at least some maize on them and combine with region, zone and
# crop codes data and also with price data
output.maize <- ddply(output, .(hhid, plotnum), transform, maize = any(zaocode == "Maize"))
output.maize <- output.maize[output.maize$maize, ]
output.maize <- left_join(output.maize, hhid.region)
output.maize <- merge(output.maize, select(crop.codes, crop.name, itemname, cash.crop),
                      by.x = "zaocode", by.y = "crop.name", all.x = TRUE)
output.maize <- left_join(output.maize, select(prices, itemname, region, region.price)) 

# count the number of crops on each plot and combine with the output and price data
# calculate the value in shillings of each plot
count <- melt(select(output.maize, hhid, plotnum, zaocode), id = c("hhid", "plotnum"))
count <- ddply(count, .(hhid, plotnum), summarize,
               crop.count = length(unique(value[!is.na(value)])))
output.maize <- left_join(output.maize, count) 
output.maize$value <- output.maize$region.price * output.maize$output.kg

# calculate the Liu-Meyres index
output.maize <- ddply(output.maize, .(hhid, plotnum), 
                      summarize, plot.value = sum(value),
                      maize.price = region.price[zaocode == "Maize"],
                      maize.value = maize.price * output.kg[zaocode == "Maize"],
                      output.kg.old = output.kg[zaocode == "Maize"],
                      output.kg.new = plot.value/maize.price, 
                      maize.share = maize.value/plot.value * 100, crop.count = unique(crop.count),
                      beans = any(zaocode == "Beans"), cash.crop = any(cash.crop == "YES"))
output <- filter(output, zaocode == "Maize") %>% 
        select(hhid, plotnum, total.plot, inter.crop, seed.type)
output.maize <- left_join(output.maize, output)

write.csv(output.maize, "./2008/output_maize_y1.csv", row.names = FALSE)
