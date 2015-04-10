#'output for year 2 using the Liu Meyers index
#'
setwd('M:/cleaned_data')

library(foreign)
library(plyr)
library(reshape2)
library(dplyr)

# Compute output index following Sheahan
crop.codes <- read.csv('C:/Users/morle001/Dropbox/Micro_IPOP/Analysis/Cleaned_data/crop_codes.csv')

prices <- read.csv('./2010/prices_winsor_y2.csv')

HQSECA <- read.dta('C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_A.dta', convert.factors = TRUE) 
y2_hhid.region <- unique(select(HQSECA, y2_hhid, region)) %>% 
        mutate(region = factor(region, labels = levels(factor(prices$region))))
y2_hhid.region$y2_hhid <- as.numeric(y2_hhid.region$y2_hhid)

# have to read in as a stata file otherwise 'ghost' households appear!
output <- read.csv('./2010/plot_input_output.csv') %>% 
        select(y2_hhid, plotnum, zaocode, output.kg, total.plot, inter.crop, seed.type)

output.maize <- merge(output, y2_hhid.region, by = 'y2_hhid', all.x = TRUE)

# find plots which contain at least some maize on them and combine with region, zone and
# crop codes data and also with price data
output.maize <- ddply(output.maize, .(y2_hhid, plotnum), transform, maize = any(zaocode == "Maize"))
output.maize <- output.maize[output.maize$maize, ]
bad <- is.na(output.maize$maize)
output.maize <- output.maize[!bad, ]
output.maize <- merge(output.maize, select(crop.codes, crop.name, itemname, cash.crop),
                      by.x = "zaocode", by.y = "crop.name", all.x = TRUE)
output.maize <- left_join(output.maize, select(prices, itemname, region, region.price = region.price.tot))

count <- melt(select(output.maize, y2_hhid, plotnum, zaocode), id = c("y2_hhid", "plotnum"))
count <- ddply(count, .(y2_hhid, plotnum), summarize,
               crop.count = length(unique(value[!is.na(value)])))
output.maize <- left_join(output.maize, count)
output.maize <- output.maize[!is.na(output.maize$zaocode), ]

output.maize$value <- output.maize$region.price * output.maize$output.kg

output.maize <- ddply(output.maize, .(y2_hhid, plotnum), 
                      summarize, plot.value = sum(value),
                      maize.price = region.price[zaocode == "Maize"],
                      maize.value = maize.price * output.kg[zaocode == "Maize"],
                      output.kg.old = output.kg[zaocode == "Maize"],
                      output.kg.new = plot.value/maize.price, 
                      maize.share = maize.value/plot.value * 100, crop.count = unique(crop.count),
                      beans = any(zaocode == "Beans"), cash.crop = any(cash.crop == "YES"))

output <- filter(output, zaocode == "Maize") %>% 
        select(y2_hhid, plotnum, total.plot, inter.crop, seed.type)
output.maize <- left_join(output.maize, output)

write.csv(output.maize, "./2010/output_maize_y2.csv", row.names = FALSE)
