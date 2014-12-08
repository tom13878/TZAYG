# Year 1: merge all databases into a single file
# I. combine data
#    1. subset the plot input Output data for maize plotsPlot level input-output data with plot level indicators and output data for Maize plots
Plot.tot <- PlotIO[PlotIO$zaocode == "Maize", ]
Plot.tot <- merge(Plot.tot, PlotInd, by = c("hhid", "plotnum"))
Plot.tot <- merge(Plot.tot, Output.Maize, by = c("region", "hhid", "plotnum"))
## Combine all data at various levels - Finish Line
Total.TZA.Y1 <- merge(Plot.tot,HH1, by = c("hhid"), all.x = TRUE)
Total.TZA.Y1 <- merge(Total.TZA.Y1, ComInd[, -c(3:6, 54)], by = c("hhid", "region"), all.x = TRUE)
summary(Total.TZA.Y1)                  

save(Total.TZA.Y1, file = "./Analysis/TZA 2008/Total.TZA.Y1.RData")          