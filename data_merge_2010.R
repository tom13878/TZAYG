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