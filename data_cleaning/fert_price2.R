source("M:/TZAYG/winsor.R")
fert.vars <- read.csv("./Analysis/Cleaned_Data/fert_vars_y2.csv")
fert.vars.winsor <- ddply(fert.vars, .(region, fert.type),
                          function(elt) winsor(elt, 0.975, "nit.price.kg"))
write.csv(fert.vars.winsor, "./Analysis/Cleaned_data/fert_vars_winsor_y2.csv", row.names = FALSE)