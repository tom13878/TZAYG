
# fertilizer prices

fert.vars <- read.csv("./fertilizer_variables.csv")
fert.vars.winsor <- ddply(fert.vars, .(fert.type),
                          function(elt) winsor(elt, "nit.price.kg", 0.95))
write.csv(fert.vars.winsor, "M:/cleaned_data/2008/fertilizer_winsor.csv", row.names = FALSE)

pdf('./fert_split.pdf', width = 8, height = 4)
ddply(fert.vars.winsor, .(fert.type), failwith(NA, plotpattern))
dev.off()