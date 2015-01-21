#' This section is also part of TZA10_prep.R
#' fertilizer variables are taken from agricultural questionnaire section 3A 
#' and used to produce a database of plots by fertilizer type and quantity used
#' of nitrogen per plot for use in the production function

s <- select(AQSEC3A, y2_hhid, plotnum, type1 = ag3a_46, type2 = ag3a_53)
m <- melt(s, id = c('y2_hhid', 'plotnum'))
m <- arrange(m, variable)

ty1 <- filter(m, variable == 'type1')
ty2 <- filter(m, variable == 'type2')

ty1j <- left_join(ty1, select(AQSEC3A, y2_hhid, plotnum, value = ag3a_46, fert.kg = ag3a_47))
ty1j <- select(ty1j, y2_hhid, plotnum, type = value, fert.kg)

ty2j <- left_join(ty2, select(AQSEC3A, y2_hhid, plotnum, value = ag3a_53, fert.kg = ag3a_54))
ty2j <- select(ty2j, y2_hhid, plotnum, type = value, fert.kg)

tot <- rbind(ty1j, ty2j)

# attach labels to fertilizer types
tot <- transform(tot, type = factor(type, labels=c("DAP", "UREA", "TSP", "CAN", "SA",
                                                   "generic NPK (TZA)", "MRP")))
# read in fertilizer composition table
fert.comp <- read.xls("Data/Other/Fert_comp.xlsx", sheet = 2) %>% rename(type = Fert_type2)
fert.comp$P_share <- as.numeric(fert.comp$P_share)
fert.comp$K_share <- as.numeric(fert.comp$K_share)
fert.comp$N_share <- as.numeric(fert.comp$N_share)

# create fertilizer variables
fert.vars <- merge(tot, select(fert.comp, type, N_share, P_share, K_share), all.x = TRUE) %>%
        mutate(N = fert.kg * (N_share/100), P = fert.kg * (P_share/100), K = fert.kg * (K_share/100)) %>%
        select(y2_hhid, plotnum, everything()) %>%
        ddply(.(y2_hhid, plotnum), summarize,
              nitrogen.kg = sum(N, na.rm = TRUE),
              phosphorous.kg = sum(P, na.rm = TRUE),
              potassium.kg = sum(K, na.rm = TRUE)) %>%
        arrange(desc(nitrogen.kg))