# tables for data section

# voucher sections. Look at fertilizer vouchers per year per region 
plot_vars_10 <- read.csv("M:/TZAYG/data/2010/plot_variables_w2.csv")

voucher <- select(plot_vars_10, y2_hhid, voucher1, voucher2) %>% arrange(voucher1)

voucher <- transmute(voucher, y2_hhid, v1=ifelse(voucher1 %in% "YES", 1, 0),
                     v2=ifelse(voucher2 %in% "NO", 1, 0))

voucher <- melt(voucher, id.vars="y2_hhid")

voucher <- group_by(voucher, y2_hhid) %>% summarise(v=sum(value)) %>% arrange(v)

# add some region  information and make a table by region
region_names <- c("dodoma", "arusha", "kilimanjaro", "tanga", "morogoro", "pwani",
                  "dar es salaam", "lindi", "mtwara", "ruvuma", "iringa", "mbeya",
                  "singida", "tabora", "rukwa", "kigoma", "shinyanga", "kagera",
                  "mwanza", "mara", "manyara", "kaskazini unguja", "kusini unguja",
                  "mjini magharibi", "kaskazini pemba", "kusini pemba")
hhid_region <- read_dta("C:/Users/morle001/Dropbox/Micro_IPOP/Data/Tanzania/2010_11/Stata/TZNPS2HH1DTA/HH_SEC_A.dta")
hhid_region <- select(hhid_region, y2_hhid, region)
hhid_region$region <- factor(hhid_region$region, labels=region_names)

# test whether comm_link hh's in other hh's
voucher$y2_hhid <- as.character(voucher$y2_hhid)
voucher$y2_hhid <- ifelse(str_length(voucher$y2_hhid) < 16, paste("0", voucher$y2_hhid, sep=""), voucher$y2_hhid)
table(voucher$y2_hhid %in% hhid_region$y2_hhid)

# join fert info with voucher info
voucher <- left_join(voucher, hhid_region)
voucher$recip <- ifelse(!(voucher$v %in% 0), 1, 0)
by_region1 <- group_by(voucher, region) %>% summarise(p1=sum(recip)/n())

# g <- ggplot(data=by_region, aes(x=region, y=x), colour="black") + geom_bar(stat="identity", fill="pink")
# g <- g + theme(panel.background=element_rect(fill="white"))
# g <- g + theme(axis.text.x=element_text(angle=90, hjust=1)) 
# 

# # # # # # # # # # # now do the same for 2012 # # # # # # # # # # # #
plot_vars_12 <- read.csv("M:/TZAYG/data/2012/plot_variables_w3.csv")

voucher2 <- select(plot_vars_12, y3_hhid, voucher1, voucher2) %>% arrange(voucher1)

voucher2 <- transmute(voucher2, y3_hhid, v1=ifelse(voucher1 %in% "YES", 1, 0),
                     v2=ifelse(voucher2 %in% "NO", 1, 0))

voucher2 <- melt(voucher2, id.vars="y3_hhid")

voucher2 <- group_by(voucher2, y3_hhid) %>% summarise(v2=sum(value)) %>% arrange(v2)

# add some region  information and make a table by region
filepath2 <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data"
setwd(filepath2)
comm_link2 <- read_dta("HH_SEC_A.dta")
comm_link2 <- select(comm_link2, y3_hhid, region=hh_a01_2)
comm_link2$region <- factor(comm_link2$region) %>% tolower()

# test whether comm_link hh's in other hh's
table(voucher2$y3_hhid %in% comm_link2$y3_hhid)

# join fert info with voucher info
voucher2 <- left_join(voucher2, comm_link2)
voucher2$recip <- ifelse(!(voucher2$v2 %in% 0), 1, 0)
by_region2 <- group_by(voucher2, region) %>% summarise(p2=sum(recip)/n())
# g <- ggplot(data=by_region, aes(x=region, y=x), colour="black") + geom_bar(stat="identity", fill="pink")
# g <- g + theme(panel.background=element_rect(fill="white"))
# g <- g + theme(axis.text.x=element_text(angle=90, hjust=1)) + labs()
# summarise vouchers by region

# vouchers by region
vouch_tot <- left_join(by_region2, by_region1)
m <- melt(vouch_tot, id.vars="region")

g <- ggplot(data=m, aes(x=region, y=variable, fill=variable) ) + geom_bar(stat="identity")


vtbl <- xtable(as.data.frame(vouch_tot))
print(vtbl, type="html")
