# fert graph

voucher <- select(tp, hhid, region, zone, year, voucher)

by_region_year <- group_by(voucher, region, year) %>% summarise(v=sum(voucher))
by_region <- ddply(by_region_year, .(region), transform, frac=v/sum(v))
by_region$frac <- ifelse(is.na(by_region$frac), 0, by_region$frac)
by_region <- select(by_region, -v)

library(ggplot2)
library(RColorBrewer)

# make year a factor and relevel so that 2010 comes first
by_region$year <- factor(by_region$year, levels=c("2010", "2012"))
by_region$frac <- by_region$frac*100

g <- ggplot(data=by_region, aes(x=region, y=frac, fill=year, order=year)) +
        geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90, hjust=1)) +
        guides(fill=guide_legend(reverse=TRUE)) + scale_fill_brewer(palette="Pastel1")


by_zone_year <- group_by(voucher, zone, year) %>% summarise(v=sum(voucher))
by_zone <- ddply(by_zone_year, .(zone), transform, frac=v/sum(v))
by_zone$frac <- ifelse(is.na(by_zone$frac), 0, by_zone$frac)
by_zone <- select(by_zone, -v)

by_zone$year <- factor(by_zone$year, levels=c("2010", "2012"))
by_zone$frac <- by_zone$frac*100

g2 <- ggplot(data=by_zone, aes(x=zone, y=frac, fill=year, order=year)) +
        geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90, hjust=1)) +
        guides(fill=guide_legend(reverse=TRUE)) + scale_fill_brewer(palette="Dark2")