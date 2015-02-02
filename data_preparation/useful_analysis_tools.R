# useful analysis tools

# 1. correlation matrix
library(DescTools)
c <- cor(db7[, whichNumerics(db7)], use = 'pairwise.complete.obs')
PlotCorr(c, col=PalDescTools("RedWhiteBlue1", 100), border="grey", frame="grey")

# revalue function in plyr!
# reorder function in plyr!!
        