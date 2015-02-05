library(foreign)

path <- 'W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data'
setwd(path)

AGSEC2A <- read.spss('./AG_SEC_2A.SAV', to.data.frame = TRUE)
?read.dta
