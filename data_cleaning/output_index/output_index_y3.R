# -------------------------------------
#' Liu Meyeres calculation for third 
#' wave of tanzania panel (2012). 
# -------------------------------------
filepath <- 'W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/Tanzania/2012/Data'
setwd( filepath )
# read in prices
prices <- read.csv("M:/cleaned_data/2012/prices.csv")
# read in region to match up with price regions
regions <- read.spss( "HH_SEC_A.SAV", to.data.frame=TRUE ) %>%
        select( y3_hhid, region=hh_a01_1 )
# read in output
output <- read.csv("M:/cleaned_data/2012/plot_output_y3.csv")


# New way of calculting the index
# split up the calculation 

# use group_by to find out if there is maize on a plot or not
by_plot <- group_by(output, y3_hhid, plotnum) %>%
        filter(maize=any(zaocode=="MAIZE")) %>%
        summarise(crop_count=length(zaocode)) # 3079 plots with an average of
                                              # 1.8 crops per plot
                          
# construct a table of item_name to zaocode because of stupid prices
x <- as.character(unique(by_plot$zaocode))
y <- c("Maize (grain)", "Mangoes", "Sorghum (grain)", "Peas", "Millet (grain)",
       "Groundnuts", "Mangoes", "Tomatoes", "Groundnuts", "Mangoes", "Mangoes",
       "Beans", "Beans", "Mangoes", "Spinach", "Peas", "Tomatoes", "Sweet potatoes",
       "Millet (grain)", "Cocoyams", "Yams", "Mangoes", "Irish potatoes",
       "Rice (paddy)", "Casava fresh", "Tomatoes", " ", "Tomatoes", "Mangoes",
       "Onions", "Mangoes", "Maize (grain)", "Peas", "Mangoes", "Mangoes", 
       "Cabbage", "Mangoes", "Tomatoes")
price_converter <- data.frame(x, y)











