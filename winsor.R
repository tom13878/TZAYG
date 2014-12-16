# created by: Tom Morley
# Date: 25-11-2014
# A function to winsorize data at a level level defined by the argument x. 
# The first argument is a dataframe, the second argument is a number between 0 and 1
# corresponding to the level at which you want to winsor and the third argument is a
# string referring to the column over which you want the winsoring to take place

winsor <- function(df, x, var) {
        
        q = quantile(df[, var], probs = c(x), na.rm = TRUE)
        
        for (i in 1:length(df[, var])){
                
                if(is.na(df[, var][i])) {
                        next
                } else if (df[, var][i] > q) {
                        df[, var][i] = q
                } else { 
                        next
                }
        } 
        df
}

# example
# df <- data.frame(x = sample(1:100, 10), y = sample(1:100, 10), z = sample(1:100,10))
# winsor(df = df, x = 0.975, var = "z")
# winsor(df = df, x = 0.95, var = "y")
# winsor(df = df, x = 0.9, var = "x")
