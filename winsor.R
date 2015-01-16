# created by: Tom Morley
# Date: 22-12-2014
# TODO {tom morley}: add control structure for dealing with NA values
#                   also modify so that it is possible to take just a vector of
#                   values too - try this by creating a class?

winsor <- function(df, var, x) {
        
        ord <- order(df[, var])
        C <- ord[x*length(ord)]
        
        for (i in 1:length(df[, var])) {
                
                if (is.na(df[, var][i])){
                        next
                } else {
                        if (df[, var][i] < df[, var][C]){
                                next
                        } else {
                                df[, var][i] <- df[, var][C]
                        } 
                }
        }
        df
}        


# example
# df <- data.frame(x = sample(1:10000, 100), y = sample(1:10000, 100))
# w1 <- winsor(df = df, "x", 0.95)
# w2 <- winsor(df = df, x = 0.95, var = "y")

