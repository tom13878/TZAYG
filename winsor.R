# created by: Tom Morley
# Date: 22-12-2014


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


# examples
# df <- data.frame(x = sample(1:10000, 100), y = sample(1:10000, 100))
# w1 <- winsor(df = df, "x", 0.95)
# w2 <- winsor(df = df, x = 0.95, var = "y")

