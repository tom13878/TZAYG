#' Two functions for winsoring data. The first simply takes the values and
#' removes the upper levels with the 0.95 quantile value. This could be extended 
#' to deal with small values too. The second function is more complicated
#' check out http://www.r-bloggers.com/winsorization/


winsor1 <- function(df, var, fraction = 0.05) {
        if (length(fraction) != 1 || fraction < 0 || fraction > 0.1){
                stop('bad choice of fraction!!')
        } 
        
        lim <- quantile(df[, var], probs = 1-fraction, na.rm = TRUE)
        df[, var][df[, var] > lim] <- lim 
        df
        
        }
        

winsor2 <- function(df, var, multiple = 3){
        if(length(multiple) != 1 || multiple <= 0){
                stop('bad choice of multiple')
        }
        med <- median(df[, var], na.rm = TRUE)
        df[, var] <- df[, var] - med
        # note scale factor is 1.4826 for normal data assumption
        sc <- mad(df[, var], center = 0, na.rm = TRUE)* multiple
        print(sc)
        df[, var][df[, var] > sc] <- sc
        df[, var] <- df[, var] + med
        df
}

winsor3 <- function(df, cols, multiple = 3){
        if(length(multiple) != 1 || multiple <= 0){
                stop('bad choice of multiple')
        }
        
        for(i in cols){
                print(i)
                med <- median(df[, i], na.rm = TRUE)
                df[, i] <- df[, i] - med
                # note scale factor is 1.4826 for normal data assumption
                sc <- mad(df[, i], center = 0, na.rm = TRUE)* multiple
                print(sc)
                df[, i][df[, i] > sc] <- sc
                df[, i] <- df[, i] + med
        }
        df
}

trim1 <- function(df, col){
        q <- quantile(df[, col], probs = c(0.05, 0.95), na.rm = TRUE)
        ll <- q[1]
        ul <- q[2]
        df[,col][df[,col] < ll | df[,col] > ul] <- NA
        df
}

trim2 <- function(df, cols){
        for(i in cols){
                q <- quantile(df[, i], probs = c(0.05, 0.95), na.rm = TRUE)
                ll <- q[1]; print(ll)
                ul <- q[2]; print(ul)
                df[,i][df[,i] < ll | df[,i] > ul] <- NA
        }
        df
}
#' example   
# df <- data.frame(x = rnorm(10), y = sample(1:100, 10, replace = TRUE))
#' winsor3(df, c('x', 'y'), 3)
#' winsor1(df, 'y', 0.05)