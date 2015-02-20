# function for making latex tables

LaTeX_out <- function(df) {
        
        # initialize list object and a NULL object
        L <- list()
        c <- NULL
        
        # set NA values to zero
        for (i in 1:ncol(df)){
                df[,i][is.na(df[,i])] <- 0
        }
        
        # create list from rows of data frame 
        
        for (i in 1:nrow(df)){
               L <- append(L, list(df[i, ]))
        }

        # make all elements of the list characters
        
        for (i in 1:length(L)) {
            L[[i]] <- as.character(L[[i]])    
        }
        
        # add function for pasting together rows of dataframe
        # ready for LaTeX table
        add <- function(x){
                c <- NULL
                for (i in 1:length(x)){
                        if(is.null(c)){
                                c <- paste(x[1])
                        } else {
                                c <- paste(c, x[i], sep = ' & ')
                        }
                }
                paste(c, ' \\', sep = '')
        }
        
        # Apply add function across components of list
        L <- lapply(L, add)
        
}
        
        
        

        
        
        
        
        
        
        
        
        
        
        
