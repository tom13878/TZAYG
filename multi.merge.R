## date created: 8-10-2014
## function description:
## takes a list of dataframes to be merged x, and a character vector a of length 1
## performs multi merging of dataframes in x


multi.merge <- function(x, a, ...) {
  
  ## initialise data frame with first element of list 
  df <- x[[1]] 
  ## loop over elements of list
  for (i in 2:length(x)){
    df <- merge(df, x[[i]], by = a)
  }
  ## return single data frame
  df
  
}

