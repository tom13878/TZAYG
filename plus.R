# Plus
plus <- function(x) {
        if(all(is.na(x))){
                c(x[0],NA)} else {
                        sum(x,na.rm = TRUE)}
}
