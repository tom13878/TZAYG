# winsor vectors only
winsor_vec <- function(V, x) {
        
        ord <- order(V)
        C <- ord[x*length(ord)]
        
        for (i in 1:length(V)) {
                if (V[i] < V[C]){
                        next
                } else {
                        V[i] <- V[C]
                }   
        }
        V
}   

#tests
# x <- sample(1:100, 100, replace = TRUE)
# winsor.test <- winsor_vec(x, 0.95)
