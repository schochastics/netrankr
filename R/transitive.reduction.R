#' @title Transitive Reduction of Dominance Relations
#' @description  Calculates the transitive reduction of a directed graph.
#'
#' @param P matrix containing relations
#' @return transitive reduction of P
#' @examples
#' require(igraph)
#' g <- sample_gnp(100,0.1)
#' P<-neighborhood_inclusion(g)
#' sum(P)
#' T<-transitive_reduction(P)
#' sum(T)
#' @export
transitive_reduction <- function(P){
  B <- P
  n <- nrow(B)
  for (j in 1:n){
    for (i in 1:n){
      if (B[i,j]==1){
        for (k in 1:n)
          if (B[j,k]==1){
            B[i,k] <- 0
          }
      }
    }
  }
  return(B)
}
