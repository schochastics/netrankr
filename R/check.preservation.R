check_preservation=function(P,scores){
  #' @title Checks if a partial ranking is preserved 
  #' @description Checks if a partial ranking is preserved in the ranking induced by scores
  #' @param P a partial ranking as a matrix object
  #' @param scores scores for each element
  #' @details In order for a score vector to preserve a partial ranking, the following 
  #' condition must be fulfilled:
  #' \code{P[u,v]==1 & scores[i]<=scores[j]}
  #' @return a boolean 
  #' @examples
  #' require(igraph)
  #' g=graph.star(5,"undirected")
  #' neighborhood_inclusion(g)
  #' @export
  
  P.idx=which(P==1,arr.ind=T)
  preserved=all(!apply(P.idx,1,function(x){scores[x[1]]>scores[x[2]]}))
  
  return(preserved)
}
