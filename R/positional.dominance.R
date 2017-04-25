positional_dominance=function(A,map=FALSE,benefit=TRUE){
  #' @title Generalized Dominance in Graphs
  #' @description generalized dominance relations. More to come
  #'
  #' @param A matrix containing attributes or relations
  #' @param map boolean if rows can be sorted or not (default)
  #' @param benefit boolean if higher values(default) or lower values are better
  #' @details positional dominance is a generalization of neighborhood inclusion. 
  #' In the default case, it checks for all pairs \eqn{i,j} if \eqn{A_{it} \geq A_{jt}} holds for all \eqn{t}.
  #' If \code{map=TRUE}, the rows of \eqn{A} are sorted decreasingly (\code{benefit=TRUE}) or increasingly
  #' (\code{benefit=FALSE}) and then the dominance condition is checked.
  #' @return dominance relations as matrix object.
  #' @seealso [neighborhood_inclusion],[check_preservation]
  #' @examples
  #' ###
  #' require(igraph)
  #' 
  #' g <- graph.empty(n=11,directed = FALSE)
  #' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
  #'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  #' P<-neighborhood_inclusion(g)
  #' comparable_pairs(P)
  #' 
  #' dist <- distances(g)
  #' D <- positional_dominance(dist,map=TRUE,benefit=FALSE) #lower distances are better
  #' comparable_pairs(D) #more comparables than P
  #' ### all distance based indices preserve the partial ranking D
  #' check_preservation(D,distance_index(g,type="sor"))
  #' check_preservation(D,distance_index(g,type="ros"))
  #' check_preservation(D,distance_index(g,type="pow2"))
  #' check_preservation(D,distance_index(g,type="int"))
  #' @export
  
  D=matdom(A,map,benefit)
  return(D)
}
