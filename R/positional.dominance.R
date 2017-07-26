#' @title Generalized Dominance in Graphs
#' @description generalized dominance relations. 
#'
#' @param A matrix containing attributes or relations
#' @param type string. Either "one-mode" (default) if \code{A} is a regular one-mode network
#' or "two-mode" if \code{A} is a general data matrix. 
#' @param map boolean if rows can be sorted or not (default)
#' @param benefit boolean if higher values(default) or lower values are better
#' @details Positional dominance is a generalization of neighborhood-inclusion for 
#' arbitrary network data. In the default case, it checks for all pairs \eqn{u,v} if 
#' \eqn{A_{ut} \ge A_{vt}} holds for all \eqn{t} if \code{benefit=TRUE} or 
#' \eqn{A_{ut} \le A_{vt}} holds for all \eqn{t} if \code{benefit=FALSE}.  
#' This form of dominance is referred to as *dominance under total heterogeneity*. 
#' If \code{map=TRUE}, the rows of \eqn{A} are sorted decreasingly (\code{benefit=TRUE}) 
#' or increasingly (\code{benefit=FALSE}) and then the dominance condition is checked. This second
#' form of dominance is referred to as *dominance under total homogeneity*.
#' 
#' @return dominance relations as matrix object. \code{P[u,v]=1} if u is dominated by v.
#' @author David Schoch
#' 
#' @references Brandes, U., 2016. Network positions. *Methodological Innovations* 9,
#' 2059799116630650.
#' 
#' Schoch, D. and Brandes, U., 2016. Re-conceptualizing centrality in social networks. 
#' *European Journal of Applied Mathematics* 27(6), 971-985.
#' 
#' @seealso [neighborhood_inclusion], [indirect_relations], [exact_rank_prob]
#' @examples
#' require(igraph)
#' 
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' P<-neighborhood_inclusion(g)
#' comparable_pairs(P)
#' 
#' dist <- indirect_relations(g,type="geodesic")
#' D <- positional_dominance(dist,map=FALSE,benefit=FALSE) 
#' comparable_pairs(D) #same as P
#' 
#' D <- positional_dominance(dist,map=TRUE,benefit=FALSE) 
#' comparable_pairs(D) #more comparables than P
#' 
#' @export

positional_dominance <- function(A,type="one-mode",map=FALSE,benefit=TRUE){
  if(grepl("one",type)){
    D <- matdom(A,map,benefit)
  }
  else if(grepl("two",type)){ #should be implemented in C++
    fct<-function(x,y) all(x<=y)+0
    vecfct<-Vectorize(fct)
    r.rows <- split(A, row(A))
    D <- outer(r.rows,r.rows,vecfct)
    diag(D) <- 0
  }
  return(D)
}
