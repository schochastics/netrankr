#' @title Indirect relations of a network
#' @description Derive indirect relations like distances, for a given network. This function
#' can be used to calculate a variety of centrality indices.
#' @param g igraph object. The network for which relations should be derived
#' @param relation string. giving the relation to be calculated. One of "identity", 
#' "distances", "dependencies" or "walks". See Details.
#' @param FUN a weighting function. See Details.
#' @param ... additional arguments passed to the function FUN.
#' @details The \emph{relation} parameter has the following options.  
#' 
#' \emph{"identity"} returns the adjacency matrix of the network.  
#' 
#' \emph{"distances"} returns geodesic distances between all pairs of nodes.  
#' 
#' \emph{"dependencies"} returns dyadic dependencies 
#' \deqn{\delta(u,s) = \sum_{t \in V} \frac{\sigma(s,t|u)}{\sigma(s,t)}}
#' where \eqn{\sigma(s,t|u)} is the number of shortest paths from s to t that include u and
#' \eqn{\sigma(s,t)} is the total number of shortest (s,t)-paths. The row sums of the resulting matrix 
#' is equal to the betweenness scores.  
#' 
#' \emph{"walks"} returns walk counts between pairs of nodes, usually they are 
#' weighted decreasingly in their lengths or other properties which can be done by adding
#' a function in \code{FUN}.  
#' 
#' \emph{"resistance"} returns the resistance distance between all pairs of vertices.
#'  
#' The function \code{FUN} is used to further specify the indirect
#' relation. see `?transform_relations` for predefined functions.
#' 
#' @return a matrix containing indirect relations in a network.
#' @author David Schoch
#' @examples
#' 
#' require(igraph)
#' 
#' @export
indirect_relations <- function(g,relation="geodesic",
                               FUN=identity,...){
  
  if(relation=="geodesic"){
    rel <- igraph::distances(g,mode = "all")
    rel <- FUN(rel,...)
  } else if(relation=="identity"){
      rel <- igraph::get.adjacency(g,type="both",sparse=FALSE)
      rel <- FUN(rel,...)
      diag(rel) <- 0
  } else if(relation=="dependencies"){
      adj <- lapply(igraph::get.adjlist(g),function(x)x-1)
      rel <- dependency(adj)
  } else if(relation=="walks"){
      eigen.decomp <- eigen(igraph::get.adjacency(g,type="both"))
      lambda <- eigen.decomp$values
      X <- eigen.decomp$vectors
      rel <- X%*%diag(FUN(lambda,...))%*%t(X)
  } else if(relation=="resistance"){
    L <- igraph::graph.laplacian(g,sparse=FALSE)
    n <- igraph::vcount(g)
    A <- L+matrix(1/n,n,n)
    C <- solve(A)
    rel <- resistanceDistance(C,n)
    rel <- FUN(rel,...)
  }else stop(paste(relation,"is not defined as indirect relation"))
  return(rel)
}