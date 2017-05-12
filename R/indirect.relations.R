#' @title Indirect relations of a network
#' @description Derive indirect relations like distances for a given network. 
#' @param g igraph object. The network for which relations should be derived.
#' @param relation string. giving the relation to be calculated. See Details for options.
#' @param FUN a function that allows the transformation of relations. See Details.
#' @param ... additional arguments passed to FUN.
#' @details The \emph{relation} parameter has the following options.  
#' 
#' \emph{"identity"} returns the adjacency matrix of the network.  
#' 
#' \emph{"distances"} returns geodesic distances between all pairs of nodes.  
#' 
#' \emph{"dependencies"} returns dyadic dependencies 
#' \deqn{\delta(u,s) = \sum_{t \in V} \frac{\sigma(s,t|u)}{\sigma(s,t)}}
#' where \eqn{\sigma(s,t|u)} is the number of shortest paths from s to t that include u and
#' \eqn{\sigma(s,t)} is the total number of shortest (s,t)-paths. This relation is mostly used
#' for betweenness-like centrality indices.
#' 
#' \emph{"walks"} returns walk counts between pairs of nodes, usually they are 
#' weighted decreasingly in their lengths or other properties which can be done by adding
#' a function in \code{FUN}.  
#' 
#' \emph{"resistance"} returns the resistance distance between all pairs of vertices.
#'  
#' The function \code{FUN} is used to further specify the indirect
#' relation. See [transform_relations] for predefined functions and additional help.
#' 
#' @return a matrix containing indirect relations in a network.
#' @author David Schoch
#' @seealso [aggregate_index] to build centrality indices, [positional_dominance] to derive dominance relations
#' @examples
#' require(igraph)
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' #geodesic distances
#' D <- indirect_relations(g,relation="geodesic") 
#' #dyadic dependencies
#' D <- indirect_relations(g,relation="dependencies")
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