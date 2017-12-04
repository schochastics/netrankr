#' @title Indirect relations in a network
#' @description Derive indirect relations for a given network. 
#' Observed relations, like presents or absence of a relation, are commonly not the center
#' of analysis, but are transformed in a new set of indirect relation like geodesic 
#' distances among nodes. These transformations are usually an implicit step when centrality
#' indices are used. Making this step explicit gives more possibilities, for example
#' calculating partial centrality rankings with [positional_dominance].
#' @param g igraph object. The network for which relations should be derived.
#' @param type String giving the relation to be calculated. See Details for options.
#' @param log_param Numeric parameter. Only used for logarithmic forest distance. 
#' @param FUN A function that allows the transformation of relations. See Details.
#' @param ... Additional arguments passed to FUN.
#' @details The `type` parameter has the following options.  
#' 
#' \emph{'identity'} returns the adjacency matrix of the network.  
#' 
#' \emph{'geodesic'} returns geodesic distances between all pairs of nodes.  
#' 
#' \emph{'depend_sp'} returns dyadic dependencies 
#' \deqn{\delta(u,s) = \sum_{t \in V} \frac{\sigma(s,t|u)}{\sigma(s,t)}}
#' where \eqn{\sigma(s,t|u)} is the number of shortest paths from s to t that include u and
#' \eqn{\sigma(s,t)} is the total number of shortest (s,t)-paths. This relation is used
#' for betweenness-like centrality indices.
#' 
#' \emph{'walks'} returns walk counts between pairs of nodes, usually they are 
#' weighted decreasingly in their lengths or other properties which can be done by adding
#' a function in \code{FUN}.  See [transform_relations] for options.
#' 
#' \emph{'resistance'} returns the resistance distance between all pairs of nodes.
#'  
#' \emph{'log_forest'} returns a logarithmic forest distance \eqn{d_\alpha(s,t)}. The logarithmic forest
#' distances form a one-parametric family converging to the geodesic distance as \eqn{\alpha \to 0^+}
#' and to the resistance distance as \eqn{\alpha \to \infty}. See 
#'  
#' Chebotarev, P., 2011. A class of graph-geodetic distances generalizing the shortest-path and
#' the resistance distances. *Discrete Applied Mathematics* 159,295-302.
#' 
#' for more details.
#'       
#' The function \code{FUN} is used to further specify the indirect
#' relation. See [transform_relations] for predefined functions and additional help.
#' 
#' @return A matrix containing indirect relations in a network.
#' @author David Schoch
#' @seealso [aggregate_positions] to build centrality indices, [positional_dominance] to derive dominance relations
#' @examples
#' library(igraph)
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' #geodesic distances
#' D <- indirect_relations(g,type = "geodesic") 
#' 
#' #dyadic dependencies
#' D <- indirect_relations(g,type = "depend_sp")
#' 
#' #walks attenuated exponentially by there length
#' W <- indirect_relations(g,type = "walks",FUN = walks_exp)
#' 
#' #positional dominance of a transformed relation...
#' D <- indirect_relations(g,type = "geodesic") 
#' 
#' #...under total heterogeneity
#' positional_dominance(D, map = FALSE ,benefit = FALSE)
#' #...under total homogeneity
#' positional_dominance(D, map = TRUE ,benefit = FALSE)
#' @export
indirect_relations <- function(g, type = "geodesic", 
                               log_param = NULL,FUN = identity, ...) {
    if(type=="dependencies"){
      warning('"dependencies" is deprecated. Use "depend_sp" instead ')
      type <- "depend_sp"
    }
    if (type == "geodesic") {
        rel <- igraph::distances(g, mode = "all")
        rel <- FUN(rel, ...)
    } else if (type == "identity") {
        rel <- igraph::get.adjacency(g, type = "both", sparse = FALSE)
        rel <- FUN(rel, ...)
        diag(rel) <- 0
    } else if (type == "depend_sp") {
        adj <- lapply(igraph::get.adjlist(g), function(x) x - 1)
        rel <- dependency(adj)
    } else if (type == "walks") {
        eigen.decomp <- eigen(igraph::get.adjacency(g, type = "both"))
        lambda <- eigen.decomp$values
        X <- eigen.decomp$vectors
        rel <- X %*% diag(FUN(lambda, ...)) %*% t(X)
    } else if (type == "resistance") {
        L <- igraph::graph.laplacian(g, sparse = FALSE)
        n <- igraph::vcount(g)
        A <- L + matrix(1/n, n, n)
        C <- solve(A)
        rel <- resistanceDistance(C, n)
        rel <- FUN(rel, ...)
    } else if (type == "log_forest"){
      if(is.null(log_param)){
        stop('argument "log_param" is missing for "log_forest", with no default')
      }
      n <- igraph::vcount(g)
      gamma <- log(exp(1) + log_param^(2/n))
      
      L <- igraph::graph.laplacian(g, sparse = FALSE)
      I <- diag(1, n)
      Q <- solve(I + log_param * L)
      
      if(log_param==1){
        H <- gamma * log(Q)
      } else{
        H <- gamma * (log_param - 1) * logb(Q, log_param)  
      }
      rel <- 0.5 * (diag(H)%*%t(rep(1,n)) + rep(1,n)%*%t(diag(H))) - H
      rel <- FUN(rel, ...)
    } 
  else stop(paste(type, "is not defined as indirect relation"))
    return(rel)
}

#-------------------------------------------------------------------------------

