#' @title Indirect relations in a network
#' @description Derive indirect relations for a given network. 
#' Observed relations, like presents or absence of a relation, are commonly not the center
#' of analysis, but are transformed in a new set of indirect relation like shortest path 
#' distances among nodes. These transformations are usually an implicit step when centrality
#' indices are used. Making this step explicit gives more possibilities, for example
#' calculating partial centrality rankings with [positional_dominance].
#' @param g igraph object. The network for which relations should be derived.
#' @param type String giving the relation to be calculated. See Details for options.
#' @param log_param Numeric parameter. Only used if type = "log_forest". 
#' @param netflow_mode String, one of raw, frac, or norm. Only used if type = "depend_netflow".
#' @param FUN A function that allows the transformation of relations. See Details.
#' @param ... Additional arguments passed to FUN.
#' @details The `type` parameter has the following options.  
#' 
#' \emph{'identity'} returns the adjacency matrix of the network.  
#' 
#' \emph{'dist_sp'} returns shortest path distances between all pairs of nodes.  
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
#' \emph{'dist_resist'} returns the resistance distance between all pairs of nodes.
#'  
#' \emph{'dist_lf'} returns a logarithmic forest distance \eqn{d_\alpha(s,t)}. The logarithmic forest
#' distances form a one-parametric family converging to shortest path distances as \eqn{\alpha \to 0^+}
#' and to the resistance distance as \eqn{\alpha \to \infty}. See 
#'  
#' Chebotarev, P., 2011. A class of graph-geodetic distances generalizing the shortest-path and
#' the resistance distances. *Discrete Applied Mathematics* 159,295-302.
#' 
#' for more details. The parameter `log_param` can be used to tune \eqn{\alpha}. 
#'
#' \emph{'depend_netflow'}
#' 
#' \emph{'depend_exp'}
#'
#' The function \code{FUN} is used to transform the indirect
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
#'
#' #shortest path distances
#' D <- indirect_relations(g,type = "dist_sp") 
#' 
#' #dyadic dependencies (used for betweenness)
#' D <- indirect_relations(g,type = "depend_sp")
#' 
#' #walks attenuated exponentially by there length
#' W <- indirect_relations(g,type = "walks",FUN = walks_exp)
#' 
#' @export
indirect_relations <- function(g, type = "dist_sp", 
                               log_param = NULL,netflow_mode = "",
                               FUN = identity, ...) {
    if(type=="dependencies"){
      warning('"dependencies" is deprecated. Use "depend_sp" instead.\n')
      type <- "depend_sp"
    }
    if (type == "dist_sp") {
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
    } else if (type == "dist_resist") {
        L <- igraph::graph.laplacian(g, sparse = FALSE)
        n <- igraph::vcount(g)
        A <- L + matrix(1/n, n, n)
        C <- solve(A)
        rel <- resistanceDistance(C, n)
        rel <- FUN(rel, ...)
    } else if (type == "dist_lf"){
      if(is.null(log_param)){
        stop('argument "log_param" is missing for "dist_lf", with no default')
      }
      rel <- log_forest_fct(g,log_param)
      rel <- FUN(rel, ...)
    } else if (type == "depend_netflow"){
      if(netflow_mode=="" | !netflow_mode%in%c("raw","frac","norm")){
        stop('netflow_mode must be one of"raw","frac","norm"')
      }
      if(netflow_mode=="norm"){
        stop('"norm" not supported yet. Use "frac" instead.')
      }
      rel <- depend_netflow_fct(g,netflow_mode)
      rel <- FUN(rel, ...)
    } else if(type=="depend_exp"){
      rel <- depend_exp_fct(g)
      rel <- FUN(rel,...)
    }
  else stop(paste(type, "is not defined as indirect relation"))
    return(rel)
}

#-------------------------------------------------------------------------------

log_forest_fct <- function(g,log_param){
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
  return(rel)
}

depend_netflow_fct <- function(g,netflow_mode){
  n <- igraph::vcount(g)
  mflow <- matrix(0,n,n)
  #maxflow
  for(s in 1:n){
    for(t in 1:n){
      if(s!=t){
        mflow[s,t] <- igraph::graph.maxflow(g,s,t)$value
      }
    }
  }
  if (netflow_mode == "norm") {
    flo <- mflow
    diag(flo) <- 0
    maxoflo <- rep(0, n)
    for (i in 1:n) maxoflo[i] <- sum(mflow[-i, -i])
  }
  flow_smat <- matrix(0,n,n)
  for(i in 1:n){
    g_i <- igraph::delete.vertices(g,i)
    for(s in 1:n){
      for(t in 1:n){
        if(i!=s & s!=t & i!=t){
          flow <- igraph::graph.maxflow(g_i,s-(s>i),t-(t>i))$value
          flow_smat[i,s] <- switch(netflow_mode, 
                                   raw =  flow_smat[i,s] + mflow[s,t] - flow, 
                                   norm = flow_smat[i,s] + mflow[s,t] - flow, 
                                   frac = flow_smat[i,s] + (mflow[s,t] - flow)/mflow[s, t])
        }
      }
    }
  }
  if (netflow_mode == "norm") {
    flow_smat <- flow_smat/maxoflo * 2
  }
  return(flow_smat)
}

depend_exp_fct <- function(g){
  A <- igraph::get.adjacency(g,"both",sparse=F)
  eigen_A <- eigen(A)
  n <- nrow(A)
  expA <- eigen_A$vectors %*% diag(exp(eigen_A$values)) %*% t(eigen_A$vectors)
  C <- (n-1)^2-(n-1)
  combet <- matrix(0,n,n)
  for(i in 1:n){
    E=matrix(0,n,n)
    E[which(A[,i]==1),i]=-1
    E[i,which(A[i,]==1)]=-1
    E <- A+E
    eigen_E <- eigen(E)
    expE <- eigen_E$vectors %*% diag(exp(eigen_E$values)) %*% t(eigen_E$vectors)
    expE <- (expA-expE)/expA
    expE[i,] <- 0
    expE[,i] <- 0
    diag(expE) <- 0
    combet[i,] <- 1/C*rowSums(expE)
  }
  
  return(combet)
}