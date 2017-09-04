#' @title Estimate rank probabilities with MCMC
#' @description Performs a probabilistic rank analysis based on an almost uniform 
#' sample of possible rankings that preserve a partial ranking.
#' @param P a matrix representing a partial ranking.
#' @param rp integer indicating the number of samples to be drawn.
#' @details This function can be used instead of [exact_rank_prob]
#' if the number of nodes is too large for an exact computation. As a rule of thumb, 
#' the number of samples should be at least cubic in the number of nodes of the network. 
#' See online manual for benchmark results.
#' @return 
#' \item{expected.rank}{Estimated expected ranks of nodes}
#' \item{relative.rank}{Matrix containing estimated relative rank probabilities:
#' \code{relative.rank[u,v]} is the probability that u is ranked lower than v.}
#' 
#' @references Bubley, R. and Dyer, M., 1999. Faster random generation of linear extensions. 
#' *Discrete Mathematics*, 201(1):81-88
#' 
#' @seealso [exact_rank_prob], [approx_rank_relative], [approx_rank_expected]
#' @author David Schoch
#' @examples
#' # Will be added in later version
#' @export
mcmc_rank_prob <- function(P,rp=nrow(P)^3){
  n.full <- nrow(P)
  MSE <- which((P+t(P))==2,arr.ind=T)
  if(length(MSE)>=1){
    MSE <- t(apply(MSE,1,sort))
    MSE <- MSE[!duplicated(MSE),]
    g <- igraph::graph.empty()
    g <- igraph::add.vertices(g,nrow(P))
    g <- igraph::add.edges(g,c(t(MSE)))
    g <- igraph::as.undirected(g)
    MSE <- igraph::clusters(g)$membership
    equi <- which(duplicated(MSE))
    P <- P[-equi,-equi]
  } else{
    MSE <- 1:nrow(P)
  }
  n <- nrow(P)
  init.rank <- as.vector(igraph::topological.sort(igraph::graph_from_adjacency_matrix(P,"directed")))
  res <- mcmc_rank(P,init.rank-1,rp)
  res$expected <- res$expected+1
  expected.full <- c(0,n.full)
  rrp.full <- matrix(0,n.full,n.full)
  for(i in sort(unique(MSE))){
    idx <- which(MSE==i)
    if(length(idx)>1){
      group.head <- i
      expected.full[idx] <- res$expected[group.head]
      rrp.full[idx,] <- do.call(rbind, replicate(length(idx), res$rrp[group.head,MSE], simplify=FALSE))
    }
    else if(length(idx)==1){
      expected.full[idx] <- res$expected[i]
      rrp.full[idx,] <- res$rrp[i,MSE]
    }
  }
  expected.full <- expected.full+sum(duplicated(MSE))
  return(list(expected.rank=expected.full,relative.rank=rrp.full))
}

