#' @title Estimate Rankings with MCMC
#' @description Performs a probabilistic rank analysis based on an almost uniform 
#' sample of possible rankings that preserve the partial ranking, instead of taking all possibilities
#' as in [exact_rank_prob].
#' @param P a matrix representing a partial ranking.
#' @param rp integer indicating the number of samples to be drawn.
#' @details As a rule of thumb, the number of samples should at least be cubic in 
#' the number of nodes of the network, but the higher the better. 
#' 
#' @return data frame containing approximated expected ranks and relative rank probabilities.
#' 
#' #' @seealso [exact_rank_prob], [approx_rank_relative], [approx_rank_expected]
#' @author David Schoch
#' @examples
#' ###TODO
#' @export
mcmc_rank_prob <- function(P,rp=10000){
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
  return(list(expected=expected.full,rrp=rrp.full))
}

