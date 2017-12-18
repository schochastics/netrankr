#' @title Indirect relations in a network
#' @description Derive indirect relations for a given network.
#' Observed relations, like presents or absence of a relation, are commonly not the center
#' of analysis, but are transformed in a new set of indirect relation like shortest path
#' distances among nodes. These transformations are usually an implicit step when centrality
#' indices are used. Making this step explicit gives more possibilities, for example
#' calculating partial centrality rankings with [positional_dominance].
#' @param g igraph object. The network for which relations should be derived.
#' @param type String giving the relation to be calculated. See Details for options.
#' @param lfparam Numeric parameter. Only used if type = "dist_lf".
#' @param dwparam Numeric parameter. Only used if type = "dist_walk".
#' @param netflowmode String, one of raw, frac, or norm. Only used if type = "depend_netflow".
#' @param rspxparam Numeric parameter. Only used if type = "depend_rsps" or type = "depend_rspn".
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
#' distances form a one-parametric family of distances, converging to shortest path distances as \eqn{\alpha -> 0}
#' and to the resistance distance as \eqn{\alpha -> \infty}. See (Chebotarev, 2011) for more details.
#' The parameter `lfparam` can be used to tune \eqn{\alpha}.
#'
#'\emph{'dist_walk'} returns the walk distance \eqn{d_\alpha^W(s,t)} between nodes. The walk distances form a one-parametric
#'family of distances, converging to shortest path distances as \eqn{\alpha -> 0} and to longest
#'walk distances for \eqn{\alpha -> \infty}. Walk distances contain the logarithmic forest
#'distances as a special case. See (Chebotarev, 2012) for more details.
#'
#' \emph{'dist_rwalk'} returns the expected length of a random walk between two nodes. For more
#' details see (Noh and Rieger, 2004)
#'
#' \emph{'depend_netflow'} returns dependencies based on network flow (See Freeman et al.,1991). 
#' If `netflowmode="raw"`, the function returns 
#' \deqn{\delta(u,s) = \sum_{t \in V} f(s,t,G)-f(s,t,G-v)} 
#' where f(s,t,G) is the maximum flow from s to t in G and f(s,t,G-v) in G without the node v.
#' For `netflowmode="frac"` it returns dependencies in the form, similar to shortest path dependencies:
#'\deqn{\delta(u,s) = \sum_{t \in V} \frac{f(s,t,G)-f(s,t,G-v)}{f(s,t,G)}} 
#'
#' \emph{'depend_curflow'} returns pairwise dependencies based on current flow. The relation is
#' based on the same idea as 'depend_sp' and 'depend_netflow'. However, instead of considering
#' shortest paths or network flow, the current flow (or equivalent: random walks) between nodes
#' are of interest. See (Newman, 2005) for details.
#'
#' \emph{'depend_exp'} returns pairwise dependencies based on 'communicability':
#' \deqn{\delta(u,s)=\sum_{t \in V} \frac{exp(A)_{st}-exp(A+E(u))_{st}}{exp(A)_{st}},}
#' where E(u) has nonzeros only in row and column u, and 
#' in this row and column has -1 if A has +1. See (Estrada et al., 2009) for additional details.
#'
#' \emph{'depend_rsps'}. Simple randomized shortest path dependencies. 
#' The simple RSP dependency of a node u with respect to absorbing paths from s to t, 
#' is defined as the expected number of visits through u over all s-t-walks. The
#' parameter `rspxparam` is the "inverse temperature parameter". 
#' If it converges to infinity, only shortest paths are considered and the expected
#' number of visits to a node on a shortest path approaches the probability of
#' following that particular path. When the parameter converges to zero, then the 
#' dependencies converge to the expected number of visits to a node over all absorbing
#' walks with respect to the unbiased random walk probabilities. This means for undirected networks,
#' that the relations converge to adjacency. See (Kivimäki et al., 2016) for details.
#'
#' \emph{'depend_rspn'} Net randomized shortest path dependencies. 
#' The parameter `rspxparam` is the "inverse temperature parameter". The asymptotic 
#' for the infinity case are the same as for 'depend_rsps'. If the parameter approaches zero, then
#' it converges to 'depend_curflow'. The net randomized shortest path dependencies
#' are closely related to the random walk interpretation of current flows.
#'  See (Kivimäki et al., 2016) for technical details.
#'
#'
#' The function \code{FUN} is used to transform the indirect
#' relation. See [transform_relations] for predefined functions and additional help.
#'
#' @return A matrix containing indirect relations in a network.
#' @author David Schoch
#' @seealso [aggregate_positions] to build centrality indices, [positional_dominance] to derive dominance relations
#' @references Chebotarev, P., 2012. The walk distances in graphs. *Discrete Applied Mathematics*, 160(10), pp.1484-1500.  
#' 
#' Chebotarev, P., 2011. A class of graph-geodetic distances generalizing the shortest-path and
#' the resistance distances. *Discrete Applied Mathematics* 159,295-302.
#'
#' Noh, J.D. and Rieger, H., 2004. Random walks on complex networks. *Physical Review Letters*, 92(11), p.118701.
#'
#' Freeman, L.C., Borgatti, S.P., and White, D.R., 1991.
#' Centrality in Valued Graphs: A Measure of Betweenness Based on Network Flow. *Social Networks* 13(2), 141-154.
#'
#' Newman, M.E., 2005. A measure of betweenness centrality based on random walks. *Social Networks*, 27(1), pp.39-54.
#'
#' Estrada, E., Higham, D.J., and Hatano, N., 2009.
#' Communicability betweenness in complex networks. *Physica A* 388,764-774.
#'
#' Kivimäki, I., Lebichot, B., Saramäki, J., and Saerens, M., 2016.
#' Two betweenness centrality measures based on Randomized Shortest Paths
#' *Scientific Reports* 6: 19668
#' @examples
#' library(igraph)
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#'
#' #shortest path distances
#' D <- indirect_relations(g,type = "dist_sp")
#'
#' #inverted shortest path distances
#' D <- indirect_relations(g,type = "dist_sp", FUN = dist_inv)

#' #shortes path dependencies (used for betweenness)
#' D <- indirect_relations(g,type = "depend_sp")
#'
#' #walks attenuated exponentially by there length
#' W <- indirect_relations(g,type = "walks",FUN = walks_exp)
#'
#' @export
indirect_relations <- function(g, 
                               type = "dist_sp",
                               lfparam = NULL, 
                               dwparam = NULL,
                               netflowmode = "",
                               rspxparam = NULL,
                               FUN = identity, ...) {
  if (type == "dependencies") {
    warning('type="dependencies" is deprecated. Using "depend_sp" instead.\n')
    type <- "depend_sp"
  }
  if (type == "geodesic") {
    warning('type="geodesic" is deprecated. Using "dist_sp" instead.\n')
    type <- "dist_sp"
  }
  if (type == "resistance") {
    warning('type="resistance" is deprecated. Using "dist_resist" instead.\n')
    type <- "dist_resist"
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
    A <- L + matrix(1 / n, n, n)
    C <- solve(A)
    rel <- resistanceDistance(C, n)
    rel <- FUN(rel, ...)
  } else if (type == "dist_lf") {
    if (is.null(lfparam)) {
      stop('argument "lfparam" is missing for "dist_lf", with no default')
    }
    rel <- log_forest_fct(g, lfparam)
    rel <- FUN(rel, ...)
  } else if (type == "dist_walk") {
    if (is.null(dwparam)) {
      stop('argument "dwparam" is missing for "dist_walk", with no default')
    }
    rel <- dist_walk_fct(g, dwparam)
    rel <- FUN(rel, ...)
  } else if (type == "depend_netflow") {
    if (netflowmode == "" | !netflowmode %in% c("raw", "frac", "norm")) {
      stop('netflowmode must be one of"raw","frac","norm"\n')
    }
    if (netflowmode == "norm") {
      warning('"norm" not supported yet. Using "frac" instead.\n')
      netflowmode <- "frac"
    }
    rel <- depend_netflow_fct(g, netflowmode)
    rel <- FUN(rel, ...)
  } else if (type == "depend_exp") {
    rel <- depend_exp_fct(g)
    rel <- FUN(rel, ...)
  } else if (type == "depend_rsps") {
    if (is.null(rspxparam)) {
      stop('argument "rspxparam" is missing for "depend_rsps", with no default')
    }
    rel <- depend_rsps_fct(g, rspxparam)
    rel <- FUN(rel, ...)
  } else if (type == "depend_rspn") {
    if (is.null(rspxparam)) {
      stop('argument "rspxparam" is missing for "depend_rspn", with no default')
    }
    rel <- depend_rspn_fct(g, rspxparam)
    rel <- FUN(rel, ...)
  } else if (type == "depend_curflow") {
    rel <- depend_curflow_fct(g)
    rel <- FUN(rel, ...)
  } else if (type == "dist_rwalk") {
    rel <- depend_curflow_fct(g)
    rel <- FUN(rel, ...)
  } else {
    stop(paste(type, "is not defined as indirect relation."))
  }

  return(rel)
}

# -------------------------------------------------------------------------------

log_forest_fct <- function(g, lfparam) {
  n <- igraph::vcount(g)
  gamma <- log(exp(1) + lfparam ^ (2 / n))

  L <- igraph::graph.laplacian(g, sparse = FALSE)
  I <- diag(1, n)
  Q <- solve(I + lfparam * L)

  if (lfparam == 1) {
    H <- gamma * log(Q)
  } else {
    H <- gamma * (lfparam - 1) * logb(Q, lfparam)
  }
  rel <- 0.5 * (diag(H) %*% t(rep(1, n)) + rep(1, n) %*% t(diag(H))) - H
  return(rel)
}

depend_netflow_fct <- function(g, netflowmode) {
  n <- igraph::vcount(g)
  mflow <- matrix(0, n, n)
  # maxflow
  for (s in 1:n) {
    for (t in 1:n) {
      if (s != t) {
        mflow[s, t] <- igraph::graph.maxflow(g, s, t)$value
      }
    }
  }
  if (netflowmode == "norm") {
    flo <- mflow
    diag(flo) <- 0
    maxoflo <- rep(0, n)
    for (i in 1:n) maxoflo[i] <- sum(mflow[-i, -i])
  }
  flow_smat <- matrix(0, n, n)
  for (i in 1:n) {
    g_i <- igraph::delete.vertices(g, i)
    for (s in 1:n) {
      for (t in 1:n) {
        if (i != s & s != t & i != t) {
          flow <- igraph::graph.maxflow(g_i, s - (s > i), t - (t > i))$value
          flow_smat[i, s] <- switch(netflowmode,
            raw = flow_smat[i, s] + mflow[s, t] - flow,
            norm = flow_smat[i, s] + mflow[s, t] - flow,
            frac = flow_smat[i, s] + (mflow[s, t] - flow) / mflow[s, t]
          )
        }
      }
    }
  }
  if (netflowmode == "norm") {
    flow_smat <- flow_smat / maxoflo * 2
  }
  return(flow_smat)
}

depend_exp_fct <- function(g) {
  A <- igraph::get.adjacency(g, "both", sparse = F)
  eigen_A <- eigen(A)
  n <- nrow(A)
  expA <- eigen_A$vectors %*% diag(exp(eigen_A$values)) %*% t(eigen_A$vectors)
  C <- (n - 1) ^ 2 - (n - 1)
  combet <- matrix(0, n, n)
  for (i in 1:n) {
    E <- matrix(0, n, n)
    E[which(A[, i] == 1), i] <- -1
    E[i, which(A[i, ] == 1)] <- -1
    E <- A + E
    eigen_E <- eigen(E)
    expE <- eigen_E$vectors %*% diag(exp(eigen_E$values)) %*% t(eigen_E$vectors)
    expE <- (expA - expE) / expA
    expE[i, ] <- 0
    expE[, i] <- 0
    diag(expE) <- 0
    combet[i, ] <- 1 / C * rowSums(expE)
  }

  return(combet)
}

depend_rsps_fct <- function(g, rspxparam) {
  n <- igraph::vcount(g)
  I <- diag(1, n)

  A <- igraph::get.adjacency(g, sparse = F)
  D <- diag(1 / igraph::degree(g))
  P_ref <- D %*% A
  C <- 1 / A
  C[is.infinite(C)] <- 0
  W <- P_ref * exp(-rspxparam * C)

  Z <- solve((I - W), I)
  Zdiv <- 1 / Z
  bet.mat <- t(sapply(1:n, function(x)
    (Z[x, ] * t(Zdiv)) %*% Z[, x] - sum(Z[x, ] * diag(Zdiv) * Z[, x])))
  diag(bet.mat) <- 0

  return(bet.mat)
}

depend_rspn_fct <- function(g, rspxparam) {
  n <- igraph::vcount(g)
  I <- diag(1, n)

  A <- igraph::get.adjacency(g, sparse = F)
  D <- diag(1 / igraph::degree(g))
  P_ref <- D %*% A
  C <- 1 / A
  C[is.infinite(C)] <- 0
  W <- P_ref * exp(-rspxparam * C)

  Z <- solve((I - W), I)
  Zdiv <- 1 / Z
  adj <- igraph::get.adjlist(g, "all")
  A <- lapply(adj, function(x) as.vector(x) - 1)
  bet.mat <- dependRspn(A, Z, Zdiv, W, n)
  return(bet.mat)
}

depend_curflow_fct <- function(g) {
  n <- igraph::vcount(g)
  D <- diag(igraph::degree(g))
  A <- igraph::get.adjacency(g, sparse = F)
  L <- D - A

  Tmat <- solve(L[1:(n - 1), 1:(n - 1)])
  Tmat <- rbind(cbind(Tmat, 0), 0)

  el <- igraph::get.edgelist(g, names = FALSE)
  m <- igraph::ecount(g)
  el <- el - 1L

  bet.mat <- dependCurFlow(Tmat, el, m, n)
  return(bet.mat)
}

dist_rwalk_fct <- function(g) {
  n <- igraph::vcount(g)
  A <- igraph::get.adjacency(g, sparse = FALSE)
  M <- A / rowSums(A)
  e <- rep(1, n - 1)
  H <- matrix(0, n, n)
  for (j in 1:n) {
    Mj <- M[-j, -j]
    Hij <- solve(diag(e) - Mj) %*% e
    H[j, -j] <- Hij # transposed to original (to fit framework with rowSums)
  }
  return(H)
}

dist_walk_fct <- function(g,dwparam) {
  n <- igraph::vcount(g)
  A <- igraph::get.adjacency(g, sparse = FALSE)
  bigeig <- eigen(A,only.values = TRUE)$values[1]
  if(dwparam>bigeig){
    stop(paste0("dwparam to large. To ensure convergence, use a value greater 0 and less than 1/",bigeig))
  }
  I <- diag(1,n)
  Rt <- solve((I-dwparam*A))
  Ht <- log(Rt)
  Dt <- 0.5 * (diag(Ht) %*% t(rep(1, n)) + rep(1, n) %*% t(diag(Ht))) - Ht
  alpha <- (1/dwparam - bigeig)^(-1)
  if(alpha!=1){
    gamma <- log(exp(1)+alpha^(2/n))*(alpha-1)/log(alpha)
  } else {
    gamma <- log(exp(1)+1)
  }
  Dt <- gamma * Dt
  return(Dt)
}