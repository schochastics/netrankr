#' @name swan_closeness
#' @title Impact on closeness when a node is removed
#'
#' @description
#' `swan_closeness` measures the change in the sum of the inverse of distances between all node pairs
#' when excluding that node.#'
#' @param g An `igraph` object representing the graph to analyze.
#'
#' @details
#' `swan_closeness` measures the impact of a node's removal by computing the change in
#' the sum of inverse distances between all node pairs.
#'
#' The code is an adaptation from the NetSwan package that was archived on CRAN.
#' @return
#' A numeric vector containing the `swan_closeness` values for all vertices.
#'
#' @references
#' Lhomme S. (2015). *Analyse spatiale de la structure des réseaux techniques dans un contexte de risques*.
#' Cybergeo: European Journal of Geography.
#' @examples
#' library(igraph)
#' # Example graph (electrical network structure)
#' elec <- matrix(ncol = 2, byrow = TRUE, c(
#'   11,1, 11,10, 1,2, 2,3, 2,9,
#'   3,4, 3,8, 4,5, 5,6, 5,7,
#'   6,7, 7,8, 8,9, 9,10
#' ))
#' gra <- graph_from_edgelist(elec, directed = FALSE)
#'
#' # Compute swan_closeness
#' f2 <- swan_closeness(gra)
#'
#' # Compare with betweenness centrality
#' bet <- betweenness(gra)
#' reg <- lm(bet ~ f2)
#' summary(reg)
#' @export
swan_closeness <- function(g) {
  n <- igraph::vcount(g)
  swancc <- rep(0, n)
  cc <- igraph::distances(g)
  ccb <- 1 / cc
  ccb[is.infinite(ccb)] <- 0
  tot <- sum(ccb)
  for (i in seq_len(n)) {
    g2 <- g
    g2 <- igraph::delete_vertices(g2, i)
    cc_no_i <- igraph::distances(g2)
    cc_no_ib <- 1 / cc_no_i
    cc_no_ib[is.infinite(cc_no_ib)] <- 0
    tot2 <- sum(cc_no_ib)
    swancc[i] <- tot2 - (tot - sum(ccb[i, ]) - sum(ccb[, i]))
  }
  return(swancc)
}

#' @name swan_combinatory
#' @title Error and attack tolerance of complex networks
#'
#' @description
#' `swan_combinatory` assesses network vulnerability and the resistance of networks
#' to node removals, whether due to random failures or intentional attacks.
#'
#' @param g An `igraph` object representing the graph to analyze.
#' @param k The number of iterations for assessing the impact of random failures.
#'
#' @details
#' Many complex systems display a surprising degree of tolerance against random failures.
#' However, this resilience often comes at the cost of extreme vulnerability to targeted attacks,
#' where removing key nodes (high-degree or high-betweenness nodes) can severely impact network connectivity.
#'
#' `swan_combinatory` simulates different attack strategies:
#' - **Random failure:** Nodes are removed randomly over multiple iterations.
#' - **Degree-based attack:** Nodes are removed in decreasing order of their degree.
#' - **Betweenness-based attack:** Nodes are removed in decreasing order of their betweenness centrality.
#' - **Cascading failure:** Nodes are removed based on recalculated betweenness after each removal.
#'
#' The function returns a matrix showing the connectivity loss for each attack scenario.
#'
#' The code is an adaptation from the NetSwan package that was archived on CRAN.
#' @return
#' A matrix with five columns:
#' \itemize{
#'   \item Column 1: Fraction of nodes removed.
#'   \item Column 2: Connectivity loss from betweenness-based attack.
#'   \item Column 3: Connectivity loss from degree-based attack.
#'   \item Column 4: Connectivity loss from cascading failure.
#'   \item Column 5: Connectivity loss from random failures (averaged over `k` iterations).
#' }
#'
#' @references
#' Albert R., Jeong H., Barabási A. (2000). *Error and attack tolerance of complex networks*.
#' Nature, 406(6794), 378-382.
#'
#' @examples
#' library(igraph)
#' # Example electrical network graph
#' elec <- matrix(ncol = 2, byrow = TRUE, c(
#'   11,1, 11,10, 1,2, 2,3, 2,9,
#'   3,4, 3,8, 4,5, 5,6, 5,7,
#'   6,7, 7,8, 8,9, 9,10
#' ))
#' gra <- graph_from_edgelist(elec, directed = FALSE)
#'
#' # Compute vulnerability measures
#' f4 <- swan_combinatory(gra, 10)
#' @export
swan_combinatory <- function(g, k) {
  n <- igraph::vcount(g)
  dist <- igraph::distances(g)
  dist[is.infinite(dist)] <- 0
  dist[dist > 0] <- 1
  tot <- sum(dist)
  fin <- matrix(ncol = 5, nrow = n, 0)
  mat <- matrix(ncol = 2, nrow = n, 0)
  mat[, 1] <- 1:n
  bet <- igraph::betweenness(g)
  mat[, 2] <- bet
  matri <- mat[order(mat[, 2]), ]
  g2 <- g
  for (i in seq_len(n)) {
    v = n + 1 - i
    g2 <- igraph::delete_vertices(g2, matri[v, 1])
    dist2 <- igraph::distances(g2)
    dist2[is.infinite(dist2)] <- 0
    dist2[dist2 > 0] <- 1
    tot2 <- sum(dist2)
    fin[i, 1] <- i / n
    fin[i, 2] <- tot - tot2
    matri[matri[, 1] > matri[v, 1], 1] <- matri[matri[, 1] > matri[v, 1], 1] - 1
  }
  mat <- matrix(ncol = 2, nrow = n, 0) #degree attack
  mat[, 1] <- 1:n
  deg <- igraph::degree(g)
  mat[, 2] <- deg
  matri <- mat[order(mat[, 2]), ]
  g2 <- g
  for (i in seq_len(n)) {
    v = n + 1 - i
    g2 <- igraph::delete_vertices(g2, matri[v, 1])
    dist2 <- igraph::distances(g2)
    dist2[is.infinite(dist2)] <- 0
    dist2[dist2 > 0] <- 1
    tot2 <- sum(dist2)
    fin[i, 3] <- tot - tot2
    matri[matri[, 1] > matri[v, 1], 1] <- matri[matri[, 1] > matri[v, 1], 1] -
      1 #bluff
  }
  g2 <- g #cascading
  npro <- n
  lim <- n - 1
  for (i in 1:lim) {
    mat <- matrix(ncol = 2, nrow = npro, 0)
    mat[, 1] <- 1:npro
    bet <- igraph::betweenness(g2)
    mat[, 2] <- bet
    matri <- mat[order(mat[, 2]), ]
    g2 <- igraph::delete_vertices(g2, matri[npro, 1])
    dist2 <- igraph::distances(g2)
    dist2[is.infinite(dist2)] <- 0
    dist2[dist2 > 0] <- 1
    tot2 <- sum(dist2)
    fin[i, 4] <- tot - tot2
    npro <- npro - 1
  }
  fin[n, 4] <- tot
  #random
  for (l in seq_len(k)) {
    al <- sample(1:n, n)
    g2 <- g
    for (i in seq_len(k)) {
      g2 <- igraph::delete_vertices(g2, al[i])
      dist2 <- igraph::distances(g2)
      dist2[is.infinite(dist2)] <- 0
      dist2[dist2 > 0] <- 1
      tot2 <- sum(dist2)
      fin[i, 5] <- fin[i, 5] + (tot - tot2)
      al[al > al[i]] <- al[al > al[i]] - 1 #bluff
    }
  }
  fin[, 2:4] <- fin[, 2:4] / tot
  fin[, 5] <- fin[, 5] / tot / k
  return(fin)
}

#' @name swan_connectivity
#' @title Impact on connectivity when a node is removed
#'
#' @description
#' `swan_connectivity` measures the loss of connectivity when a node is removed from the network.
#'
#' @usage
#' swan_connectivity(g)
#'
#' @param g An `igraph` object representing the graph to analyze.
#'
#' @details
#' Connectivity loss indices quantify the decrease in the number of relationships between nodes
#' when one or more components are removed. `swan_connectivity` computes the connectivity loss
#' by systematically excluding each node and evaluating the resulting changes in the network structure.
#'
#' The code is an adaptation from the NetSwan package that was archived on CRAN.
#' @return
#' A numeric vector where each entry represents the connectivity loss when the corresponding node is removed.
#'
#' @references
#' Lhomme S. (2015). *Analyse spatiale de la structure des réseaux techniques dans un contexte de risques*.
#' Cybergeo: European Journal of Geography.
#' @examples
#' library(igraph)
#' # Example graph (electrical network structure)
#' elec <- matrix(ncol = 2, byrow = TRUE, c(
#'   11,1, 11,10, 1,2, 2,3, 2,9,
#'   3,4, 3,8, 4,5, 5,6, 5,7,
#'   6,7, 7,8, 8,9, 9,10
#' ))
#' gra <- graph_from_edgelist(elec, directed = FALSE)
#'
#' # Compute connectivity loss
#' f3 <- swan_connectivity(gra)
#' @export
swan_connectivity <- function(g) {
  n <- igraph::vcount(g)
  fin <- rep(0, n)
  dist <- igraph::distances(g)
  dist[!is.infinite(dist)] <- 0
  dist[is.infinite(dist)] <- 1
  con <- sum(dist)
  for (i in seq_len(n)) {
    g2 <- g
    g2 <- igraph::delete_vertices(g2, i)
    dist2 <- igraph::distances(g2)
    dist2[!is.infinite(dist2)] <- 0
    dist2[is.infinite(dist2)] <- 1
    con2 <- sum(dist2)
    fin[i] <- con2 - con
  }
  return(fin)
}

#' @name swan_efficiency
#' @title Impact on farness when a node is removed
#'
#' @description
#' `swan_efficiency` measures the change in the sum of distances between all node pairs
#' when excluding a node from the network.
#'
#' @param g An `igraph` object representing the graph to analyze.
#' `swan_efficiency` is based on geographic accessibility, similar to indices used for
#' assessing transportation network performance, such as closeness accessibility.
#' It quantifies the impact of node removal by calculating the change in the sum of
#' distances between all node pairs.
#'
#' The code is an adaptation from the NetSwan package that was archived on CRAN.
#'
#' @return
#' A numeric vector where each entry represents the `swan_efficiency` value for the
#' corresponding node.
#'
#' @references
#' Lhomme S. (2015). *Analyse spatiale de la structure des réseaux techniques dans un
#' contexte de risques*. Cybergeo: European Journal of Geography.
#'
#' @examples
#' library(igraph)
#' # Example graph (electrical network structure)
#' elec <- matrix(ncol = 2, byrow = TRUE, c(
#'   11,1, 11,10, 1,2, 2,3, 2,9,
#'   3,4, 3,8, 4,5, 5,6, 5,7,
#'   6,7, 7,8, 8,9, 9,10
#' ))
#' gra <- graph_from_edgelist(elec, directed = FALSE)
#'
#' # Compute efficiency impact of node removal
#' f2 <- swan_efficiency(gra)
#' bet <- betweenness(gra)
#' reg <- lm(bet ~ f2)
#' summary(reg)
#' @export
swan_efficiency <- function(g) {
  n <- igraph::vcount(g)
  fin <- rep(0, n)
  dt = igraph::distances(g)
  tot = sum(dt)
  for (i in 1:n) {
    g2 <- g
    g2 <- igraph::delete_vertices(g2, i)
    dt2 <- igraph::distances(g2)
    tot2 <- sum(dt2)
    fin[i] <- tot2 - (tot - sum(dt[i, ]) - sum(dt[, i]))
  }
  return(fin)
}
