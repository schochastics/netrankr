#' @name swan_closeness
#' @title Impact on closeness when a node is removed
#'
#' @description
#' `swan_closeness` measures the change in the sum of the inverse of distances between all node pairs
#' when excluding that node.
#'
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
  check_swan_graph(g)
  n <- igraph::vcount(g)
  swancc <- rep(0, n)
  ccb <- inverse_distances(g)
  tot <- sum(ccb)
  for (i in seq_len(n)) {
    tot2 <- sum(inverse_distances(igraph::delete_vertices(g, i)))
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
  check_swan_graph(g)
  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 1) {
    stop("k must be a positive number")
  }
  n <- igraph::vcount(g)
  if (n < 2) {
    stop("g must have at least two vertices")
  }
  tot <- connected_pairs(g)
  fin <- matrix(ncol = 5, nrow = n, 0)
  fin[, 1] <- seq_len(n) / n

  # connectivity loss when removing the vertices in `ord` one after another
  loss_sequence <- function(ord) {
    vapply(seq_len(n), function(i) {
      tot - connected_pairs(igraph::delete_vertices(g, ord[seq_len(i)]))
    }, numeric(1))
  }
  # static attacks: highest betweenness/degree first (ties: highest index first)
  fin[, 2] <- loss_sequence(rev(order(igraph::betweenness(g))))
  fin[, 3] <- loss_sequence(rev(order(igraph::degree(g))))

  # cascading: recompute betweenness after each removal
  g2 <- g
  for (i in seq_len(n - 1)) {
    bet <- igraph::betweenness(g2)
    g2 <- igraph::delete_vertices(g2, order(bet)[length(bet)])
    fin[i, 4] <- tot - connected_pairs(g2)
  }
  fin[n, 4] <- tot

  # random failures
  for (l in seq_len(k)) {
    fin[, 5] <- fin[, 5] + loss_sequence(sample(seq_len(n), n))
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
  check_swan_graph(g)
  n <- igraph::vcount(g)
  # number of ordered pairs that cannot reach each other
  disconnected_pairs <- function(g) {
    m <- igraph::vcount(g)
    m * (m - 1) - connected_pairs(g)
  }
  con <- disconnected_pairs(g)
  vapply(seq_len(n), function(i) {
    disconnected_pairs(igraph::delete_vertices(g, i)) - con
  }, numeric(1))
}

#' @name swan_efficiency
#' @title Impact on farness when a node is removed
#'
#' @description
#' `swan_efficiency` measures the change in the sum of distances between all node pairs
#' when excluding a node from the network.
#'
#' @param g An `igraph` object representing the graph to analyze.
#'
#' @details
#' `swan_efficiency` is based on geographic accessibility, similar to indices used for
#' assessing transportation network performance, such as closeness accessibility.
#' It quantifies the impact of node removal by calculating the change in the sum of
#' distances between all node pairs.
#'
#' As in NetSwan, the sum of distances is infinite if the graph is disconnected.
#' The value of a node is therefore `Inf` if its removal disconnects the graph and
#' `NaN` for all nodes if the graph is already disconnected.
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
  check_swan_graph(g)
  n <- igraph::vcount(g)
  fin <- rep(0, n)
  dt <- igraph::distances(g)
  tot <- sum(dt)
  for (i in seq_len(n)) {
    tot2 <- sum(igraph::distances(igraph::delete_vertices(g, i)))
    fin[i] <- tot2 - (tot - sum(dt[i, ]) - sum(dt[, i]))
  }
  return(fin)
}

# helpers ----------------------------------------------------------------------

check_swan_graph <- function(g) {
  if (!igraph::is_igraph(g)) {
    stop("g must be an igraph object", call. = FALSE)
  }
}

# number of ordered pairs of distinct vertices that are connected by a path
connected_pairs <- function(g) {
  cs <- igraph::components(g, mode = "weak")$csize
  sum(cs * (cs - 1))
}

# inverse shortest path distances with 0 for unreachable pairs and the diagonal
inverse_distances <- function(g) {
  d <- 1 / igraph::distances(g)
  d[is.infinite(d)] <- 0
  d
}
