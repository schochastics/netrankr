#' @title Neighborhood-inclusion preorder
#' @description Calculates the neighborhood-inclusion preorder of an undirected graph.
#' @param g An igraph object
#' @param sparse Logical scalar, whether to create a sparse matrix
#' @importClassesFrom Matrix dgCMatrix
#' @details Neighborhood-inclusion is defined as
#' \deqn{N(u)\subseteq N[v]}
#' where \eqn{N(u)} is the neighborhood of \eqn{u} and \eqn{N[v]=N(v)\cup \lbrace v\rbrace} is the closed neighborhood of \eqn{v}.
#' \eqn{N(u) \subseteq N[v]} implies that \eqn{c(u) \leq c(v)},
#' where \eqn{c} is a centrality index based on a specific path algebra. Indices
#' falling into this category are closeness (and variants), betweenness
#' (and variants) as well as many walk-based indices (eigenvector and subgraph
#' centrality, total communicability,...).
#' @return The neighborhood-inclusion preorder of \code{g} as matrix object. \code{P[u,v]=1} if \eqn{N(u)\subseteq N[v]}
#' @author David Schoch
#' @references Schoch, D. and Brandes, U., 2016. Re-conceptualizing centrality in social networks.
#' *European Journal of Applied Mathematics* 27(6), 971-985.
#'
#' Brandes, U. Heine, M., Müller, J. and Ortmann, M., 2017.
#' Positional Dominance: Concepts and Algorithms.
#' *Conference on Algorithms and Discrete Applied Mathematics*, 60-71.
#'
#'
#'
#'
#' @seealso [positional_dominance], [exact_rank_prob]
#' @examples
#' library(igraph)
#' # the neighborhood inclusion preorder of a star graph is complete
#' g <- graph.star(5, "undirected")
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#'
#' # the same holds for threshold graphs
#' tg <- threshold_graph(50, 0.1)
#' P <- neighborhood_inclusion(tg)
#' comparable_pairs(P)
#'
#' # standard centrality indices preserve neighborhood-inclusion
#' data("dbces11")
#' P <- neighborhood_inclusion(dbces11)
#'
#' is_preserved(P, degree(dbces11))
#' is_preserved(P, closeness(dbces11))
#' is_preserved(P, betweenness(dbces11))
#' @export
neighborhood_inclusion <- function(g, sparse = FALSE) {
  if (!igraph::is_igraph(g)) {
    stop("g must be an igraph object")
  }

  if (igraph::is_directed(g)) {
    stop("g must be an undirected graph")
  }

  adj <- lapply(as_adj_list_fast(g), function(x) x - 1)
  deg <- igraph::degree(g)
  dom <- nialgo(adj, deg)
  if (!sparse) {
    dom <- as.matrix(dom)
  }
  if (!is.null(igraph::V(g)$name)) {
    rownames(dom) <- colnames(dom) <- igraph::V(g)$name
  }
  return(dom)
}


as_adj_list_fast <- function(g){
  n <- igraph::vcount(g)
  lapply(1:n,function(i){
    x <- g[[i]][[1]]
    attr(x,"env") <- NULL
    attr(x,"graph") <- NULL
    class(x) <- NULL
    x
  })
}