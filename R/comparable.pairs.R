#' @title Comparable pairs in a partial order
#' @description  Calculates the fraction of comparable pairs in a partial order.
#' @param P A partial order as matrix object, e.g. calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @return Fraction of comparable pairs in `P`.
#' @author David Schoch
#' @seealso [incomparable_pairs]
#' @examples
#' library(igraph)
#' g <- sample_gnp(100, 0.1)
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#' # All pairs of vertices are comparable in a threshold graph
#' tg <- threshold_graph(100, 0.3)
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#' @export
comparable_pairs <- function(P) {
    P <- check_partial_order(P)
    igraph::edge_density(igraph::graph_from_adjacency_matrix(P, "max"))
}

#' @title Incomparable pairs in a partial order
#' @description  Calculates the fraction of incomparable pairs in a partial order.
#' @param P A partial order as matrix object, e.g. calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @return Fraction of incomparable pairs in `P`.
#' @author David Schoch
#' @seealso [comparable_pairs]
#' @examples
#' library(igraph)
#' g <- sample_gnp(100, 0.1)
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#' # All pairs of vertices are comparable in a threshold graph
#' tg <- threshold_graph(100, 0.3)
#' P <- neighborhood_inclusion(tg)
#' incomparable_pairs(P)
#' @export
incomparable_pairs <- function(P) {
    P <- check_partial_order(P)
    igraph::edge_density(igraph::complementer(igraph::graph_from_adjacency_matrix(P, "max")))
}
