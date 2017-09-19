#' @title Comparable pairs in a partial ranking
#' @description  Calculates the fraction of comparable pairs in a partial ranking.
#' This fraction is identical to the density of the induced undirected graph of 
#' a partial ranking.
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @return Fraction of comparable pairs in `P`.
#' @author David Schoch
#' @examples
#' require(igraph)
#' g <- sample_gnp(100,0.1)
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#' # All pairs of vertices are comparable in a threshold graph
#' tg <- threshold_graph(100,0.3)
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#' @export
comparable_pairs <- function(P) {
    round(igraph::graph.density(igraph::graph_from_adjacency_matrix(P, "max")), 8)
}
