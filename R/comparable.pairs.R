comparable_pairs=function(P){
  #' @title Comparable pairs of a partial order
  #' @description  Calculates the fraction of comparable pairs in a partial order. This fraction is identical to
  #' the density of the induced undirected graph of P.
  #'
  #' @param P matrix containing comparability relation.
  #' @return fraction of comparable pairs in P.
  #' @examples
  #' require(igraph)
  #' g <- erdos.renyi.game(100,0.1)
  #' P <- neighborhood_inclusion(g)
  #' comparable_pairs(P)
  #' # All pairs of vertices are comparable in a threshold graph
  #' tg <- threshold_graph(100,0.3)
  #' P <- neighborhood_inclusion(g)
  #' comparable_pairs(P)
  #' @export
  round(igraph::graph.density(igraph::graph_from_adjacency_matrix(P,"max")),8)
}
