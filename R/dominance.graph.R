#' @title Directed Comparability Graph
#' @description Turns a partial ranking into a directed graph.
#'
#' @param P Partial ranking as matrix object
#' @return igraph object.
#' @details An edge (u,v) is present if u is dominated by v in P.
#' @author David Schoch
#' @examples
#' require(igraph)
#' g <- threshold_graph(20,0.1)
#' P <- neighborhood_inclusion(g)
#' d <- dominance_graph(P)
#' plot(d)
#' # to reduce overplotting use transitive reduction
#' P <- transitive_reduction(P)
#' d <- dominance_graph(P)
#' plot(d)
#' @export
dominance_graph <- function(P){
  d <- igraph::graph_from_adjacency_matrix(P,"directed")
  # if(!is.null(igraph::V(g)$name)){
  #   igraph::V(d)$name <- igraph::V(g)$name
  # }
  return(d)
}
