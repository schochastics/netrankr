#' @title Partial ranking as directed graph
#' @description Turns a partial ranking into a directed graph. An edge (u,v) is 
#' present if `P[u,v]=1`, meaning that u is dominated by v.
#'
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @return Directed graph as an igraph object.
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- threshold_graph(20,0.1)
#' P <- neighborhood_inclusion(g)
#' d <- dominance_graph(P)
#' \dontrun{plot(d)}
#' 
#' # to reduce overplotting use transitive reduction
#' P <- transitive_reduction(P)
#' d <- dominance_graph(P)
#' \dontrun{plot(d)}
#' @export
dominance_graph <- function(P) {
  if(!inherits(P, "Matrix") & !is.matrix(P)){
    stop("P must be a dense or spare matrix")
  }
  if(!is.binary(P)){
    stop("P is not a binary matrix")
  }
  d <- igraph::graph_from_adjacency_matrix(P, "directed")
  return(d)
}
