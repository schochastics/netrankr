#' @title Transitive Reduction 
#' @description  Calculates the transitive reduction of a partial ranking.
#'
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance]. 
#' @return transitive reduction of `P`
#' @author David Schoch
#' @examples
#' require(igraph)
#' 
#' g <- threshold_graph(100,0.1)
#' P <- neighborhood_inclusion(g)
#' sum(P)
#' 
#' R <- transitive_reduction(P)
#' sum(R)
#' @export
transitive_reduction <- function(P) {
  B <- transreduct(P)
  return(B)
}
