#' @title Transitive Reduction of Dominance Relations
#' @description  Calculates the transitive reduction of a partial ranking.
#'
#' @param P matrix containing relations
#' @return transitive reduction of P
#' @author David Schoch
#' @examples
#' require(igraph)
#' g <- threshold_graph(100,0.1)
#' P <- neighborhood_inclusion(g)
#' sum(P)
#' T <- transitive_reduction(P)
#' sum(T)
#' @export
transitive_reduction <- function(P) {
    B <- transreduct(P)
    return(B)
}
