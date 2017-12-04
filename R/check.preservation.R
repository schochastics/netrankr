#' @title Check preservation
#' @description Checks if a partial ranking is preserved in the ranking induced by `scores`.
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @param scores Numeric vector containing the scores of a centrality index.
#' @details In order for a score vector to preserve a partial ranking, the following 
#' condition must be fulfilled:
#' \code{P[u,v]==1 & scores[i]<=scores[j]}.
#' @return Logical scaler whether \code{scores} preserves the relations in \code{P}.
#' @author David Schoch
#' @examples
#' 
#' library(igraph)
#' # standard measures of centrality preserve the neighborhood inclusion preorder
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' P<-neighborhood_inclusion(g)
#' 
#' is_preserved(P,degree(g))
#' is_preserved(P,betweenness(g))
#' is_preserved(P,closeness(g))
#' @export
is_preserved <- function(P, scores) {
    n <- nrow(P)
    preserved <- preserve(P, scores, n) == 0
    
    return(preserved)
}
