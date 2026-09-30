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
#' data("dbces11")
#' P <- neighborhood_inclusion(dbces11)
#'
#' is_preserved(P, degree(dbces11))
#' is_preserved(P, betweenness(dbces11))
#' is_preserved(P, closeness(dbces11))
#' @export
is_preserved <- function(P, scores) {
    P <- check_partial_order(P)
    if (!is.numeric(scores) || length(scores) != nrow(P)) {
        stop("scores must be a numeric vector with one entry per row of P")
    }
    if (anyNA(scores)) {
        stop("scores must not contain NA")
    }
    P <- as.matrix(P)
    storage.mode(P) <- "integer"
    preserved <- preserve(P, as.numeric(scores), nrow(P)) == 0

    return(preserved)
}
