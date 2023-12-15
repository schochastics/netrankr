#' @title Rankings that extend a partial ranking
#' @description Returns all possible rankings that extend a partial ranking.
#'
#' @param data List as returned by [exact_rank_prob] when run with `only.results = FALSE`
#' @param force Logical scalar. Stops function if the number of rankings is too large.
#' Only change to TRUE if you know what you are doing
#' @details The `i`th row of the matrix contains the rank of node `i` in all possible rankings
#' that are in accordance with the partial ranking `P`. The lowest rank possible is
#' associated with `1`.
#' @return A matrix containing ranks of nodes in all possible rankings.
#' @author David Schoch
#' @examples
#' P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
#' P
#' res <- exact_rank_prob(P, only.results = FALSE)
#' get_rankings(res)
#' @export

get_rankings <- function(data, force = FALSE) {
    if (!"netrankr_full" %in% class(data)) {
        stop("data is not a netrankr_full object")
    }

    if (is.null(data$tree)) {
        stop("input does not include all necessary data structures. run exact_rank_prob() with 'only.results = FALSE'")
    }
    lattice <- data$lattice
    ideals <- data$ideals
    topo.order <- data$topo.order
    linext <- data$lin.ext
    mse <- data$mse

    if (linext > 50000 && !force) {
        stop("number of possible rankings is very high. Use 'force = FALSE'
           if you know what you are doing.")
    }

    n <- length(unique(mse))
    lattice <- lapply(lattice, function(x) x + 1)
    g <- igraph::graph_from_adj_list(lattice, mode = "in")
    paths <- igraph::all_shortest_paths(g, from = n + 1, to = 1)

    paths <- lapply(paths$res, function(x) as.vector(x) - 1)
    rks <- rankings(paths, ideals, linext, n)
    rks <- rks + 1
    rks <- rks[order(topo.order), ]
    rks <- rks[mse, ]
    return(rks)
}
