#' @title Hyperbolic (centrality) index
#' @description The hyperbolic index is an index that considers all closed
#' walks of even or odd length on induced neighborhoods of a vertex.
#' @param g igraph object.
#' @param type string. 'even' if only even length walks should be considered. 'odd' (Default)
#' if only odd length walks should be used.
#' @details The hyperbolic index is an illustrative index that should
#' not be used for any serious analysis. Its purpose is to show that with enough mathematical
#' trickery, any desired result can be obtained when centrality indices are used.
#' @return A vector containing centrality scores.
#' @author David Schoch
#' @examples
#'
#' library(igraph)
#'
#' data("dbces11")
#' hyperbolic_index(dbces11, type = "odd")
#' hyperbolic_index(dbces11, type = "even")
#' @export
hyperbolic_index <- function(g, type = "odd") {
    if (!igraph::is_igraph(g)) {
        stop("g must be an igraph object")
    }
    if (igraph::is_directed(g)) {
        stop("g must be an undirected graph")
    }
    if (!is.character(type) || length(type) != 1 || !type %in% c("even", "odd")) {
        stop("type must be even or odd")
    }
    f <- if (type == "even") cosh else sinh
    g <- strip_weights(g)
    n <- igraph::vcount(g)
    ENW <- rep(0, n)
    for (v in seq_len(n)) {
        Nv <- igraph::neighborhood(g, 1, v)[[1]]
        if (length(Nv) == 1) {
            # an isolated node has no walks in its neighborhood
            next
        }
        g1 <- igraph::induced_subgraph(g, Nv)
        C <- adjacency_matrix(g1)
        eig.decomp <- eigen(C, symmetric = TRUE)
        V <- (eig.decomp$vectors)^2
        lambda <- eig.decomp$values
        ENW[v] <- sum(V %*% f(lambda)) * igraph::edge_density(g1)
    }
    return(ENW)
}
