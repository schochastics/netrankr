#' @title Spectral gap of a graph
#' @description  The spectral (or eigen) gap of a graph is the absolute
#'  difference between the biggest and second biggest eigenvalue
#' of the adjacency matrix. To compare spectral gaps across networks, the fraction can be used.
#'
#' @param g igraph object
#' @param method A string, either "frac" or "abs"
#' @return Numeric value
#' @details The spectral gap is bounded between 0 and 1 if `method="frac"`, except for
#' complete graphs where the second largest eigenvalue is negative. The closer
#' the value to one, the bigger the gap. Edge weights are ignored.
#' @author David Schoch
#' @examples
#' # The fractional spectral gap of a threshold graph is usually close to 1
#' g <- threshold_graph(50, 0.3)
#' spectral_gap(g, method = "frac")
#' @export
#'
spectral_gap <- function(g, method = "frac") {
    if (!igraph::is_igraph(g)) {
        stop("g must be an igraph object")
    }
    if (igraph::is_directed(g)) {
        stop("g must be an undirected graph")
    }
    if (!is.character(method) || length(method) != 1 || !method %in% c("frac", "abs")) {
        stop("method must be one of 'frac' or 'abs'")
    }
    if (igraph::vcount(g) < 2) {
        stop("g must have at least two vertices")
    }
    A <- igraph::as_adjacency_matrix(strip_weights(g), "both", sparse = FALSE)
    spec_decomp <- eigen(A, symmetric = TRUE, only.values = TRUE)$values[c(1, 2)]
    if (method == "frac") {
        if (spec_decomp[1] == 0) {
            # empty graph: all eigenvalues are zero
            return(0)
        }
        return(1 - spec_decomp[2] / spec_decomp[1])
    }
    spec_decomp[1] - spec_decomp[2]
}
