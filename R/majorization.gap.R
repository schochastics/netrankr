#' @title Majorization gap
#' @description  Calculates the (normalized) majorization gap of an undirected graph.
#' The majorization gap indicates how far the degree sequence of a graph is
#' from a degree sequence of a [threshold_graph].
#'
#' @param g An igraph object
#' @param norm `True` (Default) if the normalized majorization gap should be returned.
#' @details The distance is measured by the number of \emph{reverse unit
#' transformations} necessary to turn the degree sequence into a threshold sequence.
#' First, the \emph{corrected conjugated degree sequence} d' is calculated from the degree sequence d as follows:
#' \deqn{d'_k= |\{ i : i<k \land d_i\geq k-1 \} | +
#' | \{ i : i>k \land d_i\geq k \} |.}
#' the majorization gap is then defined as
#' \deqn{1/2 \sum_{k=1}^n \max\{d'_k - d_k,0\}}
#' The higher the value, the further away is a graph to be a threshold graph.
#' If `norm = TRUE`, the gap is divided by the number of edges.
#' For disconnected graphs, the gaps of all components are added up
#' (and then normalised by the total number of edges).
#' @return Majorization gap of an undirected graph.
#' @author David Schoch
#' @references Schoch, D., Valente, T. W. and Brandes, U., 2017. Correlations among centrality
#' indices and a class of uniquely ranked graphs. *Social Networks* **50**, 46–54.
#'
#' Arikati, S.R. and Peled, U.N., 1994. Degree sequences and majorization.
#' *Linear Algebra and its Applications*, **199**, 179-211.
#'
#' @examples
#' library(igraph)
#' g <- make_star(5, "undirected")
#' majorization_gap(g) # 0 since star graphs are threshold graphs
#'
#' g <- sample_gnp(100, 0.15)
#' majorization_gap(g, norm = TRUE) # fraction of reverse unit transformation
#' majorization_gap(g, norm = FALSE) # number of reverse unit transformation
#' @export
majorization_gap <- function(g, norm = TRUE) {
    if (!igraph::is_igraph(g)) {
        stop("g must be an igraph object")
    }

    if (igraph::is_directed(g)) {
        stop("g must be an undirected graph")
    }

    if (!igraph::is_connected(g)) {
        warning("graph is not connected. Computing the gap for each component separately and returning the sum.")
    }
    comps <- igraph::components(g)
    gap <- 0
    for (i in seq_len(comps$no)) {
        g1 <- igraph::induced_subgraph(g, which(comps$membership == i))
        gap <- gap + unit_transformations(igraph::degree(g1))
    }
    if (norm) {
        m <- igraph::ecount(g)
        gap <- if (m == 0) 0 else gap / m
    }
    return(gap)
}

# number of reverse unit transformations turning the degree sequence into a threshold sequence
unit_transformations <- function(deg) {
    n <- length(deg)
    deg.sorted <- sort(deg, decreasing = TRUE)
    deg.cor <- vapply(seq_len(n), function(k) {
        sum(deg.sorted[seq_len(k - 1)] >= (k - 1)) + sum(deg.sorted[seq_len(n) > k] >= k)
    }, numeric(1))
    gap <- deg.cor - deg.sorted
    0.5 * sum(gap[gap >= 0])
}
