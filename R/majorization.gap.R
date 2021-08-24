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
#' g <- graph.star(5, "undirected")
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
    warning("graph is not connected. Computing for each component separately and returning sum.")
  }
  comps <- igraph::components(g)
  if (comps$no > 1) {
    gap <- 0
    for (i in 1:comps$no) {
      g1 <- igraph::induced_subgraph(g, which(comps$membership == i))
      if (igraph::ecount(g1) != 0) {
        n <- igraph::vcount(g)
        deg.sorted <- sort(igraph::degree(g1), decreasing = TRUE)
        deg.cor <- sapply(1:n, function(k) {
          length(which(deg.sorted[which((1:n) < k)] >= (k - 1))) + length(which(deg.sorted[which((1:n) > k)] >= k))
        })
        gap1 <- deg.cor - deg.sorted
        if (!norm) {
          gap1 <- 0.5 * sum(gap1[gap1 >= 0])
        } else {
          gap1 <- 0.5 * sum(gap1[gap1 >= 0]) / igraph::ecount(g1)
        }
        gap <- gap + gap1
      }
    }
  } else {
    n <- igraph::vcount(g)
    deg.sorted <- sort(igraph::degree(g), decreasing = TRUE)
    deg.cor <- sapply(1:n, function(k) {
      length(which(deg.sorted[which((1:n) < k)] >= (k - 1))) + length(which(deg.sorted[which((1:n) > k)] >= k))
    })
    gap <- deg.cor - deg.sorted
    if (!norm) {
      gap <- 0.5 * sum(gap[gap >= 0])
    } else {
      gap <- 0.5 * sum(gap[gap >= 0]) / igraph::ecount(g)
    }
  }
  return(gap)
}
