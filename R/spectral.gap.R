#' @title Spectral gap of a graph
#' @description  The spectral (or eigen) gap of a graph is the absolute 
#'  difference between the biggest and second biggest eigenvalue
#' of the adjacency matrix. To compare spectral gaps across networks, the fraction can be used.
#' 
#' @param g igraph object
#' @param method A string, either "frac" or "abs"
#' @return Numeric value
#' @details The spectral gap is bounded between 0 and 1 if `method="frac"`. The closer
#' the value to one, the bigger the gap. 
#' @author David Schoch
#' @examples
#' #The fractional spectral gap of a threshold graph is usually close to 1
#' g <- threshold_graph(50,0.3)
#' spectral_gap(g,method = "frac")
#' 
#' @export
#'
spectral_gap <- function(g, method = "frac") {
    spec_decomp <- eigen(igraph::get.adjacency(g, "both"))$values[c(1, 2)]
    if (method == "frac") {
        return(1 - spec_decomp[2]/spec_decomp[1])
    } else if (method == "abs") {
        return(spec_decomp[1] - spec_decomp[2])
    } else {
        stop("method must be one of 'frac' or 'abs'")
    }
}
