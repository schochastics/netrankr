#' @title Spectral Gap of a Graph
#' @description  The spectral gap of a graph is the absolute difference between the biggest and second biggest eigenvalue
#' of the adjacency matrix. To compare spectral gaps across networks, the fraction can be used
#'
#' @param g igraph object
#' @param method a string, either "frac" or "abs"
#' @return numeric value
#' @examples
#' #The spectral gap of a threshold graph is usually quite big
#' g=threshold_graph(10,0.3)
#' spectral_gap(g,method="fraction")
#' @export
#'
spectral_gap <- function(g,method="fraction"){
  sA <- eigen(igraph::get.adjacency(g,"both"))$values[c(1,2)]
  if(method=="fraction"){
    return(1-sA[2]/sA[1])
  }
  else if(method=="absolute"){
    return(sA[1]-sA[2])
  }

  else{
    stop("method must be one of frac or abs")
  }
}
