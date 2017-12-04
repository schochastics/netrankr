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
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' hyperbolic_index(g,type = "odd")
#' hyperbolic_index(g,type = "even")
#' @export
hyperbolic_index <- function(g, type = "odd") {
    n <- igraph::vcount(g)
    if (type == "even") {
        ENW <- rep(0, n)
        for (v in 1:n) {
            Nv <- igraph::neighborhood(g, 1, v)[[1]]
            g1 <- igraph::induced.subgraph(g, Nv)
            C <- igraph::get.adjacency(g1, type = "both")
            eig.decomp <- eigen(C, symmetric = TRUE)
            V <- (eig.decomp$vectors)^2
            lambda <- eig.decomp$values
            ENW[v] <- sum(V %*% cosh(lambda)) * igraph::graph.density(g1)  #cosh(x)
        }
    } else if (type == "odd") {
        ENW <- rep(0, n)
        for (v in 1:n) {
            Nv <- igraph::neighborhood(g, 1, v)[[1]]
            g1 <- igraph::induced.subgraph(g, Nv)
            C <- igraph::get.adjacency(g1, type = "both")
            eig.decomp <- eigen(C, symmetric = TRUE)
            V <- (eig.decomp$vectors)^2
            lambda <- eig.decomp$values
            ENW[v] <- sum(V %*% sinh(lambda)) * igraph::graph.density(g1)  #sinh(x)
        }
    } else {
        stop("type must be even or odd")
    }
    return(ENW)
}
