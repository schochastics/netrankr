#' @title Neighborhood-inclusion preorder
#' @description Calculates the neighborhood-inclusion preorder of an undirected graph.
#' @param g An igraph object
#' @details Neighborhood-inclusion is defined as
#' \deqn{N(u)\subseteq N[v]}
#' where \eqn{N(u)} is the neighborhood of \eqn{u} and \eqn{N[v]=N(v)\cup \lbrace v\rbrace} is the closed neighborhood of \eqn{v}.
#' \eqn{N(u) \subseteq N[v]} implies that \eqn{c(u) \leq c(v)},
#' where \eqn{c} is a centrality index based on a specific path algebra. Indices 
#' falling into this category are closeness (and variants), betweenness 
#' (and variants) as well as many walk-based indices (eigenvector and subgraph 
#' centrality, total communicability,...). 
#' @return The neighborhood-inclusion preorder of \code{g} as matrix object. \code{P[u,v]=1} if \eqn{N(u)\subseteq N[v]}
#' @author David Schoch
#' @references Schoch, D. and Brandes, U., 2016. Re-conceptualizing centrality in social networks. 
#' *European Journal of Applied Mathematics* 27(6), 971-985.
#' 
#' Brandes, U. Heine, M., Müller, J. and Ortmann, M., 2017.
#' Positional Dominance: Concepts and Algorithms. 
#' *Conference on Algorithms and Discrete Applied Mathematics*, 60-71.
#' 
#' 
#' 
#' 
#' @seealso [positional_dominance], [exact_rank_prob]
#' @examples
#' library(igraph)
#' #the neighborhood inclusion preorder of a star graph is complete
#' g <- graph.star(5,'undirected')
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#' 
#' #the same holds for threshold graphs
#' tg <- threshold_graph(50,0.1)
#' P <- neighborhood_inclusion(tg)
#' comparable_pairs(P)
#' 
#' #standard centrality indices preserve neighborhood-inclusion
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' P <- neighborhood_inclusion(g)
#' 
#' is_preserved(P,degree(g))
#' is_preserved(P,closeness(g))
#' is_preserved(P,betweenness(g))
#' @export
neighborhood_inclusion <- function(g) {
    adj <- lapply(igraph::get.adjlist(g), function(x) x - 1)
    deg <- igraph::degree(g)
    dom <- nialgo(adj, deg)
    if(!is.null(igraph::V(g)$name) ){
      rownames(dom) <- colnames(dom) <- V(g)$name
    }
    return(dom)
}
