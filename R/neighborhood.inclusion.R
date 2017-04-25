neighborhood_inclusion=function(g){
#' @title Neighborhood-Inclusion Preorder
#' @description Calculates the neighborhood-inclusion preorder of an undirected graph.
#' @param g An igraph object
#' @details neighborhood-inclusion is formally defined as
#' \deqn{N(u)\subseteq N[v]}
#' where \eqn{N(u)} is the neighborhood of \eqn{u} and \eqn{N[v]=N(v)\cup \lbrace v\rbrace} is the closed neighborhood of \eqn{v}.
#' \eqn{N(u) \subseteq N[v]} implies that \eqn{c(u) \leq c(v)},
#' where \eqn{c} is an arbitrary centrality index. Neighborhood-inclusion is thus preserved in any centrality ranking.
#' @return the neighborhood-inclusion preorder of an undirected graph g as matrix object
#' @seealso [positional_dominance],[check_preservation]
#' @examples
#' require(igraph)
#' ###the neighborhood inclusion preorder of a star graph is complete
#' g=graph.star(5,"undirected")
#' P <- neighborhood_inclusion(g)
#' comparable_pairs(P)
#' 
#' ###standard centrality indices preserve the partial ranking of neighborhood inclusion
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' P<-neighborhood_inclusion(g)
#' check_preservation(P,degree(g))
#' check_preservation(P,closeness(g))
#' check_preservation(P,betweenness(g))
#' @export

  adj=lapply(igraph::get.adjlist(g),function(x) x-1)
  deg=igraph::degree(g)
  dom=nialgo(adj,deg)
  return(dom)
}
