neighborhood_inclusion=function(g){
#' @title Neighborhood-Inclusion Preorder
#' @description Calculates the neighborhood-inclusion preorder of an undirected graph
#' @param g An igraph object
#' @details neighborhood-inclusion is formally defined as
#' \deqn{N(u)\subseteq N[v]}
#' where \eqn{N(u)} is the neighborhood of \eqn{u} and \eqn{N[v]=N(v)\cup \{v\}} is the closed neighborhood of \eqn{v}.
#' The importance of the neighborhood-inclusion is due to the following relation:
#' \deqn{N(u)\subseteq N[v] \implies c(u) \leq c(v)},
#' where \eqn{c} is an arbitrary centrality index. Neighborhood-inclusion is thus preserved in any centrality ranking.
#' @return the neighborhood-inclusion preorder of an undirected graph g
#' @examples
#' require(igraph)
#' g=graph.star(5,"undirected")
#' neighborhood_inclusion(g)
#' @export

  adj=lapply(igraph::get.adjlist(g),function(x) x-1)
  deg=igraph::degree(g)
  dom=nialgo(adj,deg)
  return(dom)
}
