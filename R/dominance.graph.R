dominance_graph=function(g){
  #' @title Neighborhood Inclusion Graph
  #' @description Displays the neighborhood inclusion preorder as a directed graph
  #'
  #' @param g igraph object
  #' @return directed igraph object with dominance relations
  #' @examples
  #' require(igraph)
  #' g <- erdos.renyi.game(100,0.1)
  #' d <- dominance_graph(g)
  #' plot(d)
  #' @export
  P=neighborhood_inclusion(g)
  d=igraph::graph_from_adjacency_matrix(P,"directed")
  return(d)
}
