#' @title Constructing Threshold Graphs
#' @description  A threshold graph is a graph where the neighborhood inclusion preorder is complete.
#' @param n number of nodes
#' @param p fraction of dominating vertices. Equates approximately to the density of the graph
#' @details Threshold graphs can be constructed with a binary sequence. For each 0, an isolated 
#' vertex is inserted and for each 1, a vertex is inserted that contects to all previously inserted 
#' vertices. An important property of threshold graphs is, that all centrality indices induce the same ranking.
#' @return a threshold graph as igraph object
#' @author David Schoch
#' @seealso [neighborhood_inclusion],[positional_dominance]
#' @examples
#' require(igraph)
#' g <- threshold_graph(10,0.3)
#' plot(g)
#' ### star graphs and complete graphs are threshold graphs
#' complete <- threshold_graph(10,1) #complete graph
#' plot(complete)
#' star <- threshold_graph(10,0) #star graph
#' plot(star)
#' ###Compare induced rankings
#' cor(degree(g),closeness(g),method="kendall")
#' @export
threshold_graph<-function(n,p){
  vschedule <- rep(0,n)
  pvals <- stats::runif(n)
  
  vschedule[pvals<=p] <- 1
  vschedule[n] <- 1
  vschedule[1] <- 0
  dom_vertices <- which(vschedule==1)
  if(length(dom_vertices)!=1){
    edgelist <- do.call(rbind,sapply(dom_vertices,function(v)cbind(rep(v,(v-1)),seq(1,(v-1)))))
    
  } else{
    edgelist <- cbind(rep(n,(n-1)),seq(1,(n-1)))
  }
  g <- igraph::graph_from_edgelist(edgelist, directed=FALSE)
  
  return(g)
}
