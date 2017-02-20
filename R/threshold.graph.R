threshold_graph<-function(n,p){
  #' @title Constructing Threshold Graphs
  #' @description  A threshold graph is a graph where all centrality indices give the same ranking.
  #'
  #' @param n number of nodes
  #' @param p fraction of dominating vertices. Equates approximately to the density of the graph
  #' @return a threshold graph as igraph object
  #' @examples
  #' g <- threshold_graph(10,0.3)
  #' plot(g)
  #'
  #' ###Compare induced rankings
  #' require(igraph)
  #' degree(g)
  #' closeness(g)
  #' cor(degree(g),closeness(g),method="kendall")
  #' @references \insertRef{mp-tgrt-95}{netrankr}
  #' @export
  vschedule=rep(0,n)
  pvals=stats::runif(n)

  vschedule[pvals<=p]=1
  vschedule[n]=1
  vschedule[1]=0
  dom_vertices=which(vschedule==1)

  edgelist=matrix(0,0,2)
  for(v in dom_vertices){
    vedges=cbind(rep(v,(v-1)),seq(1,(v-1)))
    edgelist=rbind(edgelist,vedges)
  }

  g=igraph::graph.edgelist(edgelist, directed=FALSE)
  return(g)
}
