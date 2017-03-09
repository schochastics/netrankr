majorization_gap=function(g,norm=TRUE){
  #' @title (normalized) Majorization Gap of a Graph
  #' @description  Calculates the (normalized) majorization gap of a graph. The majorization gap indicates how far the
  #' degree sequence of a graph is from a threshold sequence. The distance is measured by the number of \emph{reverse unit
  #' transformations} necessaray to turn the degree sequence into a threshold sequence.
  #'
  #' @param g An igraph object
  #' @param norm True(Default) if the normalized majorization gap should be returned.
  #' @return majorization gap of an undirected graph.
  #' @examples
  #' require(igraph)
  #' g=graph.star(5,"undirected")
  #' majorization_gap(g) #0 since star graphs are threshold graphs
  #'
  #' g=sample_gnp(100,0.15)
  #' majorization_gap(g,norm=TRUE) #fraction of reverse unit transformation
  #' majorization_gap(g,norm=FALSE) #number of reverse unit transformation
  #' @export
  n=igraph::vcount(g)
  deg.sorted=sort(igraph::degree(g),decreasing=TRUE)
  deg.cor=sapply(1:n,function(k){length(which(deg.sorted[which((1:n)<k)]>=(k-1)))+length(which(deg.sorted[which((1:n)>k)]>=k))})
  gap=deg.cor-deg.sorted
  if(!norm){
  gap=0.5*sum(gap[gap>=0])
  }
  else{
    gap=0.5*sum(gap[gap>=0])/igraph::ecount(g)
  }
  return(gap)
}
