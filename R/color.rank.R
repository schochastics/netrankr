color_rank=function(g,path.label,col="red"){
  #' @title Color Paths in Ideal Lattice
  #' @description  Simple Function to color rankings in the lattice of ideal
  #'
  #' @param g igraph object. Lattice of ideals
  #' @param path.label vector of vertex ids. Ranking to be colorized
  #' @param col color to be used
  #' @return igraph object with edge color attribute
  #' @examples
  #' ###TODO
  #' @export
  el=igraph::get.edgelist(g,names=F)
  v <- 1
  rank.path=rep(0,length(path.label))
  for(i in path.label){
    idx <- which(el[,1]==v)
    j <- which(igraph::get.edge.attribute(g,"label",idx)==i)
    rank.path[i] <- idx[j]
    v <- el[idx[j],2]
  }
  igraph::E(g)$color[rank.path]=ifelse(igraph::E(g)$color[rank.path]=="grey",col,"blue")
  g
}
