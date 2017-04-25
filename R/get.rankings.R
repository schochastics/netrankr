get_rankings=function(lattice,ideals,topo.order,linext,mse,force=F){
  #' @title All rankings of a partial ranking
  #' @description Returns all possible rankings of a partial ranking using the lattice of ideals.
  #'
  #' @param lattice adjacency list of predecessors in lattice of ideal. This list is returned if [rank_analysis] 
  #' is run with `only.results=FALSE`
  #' @param ideals list of ideals. This list is returned if [rank_analysis] 
  #' is run with `only.results=FALSE`
  #' @param topo.order topological order used in [rank_analysis]
  #' @param linext number of possible rankings. returned by rank_analysis
  #' @param mse equivalence classes of rankings. returned by [rank_analysis]
  #' @param force boolean. stops function if the number of rankings is too large.
  #' Only change to TRUE if you know what you are doing
  #' @return a matrix containing all ranking positions of nodes
  #' @examples
  #' P=matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
  #' P
  #' res <- rank_analysis(P,only.results=FALSE)
  #' get_rankings(res$lattice,res$ideals,res$topo.order,res$lin.ext,res$mse)
  #' ###TODO
  #' @export
  if(linext>10000 & !force){
    stop("number of linear extensions to high. Use force=F if you know what you are doing.")
  }
  n <- length(unique(mse))
  lattice <- lapply(lattice,function(x)x+1)
  g <- igraph::graph_from_adj_list(lattice,mode="in")
  paths <- igraph::all_shortest_paths(g,from=n+1,to=1)
  
  paths <- lapply(paths$res,function(x) as.vector(x)-1)
  rks <- rankings(paths, ideals, linext, n)
  rks <- rks+1
  rks <- rks[order(topo.order),]
  rks <- rks[mse,]
  return(rks)
}