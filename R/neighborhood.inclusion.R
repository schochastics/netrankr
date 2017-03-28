neighborhood_inclusion=function(g){
#' @title Neighborhood-Inclusion Preorder
#' @description Calculates the neighborhood-inclusion preorder of an undirected graph
#' @param g An igraph object
#' @return the neighborhood-inclusion preorder of an undirected graph g
#' @examples
#' require(igraph)
#' g=graph.star(5,"undirected")
#' neighborhood_inclusion(g)
#' @export
  # n=igraph::vcount(g)
  # dom=matrix(0,n,n)
  # deg=igraph::degree(g)
  # marked=rep(0,n)
  # t=rep(0,n)
  # Nv.all=get.adjlist(g)
  # for(v in 1:n){
  #   Nv=as.vector(Nv.all[[v]])
  #   for(u in Nv){
  #     Nu=c(as.vector(Nv.all[[u]]),u)
  #     for(w in setdiff(Nu,v)){
  #       if(marked[w]!=v){
  #         marked[w]=v
  #         t[w]=0
  #       }
  #       t[w]=t[w]+1
  #       if(t[w]==deg[v]){
  #         dom[v,w]=1
  #       }
  #     }
  #   }
  # }
  adj=lapply(igraph::get.adjlist(g),function(x) x-1)
  deg=igraph::degree(g)
  dom=nialgo(adj,deg)
  return(dom)
}
