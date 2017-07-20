#' @title Rank interval of nodes
#' @description Calculate the maximal and minimal rank possible for each node using the partial ranking P.
#' @param P a partial ranking as a matrix 
#' @details Note that the returned `mid_point` is not the same as the expected rank, for instance computed with [exact_rank_prob].
#' It is simply the mid point between `min_rank` and `max_rank`.
#' @return a data frame with the minimal, maximal rank of each node together with the mid point of the two extrema.
#' @author David Schoch
#' @seealso [exact_rank_prob]
#'
#' @examples
#' P <- matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
#' rank_intervals(P)
#' @export
rank_intervals <- function(P){
  # MSE=which((P+t(P))==2,arr.ind=T)
  # if(length(MSE)>=1){
  #   MSE <- t(apply(MSE,1,sort))
  #   MSE <- MSE[!duplicated(MSE),]
  #   g <- igraph::graph.empty()
  #   g <- igraph::add.vertices(g,nrow(P))
  #   g <- igraph::add.edges(g,c(t(MSE)))
  #   g <- igraph::as.undirected(g)
  #   MSE <- igraph::clusters(g)$membership
  #   equi <- which(duplicated(MSE))
  #   P <- P[-equi,-equi]
  # } else{
  #   MSE <- 1:nrow(P)
  # }
  n <- nrow(P)
  max_rank_all <- n-rowSums((P-t(P))==1)-rowSums(P==1 & t(P)==1) #CAUTION!!!!
  min_rank_all <- colSums((P-t(P))==1)+1
  mid_point_all <- (max_rank_all + min_rank_all)/2
  
  return(data.frame(min_rank=min_rank_all,max_rank=max_rank_all,mid_point=mid_point_all))
}
