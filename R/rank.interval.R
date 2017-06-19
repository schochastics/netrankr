#' @title Rank interval of nodes
#' @description Calculate the maximal and minmal rank possible for each node using the partial ranking P.
#' @param P a partial ranking as a matrix 
#' @details Note that the returned `mid_point` is not the same as the expected rank, for instance computed with [rank_analysis].
#' It is simply the mid point between `min_rank` and `max_rank`.
#' @return a data frame with the minimal, maximal rank of each node together with the mid point of the two extrema.
#' @author David Schoch
#' @seealso [rank_analysis]
#'
#' @examples
#' P <- matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
#' rank_intervals(P)
#' @export
rank_intervals <- function(P){
  n <- nrow(P)
  max_rank <- n-rowSums(P) 
  min_rank <- colSums(P)+1
  mid_point <- (min_rank+max_rank)/2
  return(data.frame(min_rank,max_rank,mid_point))
}