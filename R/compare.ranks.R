#' @title Count occurences of pairs in rankings
#' @description  Counts the number of Concordant, Discordant and (Left/Right) Ties between two rankings.
#'
#' @param x a ranking vector.
#' @param y a ranking vector with the same length as \code{x}.
#' @return a list containing
#' \item{concordant}{number of concordant pairs: \code{x[i]}>\code{x[j]} and \code{y[i]}>\code{y[j]}}
#' \item{discordant}{number of discordant pairs: \code{x[i]}>\code{x[j]} and \code{y[i]}<\code{y[j]}}
#' \item{ties}{number of tied pairs:  \code{x[i]}=\code{x[j]} and \code{y[i]}=\code{y[j]}}
#' \item{left}{number of left: \code{x[i]}=\code{x[j]} and \code{y[i]}!=\code{y[j]}}
#' \item{right}{number of right: \code{x[i]}!=\code{x[j]} and \code{y[i]}=\code{y[j]}}
#' @details Explicitly calculating the number of occuring cases is more robust than using correlation 
#' indices as given in the \code{cor} function. Especially left and right ties can significantly alter an
#' intuitively high correlation (See example).
#' @author David Schoch
#' @examples
#' require(igraph)
#' tg <- threshold_graph(100,0.2)
#' compare_ranks(degree(tg),closeness(tg)) #only concordant pairs
#' compare_ranks(degree(tg),betweenness(tg)) #no discordant pairs
#' ## Rank Correlation
#' cor(degree(tg),closeness(tg)) #1
#' cor(degree(tg),betweenness(tg)) #not 1, although no discordant pairs
#' @export
compare_ranks <- function(x,y){
  if(length(x)!=length(y)){
    stop("x and y must have the same length")
  }
  res <- checkPairs(x,y)
  return(res)
}

