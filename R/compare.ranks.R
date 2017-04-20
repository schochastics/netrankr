compare_ranks=function(x,y){
  #' @title Count occurences of pairs in rankings
  #' @description  Counts the number of Concordant, Discordant, Tied and Unknown between two rankings
  #'
  #' @param x a ranking vector
  #' @param y a ranking vector with the same length as \emph{x}
  #' @return a list containing
  #' \item{c}{number of concordant pairs: \code{x[i]}>\code{x[j]} and \code{y[i]}>\code{y[j]}}
  #' \item{d}{number of discordant pairs: \code{x[i]}>\code{x[j]} and \code{y[i]}<\code{y[j]}}
  #' \item{t}{number of tied pairs:  \code{x[i]}=\code{x[j]} and \code{y[i]}=\code{y[j]}}
  #' \item{u}{number of left and right ties: \code{x[i]}=\code{x[j]} and \code{y[i]}>\code{y[j]}}
  #' @details TODO
  #' @examples
  #' require(igraph)
  #' tg=threshold_graph(100,0.2)
  #' compare_ranks(degree(tg),closeness(tg)) #only concordant pairs
  #' compare_ranks(degree(tg),betweenness(tg)) #no discordant pairs
  #' @export
  if(length(x)!=length(y)){
    stop("x and y must have the same length")
  }
  res <- checkPairs(x,y)
  return(res)
}

