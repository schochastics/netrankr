#' @title Count occurrences of pairs in rankings
#' @description  Counts the number of concordant, discordant and (left/right) ties between two rankings.
#'
#' @param x A numeric vector.
#' @param y A numeric vector with the same length as \code{x}.
#' @return A list containing
#' \item{concordant}{number of concordant pairs: \code{x[i]} > \code{x[j]} and \code{y[i]} > \code{y[j]}}
#' \item{discordant}{number of discordant pairs: \code{x[i]} > \code{x[j]} and \code{y[i]} < \code{y[j]}}
#' \item{ties}{number of tied pairs:  \code{x[i]} == \code{x[j]} and \code{y[i]} == \code{y[j]}}
#' \item{left}{number of left ties: \code{x[i]} == \code{x[j]} and \code{y[i]} != \code{y[j]}}
#' \item{right}{number of right ties: \code{x[i]} != \code{x[j]} and \code{y[i]} == \code{y[j]}}
#' @details Explicitly calculating the number of occurring cases is more robust
#' than using correlation indices as given in the \code{cor} function. Especially
#' left and right ties can significantly alter correlations.
#' @author David Schoch
#' @examples
#' library(igraph)
#' tg <- threshold_graph(100, 0.2)
#' compare_ranks(degree(tg), closeness(tg)) # only concordant pairs
#' compare_ranks(degree(tg), betweenness(tg)) # no discordant pairs
#' ## Rank Correlation
#' cor(degree(tg), closeness(tg), method = "kendall") # 1
#' cor(degree(tg), betweenness(tg), method = "kendall") # not 1, although no discordant pairs
#' @export
compare_ranks <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  res <- checkPairs(x, y)
  return(res)
}
