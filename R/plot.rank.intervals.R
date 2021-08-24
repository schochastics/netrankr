#' @title Plot rank intervals
#' @description This function is deprecated. Use `plot(rank_intervals(P))` instead
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @param cent.df A data frame containing centrality scores of indices (optional). See Details.
#' @param ties.method String specifying how ties are treated in the base \code{\link[base]{rank}} function.
#' @author David Schoch
#' @seealso [rank_intervals]
#' @examples
#' library(igraph)
#' data("dbces11")
#' P <- neighborhood_inclusion(dbces11)
#' \dontrun{
#' plot_rank_intervals(P)
#' }
#'
#' # adding index based rankings
#' cent_scores <- data.frame(
#'   degree = degree(dbces11),
#'   betweenness = round(betweenness(dbces11), 4),
#'   closeness = round(closeness(dbces11), 4),
#'   eigenvector = round(eigen_centrality(dbces11)$vector, 4)
#' )
#' \dontrun{
#' plot_rank_intervals(P, cent.df = cent_scores)
#' }
plot_rank_intervals <- function(P, cent.df = NULL, ties.method = "min") {
  warning("plot_rank_intervals is deprecated. use plot(rank_intervals(P)) instead")
  res <- rank_intervals(P)
  plot(res, cent_scores = cent.df, ties.method = ties.method)
}
