#' @title aggregation functions of relations
#' @description functions to aggregate indirect relations to construct centrality
#' scores from indirect relations calculated by [indirect_relations].
#' @param tau_x matrix containing indirect relations.
#' @name aggregate_index
#' @return scores for the index defined by the indirect relation \code{tau_x} and the 
#' used aggregation function.
#' @author David Schoch
NULL

#' @rdname aggregate_index
#' @export
aggregate_sum <- function(tau_x) {rowSums(tau_x)}

#' @rdname aggregate_index
#' @export
aggregate_prod <- function(tau_x) {apply(tau_x,1,prod)}

#' @rdname aggregate_index
#' @export
aggregate_mean <- function(tau_x) {rowMeans(tau_x)}

#' @rdname aggregate_index
#' @export
aggregate_max <- function(tau_x) {apply(tau_x,1,max)}

#' @rdname aggregate_index
#' @export
aggregate_min <- function(tau_x) {apply(tau_x,1,min)}

#' @rdname aggregate_index
#' @export
aggregate_invsum <- function(tau_x) {rowSums(tau_x)^-1}

#' @rdname aggregate_index
#' @export
aggregate_self <- function(tau_x) {diag(tau_x)}