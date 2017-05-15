#' @title quantification of (indirect) relations
#' @description functions to aggregate indirect relations to construct centrality
#' scores from indirect relations calculated by [indirect_relations].
#' @param tau_x matrix containing indirect relations.
#' @name aggregate_index
#' @details The predefined functions are mainly wrappers around base R functions.
#' `aggregate_sum()`, for instance, is equivalent to `rowSums()`. A non-base functions is
#' `aggregate_invsum()` which calculates the inverse of `aggregate_sum()`. 
#' `aggregate_self()` is mostly usefull for walk based relations, e.g. to count closed walks.
#' @return scores for the index defined by the indirect relation \code{tau_x} and the 
#' used aggregation function.
#' @seealso [indirect_relations], [transform_relations]
#' @examples
#' require(igraph)
#' require(magrittr)
#' 
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#'
#' #closeness centrality
#' g %>% indirect_relations(relation="geodesic") %>% 
#'   aggregate_invsum()
#'   
#' #betweenness centrality
#' g %>% indirect_relations(relation="dependencies") %>% 
#'   aggregate_sum()
#'   
#' #eigenvector centrality
#' g %>% indirect_relations(relation="walks",FUN=walks_limit_prop) %>% 
#'   aggregate_sum()
#'
#'#subgraph centrality
#'g %>% indirect_relations(relation="walks",FUN=walks_exp) %>% 
#'   aggregate_self()
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