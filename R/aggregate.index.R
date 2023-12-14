#' @title Quantification of (indirect) relations
#' @description Function to aggregate positions defined via indirect relations to construct centrality
#' scores.
#' @param tau_x Numeric matrix containing indirect relations calculated with [indirect_relations].
#' @param type String indicating the type of aggregation to be used. See Details for options.
#' @details The predefined functions are mainly wrappers around base R functions.
#' type='sum', for instance, is equivalent to `rowSums()`. A non-base functions is
#' type='invsum' which calculates the inverse of type='sum'.
#' type='self' is mostly useful for walk based relations, e.g. to count closed walks.
#'  Other self explanatory options are type='mean', type='min', type='max' and type='prod'.
#' @return Scores for the index defined by the indirect relation `tau_x` and the
#' used aggregation type.
#' @author David Schoch
#' @seealso [indirect_relations], [transform_relations]
#' @examples
#' library(igraph)
#' library(magrittr)
#'
#' data("dbces11")
#' # degree
#' dbces11 %>%
#'     indirect_relations(type = "adjacency") %>%
#'     aggregate_positions(type = "sum")
#'
#' # closeness centrality
#' dbces11 %>%
#'     indirect_relations(type = "dist_sp") %>%
#'     aggregate_positions(type = "invsum")
#'
#' # betweenness centrality
#' dbces11 %>%
#'     indirect_relations(type = "depend_sp") %>%
#'     aggregate_positions(type = "sum")
#'
#' # eigenvector centrality
#' dbces11 %>%
#'     indirect_relations(type = "walks", FUN = walks_limit_prop) %>%
#'     aggregate_positions(type = "sum")
#'
#' # subgraph centrality
#' dbces11 %>%
#'     indirect_relations(type = "walks", FUN = walks_exp) %>%
#'     aggregate_positions(type = "self")
#' @export
aggregate_positions <- function(tau_x, type = "sum") {
    if (!inherits(tau_x, "Matrix") && !is.matrix(tau_x)) {
        stop("tau_x must be a matrix")
    }

    if (type == "sum") {
        return(rowSums(tau_x))
    } else if (type == "prod") {
        return(apply(tau_x, 1, prod))
    } else if (type == "mean") {
        return(rowMeans(tau_x))
    } else if (type == "max") {
        return(apply(tau_x, 1, max))
    } else if (type == "min") {
        return(apply(tau_x, 1, min))
    } else if (type == "invsum") {
        return(rowSums(tau_x)^-1)
    } else if (type == "self") {
        diag(tau_x)
    } else {
        stop(paste("type =", type, "is not supported. See function details for options."))
    }
}
