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
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#'#degree
#' g %>% indirect_relations(type='identity') %>% 
#'  aggregate_positions(type='sum')
#'
#' #closeness centrality
#' g %>% indirect_relations(type='geodesic') %>% 
#'   aggregate_positions(type='invsum')
#'   
#' #betweenness centrality
#' g %>% indirect_relations(type='dependencies') %>% 
#'   aggregate_positions(type='sum')
#'   
#' #eigenvector centrality
#' g %>% indirect_relations(type='walks',FUN=walks_limit_prop) %>% 
#'   aggregate_positions(type='sum')
#'
#'#subgraph centrality
#' g %>% indirect_relations(type='walks',FUN=walks_exp) %>% 
#'   aggregate_positions(type='self')
#'   
#' @export
aggregate_positions <- function(tau_x, type = "sum") {
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
        stop(paste(type, " not supported. See function details for options."))
    }
}
