#' netrankr: a package for generic centrality analyses. 
#' 
#' netrankr provides several functions to centrality related analyses in networks. 
#' The focus lies on methods that do not relay on traditional indices like degree, 
#' betweenness or closeness.
#'
#' Some notable features of the package are: 
#'    
#' \itemize{
#' \item Working with the neighborhood inclusion preorder. This forms the bases 
#' for any centrality analysis on undirected and unweighted graphs. 
#' More details can be found in the dedicated vignette: 
#' `vignette("neighborhood_inclusion",package="netrankr")`
#' \item Constructing graphs with a unique centrality ranking. 
#' This class of graphs, known as threshold graphs, can be used to benchmark 
#' centrality indices, since they only allow for one ranking of the nodes.   
#' For more details consult the vignette: `vignette("threshold_graph",package="netrankr")`
#' \item Probabilistic ranking methods. The package includes several function to 
#' calculate rank probabilities of nodes in a network. These include expected (centrality)
#' ranks and relative rank probabilities (how likely is it that a node is more central than another)
#' An extensive example is given in the corresponding vignette
#' }
#' 
#' To learn more about netrankr, start with the vignettes:
#' `browseVignettes(package = "netrankr")`
#' 
#' @docType package
#' @name netrankr
NULL