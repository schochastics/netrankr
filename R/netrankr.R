#' netrankr: Analyzing Partial Rankings for Network Centrality
#' 
#' netrankr provides several functions to analyze partial rankings for network
#' centrality. The main focus lies on methods that do not rely on indices like degree, 
#' betweenness or closeness. The package can, however, also be used to analyse
#' partial rankings that arise from a non-network context
#'
#' Some features of the package are: 
#'    
#' \itemize{
#' \item Working with the neighborhood inclusion preorder. This forms the bases 
#' for any centrality analysis on undirected and unweighted graphs. 
#' More details can be found in the dedicated vignette: 
#' `vignette('neighborhood_inclusion',package='netrankr')`
#' \item Constructing graphs with a unique centrality ranking. 
#' This class of graphs, known as threshold graphs, can be used to benchmark 
#' centrality indices, since they only allow for one ranking of the nodes.   
#' For more details consult the vignette: `vignette('threshold_graph',package='netrankr')`
#' \item Probabilistic centrality. Why applying a handful of indices and choosing
#' the one that fits best, when it is possible to analyze **all** centrality rankings at once?
#' The package includes several function to 
#' calculate rank probabilities of nodes in a network. These include expected ranks and 
#' relative rank probabilities (how likely is it that a node is more central than another?)
#' Consult `vignette('probabilistic_cent',package='netrankr')` for more info.
#' }
#' 
#' The package provides many additional vignettes that explain the functionality 
#' of netrankr and its conceptual ideas. See `browseVignettes(package = 'netrankr')`
#' 
#' @docType package
#' @name netrankr
NULL
