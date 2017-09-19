#' netrankr: An R package to analyze partial rankings in networks
#' 
#' @description netrankr provides several functions to analyze partial rankings for network
#' centrality. The main focus lies on methods that do not rely on indices like degree, 
#' betweenness or closeness.   
#'   
#' The package follows the philosophy, that centrality 
#' can be decomposed in a series of micro steps. Starting from a network, 
#' [indirect_relations] can be derived which can either be aggregated into an index with
#' [aggregate_positions], or alternatively turned into a partial ranking with [positional_dominance].
#' The partial ranking can then be further analyzed with [exact_rank_prob], to obtain
#' probabilistic centrality rankings.
#'
#' @details Some features of the package are: 
#'    
#' \itemize{
#' \item Working with the neighborhood inclusion preorder. This forms the bases 
#' for any centrality analysis on undirected and unweighted graphs. 
#' More details can be found in the dedicated vignette: 
#' `vignette("neighborhood_inclusion",package = "netrankr")`
#' \item Constructing graphs with a unique centrality ranking. 
#' This class of graphs, known as threshold graphs, can be used to benchmark 
#' centrality indices, since they only allow for one ranking of the nodes.   
#' For more details consult the vignette: `vignette("threshold_graph",package = "netrankr")`
#' \item Probabilistic centrality. Why apply a handful of indices and choosing
#' the one that fits best, when it is possible to analyze **all** centrality rankings at once?
#' The package includes several function to calculate rank probabilities of nodes 
#' in a network. These include expected ranks and relative rank probabilities 
#' (how likely is it that a node is more central than another?)
#' Consult `vignette("probabilistic_cent",package = "netrankr")` for more info.
#' }
#' 
#' The package provides several additional vignettes that explain the functionality 
#' of netrankr and its conceptual ideas. See `browseVignettes(package = 'netrankr')`
#' or the [online manual](https://schochastics.github.io/netrankr).
#' 
#' @docType package
#' @name netrankr
NULL
