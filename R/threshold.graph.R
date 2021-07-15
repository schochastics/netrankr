#' @title Random threshold graphs
#' @description  Constructs a random threshold graph. 
#' A threshold graph is a graph where the neighborhood inclusion preorder is complete.
#' @param n The number of vertices in the graph.
#' @param p The probability of inserting dominating vertices. Equates approximately 
#'     to the density of the graph. See Details.
#' @param bseq (0,1)-vector a binary sequence that produces a threshold grah. See details
#' @details Either `n` and `p`, or `bseq` must be specified.
#' Threshold graphs can be constructed with a binary sequence. For each 0, an isolated 
#' vertex is inserted and for each 1, a vertex is inserted that connects to all previously inserted 
#' vertices. The probability of inserting a dominating vertices is controlled with parameter `p`.
#' If `bseq` is given instead, a threshold graph is constructed from that sequence.
#' An important property of threshold graphs is, that all centrality indices induce the same ranking.
#' @return A threshold graph as igraph object
#' @author David Schoch
#' @references Mahadev, N. and Peled, U. N. , 1995. Threshold graphs and related topics.
#' 
#' Schoch, D., Valente, T. W. and Brandes, U., 2017. Correlations among centrality
#' indices and a class of uniquely ranked graphs. *Social Networks* 50, 46–54.
#' 
#' @seealso [neighborhood_inclusion], [positional_dominance]
#' @examples
#' library(igraph)
#' g <- threshold_graph(10,0.3)
#' \dontrun{
#' plot(g)
#' 
#' # star graphs and complete graphs are threshold graphs
#' complete <- threshold_graph(10,1) #complete graph
#' plot(complete)
#' 
#' star <- threshold_graph(10,0) #star graph
#' plot(star)
#' }
#' 
#' # centrality scores are perfectly rank correlated
#' cor(degree(g),closeness(g),method = "kendall")
#' @export
threshold_graph <- function(n, p,bseq) {
  if(missing(n) & missing(bseq)){
    stop('Either specify both n and p, or bseq ')
  }
  if(missing(p) & missing(bseq)){
    stop('Either specify both n and p, or bseq ')
  }
  
  if(!missing(n) & !missing(p)){
    vschedule <- rep(0, n)
    pvals <- stats::runif(n)
    
    vschedule[pvals <= p] <- 1
    vschedule[n] <- 1
    vschedule[1] <- 0
  } else if(!missing(bseq)){
    n <- length(bseq)
    if(bseq[n]==0){
      warning("bseq[n]=0 produces unconnected graphs. using bseq[n]=1 instead")
      bseq[n] <- 1
    }
    vschedule <- bseq
    vschedule[1] <- 0
    
  }
  dom_vertices <- which(vschedule == 1)
  if (length(dom_vertices) != 1) {
      edgelist <- do.call(rbind, sapply(dom_vertices, function(v) cbind(rep(v, (v - 1)), seq(1, (v - 1)))))
      
  } else {
      edgelist <- cbind(rep(n, (n - 1)), seq(1, (n - 1)))
  }
  g <- igraph::graph_from_edgelist(edgelist, directed = FALSE)
  g$sequence <- vschedule
  return(g)
}
