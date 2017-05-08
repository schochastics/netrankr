#' @title Indirect relations of a network
#' @description Derive indirect relations, for instance distances, for a given network. 
#' @param g igraph object. The network for which relations should be derived
#' @param relation string. giving the relation to be calculated. See Details for options.
#' @param walk_length integer. maximum length of walks to be considered if relation="walks"
#' @param gamma vector of weights if relation = "walks". See Details.
#' @details The \emph{relation} parameter can be set to 
#' \describe{
#' \item{identity}{returns the adjacency matrix of the network.}
#' \item{distances}{calculated geodesic distances between all pairs of nodes.}
#' \item{dependencies}{returns dyadic dependencies 
#' \deqn{\delta(u,s) = \sum_{t \in V} \frac{\sigma(s,t|u)}{\sigma(s,t)}}
#' where \eqn{\sigma(s,t|u)} is the number of shortest paths from s to t that include u and
#' \eqn{\sigma(s,t)} is the total number of shortest (s,t)-paths. The row sums of the resulting matrix 
#' is equal to the betweenness scores.
#'  }
#' \item{walks}{returns weighted walk counts of the form}
#' \deqn{\sum_{i=0}^k \gamma_i A^i.}
#' k is set by the `walk_length` parameter and \eqn{\gamma} by the vector provided by `gamma`.
#' }
#' @return a matrix containing indirect relations.
#' @examples
#' 
#' require(igraph)
#' 
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' #same as distances(g,mode="all")
#' dist <- indirect_relations(g,"distances") 
#' dep  <- indirect_relations(g,"dependencies")
#' 
#' #rowSums of dep equals 2*betweenness(g)
#' rowSums(dep)-2*betweenness(g)
#' 
#' #indirect realtion for subgraph_centrality
#' walk  <- indirect_relations(g,"walks",gamma=factorial(1:10))
#' plot(rowSums(walk),subgraph_centrality(g))
#' @export
indirect_relations <- function(g,relation="distances",
                              walk_length=10,
                              gamma=factorial(1:walk_length)){
  if(relation=="distances"){
    return(igraph::distances(g,mode = "all"))
  }
  else if(relation=="identity"){
    return(igraph::get.adjacency(g,type="both",sparse=FALSE))
  }
  else if(relation=="dependencies"){
    adj <- lapply(igraph::get.adjlist(g),function(x)x-1)
    return(dependency(adj))
  }
  else if(relation=="walks"){
    A <- igraph::get.adjacency(g)
    tmp <- list()
    B <- diag(1,igraph::vcount(g))
    for(k in 1:walk_length){
      B <- B%*%A
      tmp[[k]] <- B
    }
    for(i in 1:walk_length){
      tmp[[i]] <- tmp[[i]]*gamma[i]^(-1)
    }
    return(as.matrix(Reduce("+",tmp)))
  }
}