dependency_transform <- function(g) {
  #' @title Dyadic Dependencies between nodes
  #' @description  Calculates dyadic dependencies 
  #' \deqn{
  #' \delta(s\vert u)=\sum_t \frac{\sigma(s,t\vert u)}{\sigma(s,t)}
  #' }
  #' between pairs of nodes. The values are commonly used for betweenness centrality.
  #' @param g igraph object
  #' @details Betweenness centrality is equal to column sums!
  #'
  #' @return a matrix containing dyadic dependencies
  #' @seealso [positional_dominance]
  #' @examples
  #' ###TODO
  #' @export
  A=get.adjacency(g,"both")
  n <- nrow(A)
  C<-matrix(0,n,n)
  s.ind=0
  for (s in 1:n) {
    s.ind=s.ind+1
    S <- c()
    P <- vector("list", n)
    sig <- rep(0, n)
    sig[s] = 1
    d <- rep(-1, n)
    d[s] = 0
    Q = c(s) # this is a QUEUE
    while (length(Q) > 0) {
      v <- Q[1]
      Q <- Q[-1]
      S <- c(v, S)
      
      neighbors <- which(A[v,] > 0)
      for (w in neighbors) {
        if (d[w] < 0) {
          Q <- c(Q, w)
          d[w] <- d[v] + 1
        }
        if (d[w] == d[v] + 1) {
          sig[w] <- sig[w] + sig[v]
          P[[w]] <- c(P[[w]], v)
        }
      }
    }
    
    delta <- rep(0, n)
    while (length(S) > 0) {
      w <- S[1]
      S <- S[-1]
      for (v in P[[w]]) {
        delta[v] <- delta[v] + ((sig[v]/sig[w])*(1+delta[w]))
      }
      if (w != s) {
        C[s.ind,w] <- C[s.ind,w] + delta[w]
      }
    }
  }
  return(C)
}

distance_transform <- function(g,mode="all") {
  #' @title Distances between nodes
  #' @description  Calculates distances between pairs of nodes. The values are commonly used for closeness centrality.
  #' @param g igraph object
  #' @param mode Character constant, gives whether the distances to or from the given vertices 
  #' should be calculated for directed graphs. 
  #' If out then the shortest paths from the vertex, 
  #' if in then to it will be considered. 
  #' If all, the default, then the corresponding undirected graph will be used, ie. not directed paths are searched. 
  #' @details #TODO
  #'
  #' @return a matrix containing distances
  #' @seealso [positional_dominance]
  #' @examples
  #' ###TODO
  #' @export
  return(igraph::distances(g,mode=mode))
}
