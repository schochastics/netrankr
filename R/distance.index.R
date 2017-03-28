distance_index=function(g,type="ros",alpha=1){
#' @title Distance-based Centrality Indices
#' @description Indices that are based on distances
#' @param g An igraph object
#' @param type character specifying the used index. See details for possibilities
#' @param alpha numeric value for parametrized indices. See details
#' @return Numeric vector with scores of the specified index
#' @details The \emph{type} parameter can be set to 
#' \describe{
#' \item{ros}{"reciprocal of sum". \deqn{c(u)=\frac{1}{\sum_{t \in V} dist(i,t)}}}
#' \item{sor}{"sum of reciprocals". \deqn{c(u)=\sum_{t \in V} \frac{1}{dist(i,t)}}}
#' \item{dpow}{"". \deqn{c(u)=\sum_{t \in V} dist(i,t)^{-\alpha} \quad \alpha\geq 0}}
#' \item{powd}{"". \deqn{c(u)=\sum_{t \in V} \alpha^{-dist(i,t)} \quad \alpha \in (0,1)}}
#' \item{int}{"". \deqn{c(u)=\sum_{t \in V} 1-\frac{dist(i,t)-1}{diameter(G)}}}
#' \item{pow2}{"". \deqn{c(u)=\sum_{t \in V} 2^{-dist(i,t)}}}
#' }
#' @examples
#' require(igraph)
#' g=sample_gnp(100,0.1)
#' distance_index(g,type="ros")
#' distance_index(g,type="sor")
#' @export
  D=igraph::distances(g)
  diam=igraph::diameter(g)
  switch(type,
         ros={1/rowSums(D)},
         sor={R=1/D
         diag(R)=0
         rowSums(R)},
         dpow={R=(D^-alpha)
         diag(R)=0
         rowSums(R)},
         powd={rowSums(alpha^D)},
         int={rowSums(1-(D-1)/diam)},
         pow2={rowSums(2^-D)}
  )         
}