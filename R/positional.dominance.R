#' @title Generalized Dominance in Graphs
#' @description generalized dominance relations. 
#'
#' @param A Matrix containing attributes or relations, for instance calculated by [indirect_relations].
#' @param type A string which is either 'one-mode' (Default) if \code{A} is a regular one-mode network
#' or 'two-mode' if \code{A} is a general data matrix. 
#' @param map Logical scalar, whether rows can be sorted or not (Default). See Details.
#' @param benefit Logical scalar, whether the attributes or relations are benefit or cost variables.
#' @details Positional dominance is a generalization of neighborhood-inclusion for 
#' arbitrary network data. In the default case, it checks for all pairs \eqn{u,v} if 
#' \eqn{A_{ut} \ge A_{vt}} holds for all \eqn{t} if \code{benefit = TRUE} or 
#' \eqn{A_{ut} \le A_{vt}} holds for all \eqn{t} if \code{benefit = FALSE}.  
#' This form of dominance is referred to as *dominance under total heterogeneity*. 
#' If \code{map=TRUE}, the rows of \eqn{A} are sorted decreasingly (\code{benefit = TRUE}) 
#' or increasingly (\code{benefit = FALSE}) and then the dominance condition is checked. This second
#' form of dominance is referred to as *dominance under total homogeneity*, while the
#' first is called *dominance under total heterogeneity*.
#' 
#' @return Dominance relations as matrix object. An entry `[u,v]` is `1` if u is dominated by v.
#' @author David Schoch
#' 
#' @references Brandes, U., 2016. Network positions. *Methodological Innovations* 9,
#' 2059799116630650.
#' 
#' Schoch, D. and Brandes, U., 2016. Re-conceptualizing centrality in social networks. 
#' *European Journal of Applied Mathematics* 27(6), 971-985.
#' 
#' @seealso [neighborhood_inclusion], [indirect_relations], [exact_rank_prob]
#' @examples
#' library(igraph)
#' 
#' data("dbces11")
#' 
#' P <- neighborhood_inclusion(dbces11)
#' comparable_pairs(P)
#' 
#' # positional dominance under total heterogeneity
#' dist <- indirect_relations(dbces11,type = "dist_sp")
#' D <- positional_dominance(dist,map = FALSE,benefit = FALSE) 
#' comparable_pairs(D) 
#' 
#' # positional dominance under total homogeneity
#' D_map <- positional_dominance(dist,map = TRUE,benefit = FALSE) 
#' comparable_pairs(D_map)
#' 
#' @export

positional_dominance <- function(A, type = "one-mode", map = FALSE, benefit = TRUE) {
  if (grepl("one", type)) {
      D <- matdom(A, map, benefit)
  } else if (grepl("two", type)) {
      # should be implemented in C++
      fct <- function(x, y) all(x <= y) + 0
      vecfct <- Vectorize(fct)
      r.rows <- split(A, row(A))
      D <- outer(r.rows, r.rows, vecfct)
      diag(D) <- 0
  }
  if(!is.null(rownames(A)) ){
    rownames(D) <- rownames(A)
  }
  if(!is.null(colnames(A)) ){
    colnames(D) <- colnames(A)
  }
  return(D)
}
