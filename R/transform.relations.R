#' @title transformation of indirect relations 
#' @description Mostly wrapper functions to transform indirect relations.
#' @param x matrix of transformed relations.
#' @param alpha weighting factor.
#' @param k for walk counts up to a certain length.
#' @description This functions can be used in conjunction with [indirect_relations].
#' @name transform_relations
#' @return transformed relations as matrix
#' @author David Schoch
NULL

#' @rdname transform_relations
#' @export
walks_limit_prop <- function(x){c(1,rep(0,length(x)-1))}

#' @rdname transform_relations
#' @export
walks_even_exp <- function(x,alpha=1) {cosh(alpha*x)}

#' @rdname transform_relations
#' @export
walks_odd_exp <- function(x,alpha=1) {sinh(alpha*x)}

#' @rdname transform_relations
#' @export
walks_exp <- function(x,alpha=1) {exp(alpha*x)}

#' @rdname transform_relations
#' @export
walks_attenuated <- function(x,alpha=1/max(x)*0.99) {1/(1-alpha*x)}

#' @rdname transform_relations
#' @export
walks_uptok <- function(x,alpha=1,k=3) {
  y <- 0
  for(i in 1:k){
    y <- y+alpha^i*x^i
  }
  return(y)
}

#' @rdname transform_relations
#' @export
dist_2pow <- function(x) {2^(-x)}

#' @rdname transform_relations
#' @export
dist_inv <- function(x) {
  y<-x^(-1)
  y[is.infinite(y)] <- 0
}

#' @rdname transform_relations
#' @export
dist_dpow <- function(x,alpha=1){x^(-alpha)}

#' @rdname transform_relations
#' @export
dist_powd <- function(x,alpha=0.5){alpha^(-x)}
