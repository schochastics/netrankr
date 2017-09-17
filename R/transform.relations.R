#' @title transformation of indirect relations 
#' @description Mostly wrapper functions that can be used in conjunction 
#' with [indirect_relations] to fine tune indirect relations.
#' @param x matrix of relations.
#' @param alpha weighting factor.
#' @param k for walk counts up to a certain length.
#' @details The predefined functions follow the naming scheme `relation_transformation`.
#' Predefined functions `walks_*` are thus best used with type='walks' in
#' [indirect_relations]. Theoretically, however, any transformation can be used with any relation.
#' Yet the results might not be interpretable.
#' 
#' The following functions are implemented: 
#' 
#' `dist_2pow` returns \eqn{2^{-x}}
#' 
#' `dist_inv` returns \eqn{1/x}
#'  
#' `dist_dpow` returns \eqn{x^{-\alpha}} where \eqn{\alpha} should be chosen greater than 0.
#' 
#' `dist_powd` returns \eqn{\alpha^x} where \eqn{\alpha} should be chosen between 0 and 1.
#' 
#' `dist_triang` returns \eqn{\sum_t\frac{x_{st}}{x_{su}+x_{st}}}. This function is used for
#' a corrected betweenness version.
#' 
#' `walks_limit_prop` returns the limit proportion of walks between pairs of nodes. Calculating
#' rowSums of this relation will result in the principle eigenvector of the network.
#' 
#' `walks_exp` returns \eqn{\sum_{k=0}^\infty \frac{A^k}{k!}}
#' 
#' `walks_exp_even` returns \eqn{\sum_{k=0}^\infty \frac{A^{2k}}{(2k)!}}
#' 
#' `walks_exp_odd` returns \eqn{\sum_{k=0}^\infty \frac{A^{2k+1}}{(2k+1)!}}
#' 
#' `walks_attenuated` returns \eqn{\sum_{k=0}^\infty \alpha^k A^k}
#' 
#' `walks_uptok` returns \eqn{\sum_{j=0}^k \alpha^j A^j}
#' 
#' Walk based transformation are defined on the eigen decomposition of the 
#' adjacency matrix using the fact that
#' \deqn{f(A)=Xf(\Lambda)X^T.}
#' Care has to be taken when using user defined functions. 
#' 
#' 
#' @name transform_relations
#' @return transformed relations as matrix
#' @author David Schoch
NULL


#' @rdname transform_relations
#' @export
dist_2pow <- function(x) {
    2^(-x)
}

#' @rdname transform_relations
#' @export
dist_inv <- function(x) {
    y <- x^(-1)
    y[is.infinite(y)] <- 0
    return(y)
}

#' @rdname transform_relations
#' @export
dist_dpow <- function(x, alpha = 1) {
    x <- x^(-alpha)
    diag(x) <- 0
    return(x)
}

#' @rdname transform_relations
#' @export
dist_powd <- function(x, alpha = 0.5) {
    alpha^(x)
}

#' @rdname transform_relations
#' @export
dist_triang <- function(x) {
    n <- nrow(x)
    y <- matrix(0, n, n)
    for (s in 1:n) {
        for (t in 1:n) {
            for (u in 1:n) {
                if (s != t & u != s & u != t) {
                  y[u, s] <- y[u, s] + (x[s, t])/(x[s, u] + x[u, t])
                }
            }
        }
    }
    return(y)
}

#' @rdname transform_relations
#' @export
walks_limit_prop <- function(x) {
    c(1, rep(0, length(x) - 1))
}

#' @rdname transform_relations
#' @export
walks_exp <- function(x, alpha = 1) {
    exp(alpha * x)
}

#' @rdname transform_relations
#' @export
walks_exp_even <- function(x, alpha = 1) {
    cosh(alpha * x)
}

#' @rdname transform_relations
#' @export
walks_exp_odd <- function(x, alpha = 1) {
    sinh(alpha * x)
}

#' @rdname transform_relations
#' @export
walks_attenuated <- function(x, alpha = 1/max(x) * 0.99) {
    if (alpha > 1/max(x)) {
        warning(paste0("alpha should be smaller than ", 1/max(x), ". Results are most likely wrong otherwise."))
    }
    1/(1 - alpha * x)
}

#' @rdname transform_relations
#' @export
walks_uptok <- function(x, alpha = 1, k = 3) {
    y <- 0
    for (i in 1:k) {
        y <- y + alpha^i * x^i
    }
    return(y)
}
