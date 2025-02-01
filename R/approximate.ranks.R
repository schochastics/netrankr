#' @title Approximation of expected ranks
#' @description  Implements a variety of functions to approximate expected ranks
#' for partial rankings.
#'
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @param method String indicating which method to be used. see Details.
#' @details The \emph{method} parameter can be set to
#' \describe{
#' \item{lpom}{local partial order model}
#' \item{glpom}{extension of the local partial order model.}
#' \item{loof1}{based on a connection with relative rank probabilities.}
#' \item{loof2}{extension of the previous method.}
#' }
#' Which of the above methods performs best depends on the structure and size of the partial
#' ranking. See `vignette("benchmarks",package="netrankr")` for more details.
#' @return A vector containing approximated expected ranks.
#' @author David Schoch
#' @references Brüggemann R., Simon, U., and Mey,S, 2005. Estimation of averaged
#' ranks by extended local partial order models. *MATCH Commun. Math.
#' Comput. Chem.*, 54:489-518.
#'
#' Brüggemann, R. and Carlsen, L., 2011. An improved estimation of averaged ranks
#' of partial orders. *MATCH Commun. Math. Comput. Chem.*,
#' 65(2):383-414.
#'
#' De Loof, L., De Baets, B., and De Meyer, H., 2011. Approximation of Average
#' Ranks in Posets. *MATCH Commun. Math. Comput. Chem.*, 66:219-229.
#'
#' @seealso [approx_rank_relative], [exact_rank_prob], [mcmc_rank_prob]
#' @examples
#' P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
#' # Exact result
#' exact_rank_prob(P)$expected.rank
#'
#' approx_rank_expected(P, method = "lpom")
#' approx_rank_expected(P, method = "glpom")
#' @export
approx_rank_expected <- function(P, method = "lpom") {
    if (!inherits(P, "Matrix") && !is.matrix(P)) {
        stop("P must be a dense or spare matrix")
    }
    if (!is.binary(P)) {
        stop("P is not a binary matrix")
    }

    # Equivalence classes ------------------------------------------------
    MSE <- Matrix::which((P + Matrix::t(P)) == 2, arr.ind = TRUE)
    if (length(MSE) >= 1) {
        MSE <- t(apply(MSE, 1, sort))
        MSE <- MSE[!duplicated(MSE), ]
        g <- igraph::make_empty_graph()
        g <- igraph::add_vertices(g, nrow(P))
        g <- igraph::add_edges(g, c(t(MSE)))
        g <- igraph::as_undirected(g)
        MSE <- igraph::components(g)$membership
        equi <- which(duplicated(MSE))
        P <- P[-equi, -equi]
    } else {
        MSE <- seq_len(nrow(P))
    }
    if (length(unique(MSE)) == 1) {
        stop("all elements are structurally equivalent and have the same rank")
    }

    # number of Elements
    n <- length(names)

    g <- igraph::graph_from_adjacency_matrix(P, "directed")
    n <- nrow(P)
    if (method == "lpom") {
        sx <- igraph::degree(g, mode = "in")
        ix <- (n - 1) - igraph::degree(g, mode = "all")
        r.approx <- (sx + 1) * (n + 1) / (n + 1 - ix)
        r.approx <- unname(r.approx)
    } else if (method == "glpom") {
        r.approx <- approx_glpom(P)
    } else if (method == "loof1") {
        P <- P + diag(1, n)
        s <- igraph::degree(g, mode = "in")
        l <- igraph::degree(g, mode = "out")
        r.approx <- s + 1
        for (x in 1:n) {
            Ix <- which(P[x, ] == 0 & P[, x] == 0)
            for (y in Ix) {
                approx.rank <- ((s[x] + 1) * (l[y] + 1))
                approx.num.ranks <- ((s[x] + 1) * (l[y] + 1) + (s[y] + 1) * (l[x] + 1))
                r.approx[x] <- r.approx[x] + approx.rank / approx.num.ranks
            }
        }
    } else if (method == "loof2") {
        P <- P + diag(1, n)
        s <- igraph::degree(g, mode = "in")
        l <- igraph::degree(g, mode = "out")
        s.approx <- s
        l.approx <- l
        for (x in 1:n) {
            Ix <- which(P[x, ] == 0 & P[, x] == 0)
            for (y in Ix) {
                s.approx[x] <- s.approx[x] + .sl.approx(s[x], s[y], l[x], l[y])
                l.approx[x] <- l.approx[x] + .sl.approx(s[y], s[x], l[y], l[x])
            }
        }
        r.approx <- s + 1
        s <- s.approx
        l <- l.approx
        for (x in 1:n) {
            Ix <- which(P[x, ] == 0 & P[, x] == 0)
            for (y in Ix) {
                approx.rank <- ((s[x] + 1) * (l[y] + 1))
                approx.num.ranks <- ((s[x] + 1) * (l[y] + 1) + (s[y] + 1) * (l[x] + 1))
                r.approx[x] <- r.approx[x] + approx.rank / approx.num.ranks
            }
        }
    }
    expected.full <- unname(r.approx[MSE])
    for (val in sort(unique(expected.full), decreasing = TRUE)) {
        idx <- which(expected.full == val)
        expected.full[idx] <- expected.full[idx] +
            sum(duplicated(MSE[expected.full <= val]))
    }
    return(expected.full)
}

.sl.approx <- function(sx, sy, lx, ly) {
    ((sx + 1) * (ly + 1)) / ((sx + 1) * (ly + 1) + (sy + 1) * (lx + 1))
}
#############################
#' @title Approximation of relative rank probabilities
#' @description Approximate relative rank probabilities \eqn{P(rk(u)<rk(v))}.
#' In a network context, \eqn{P(rk(u)<rk(v))} is the probability that u is
#' less central than v, given the partial ranking P.
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @param iterative Logical scalar if iterative approximation should be used.
#' @param num.iter Number of iterations to be used. defaults to 10 (see Details).
#' @details The iterative approach generally gives better approximations
#' than the non iterative, if only slightly. The default number of iterations
#' is based on the observation, that the approximation does not improve
#' significantly beyond this value. This observation, however, is based on
#' very small networks such that increasing it for large network may yield
#' better results. See `vignette("benchmarks",package="netrankr")` for more details.
#' @author David Schoch
#' @references De Loof, K. and De Baets, B and De Meyer, H., 2008. Properties of mutual
#' rank probabilities in partially ordered sets. In *Multicriteria Ordering and
#' Ranking: Partial Orders, Ambiguities and Applied Issues*, 145-165.
#'
#' @return a matrix containing approximation of relative rank probabilities.
#' \code{relative.rank[i,j]} is the probability that i is ranked lower than j
#' @seealso [approx_rank_expected], [exact_rank_prob], [mcmc_rank_prob]
#' @examples
#' P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
#' P
#' approx_rank_relative(P, iterative = FALSE)
#' approx_rank_relative(P, iterative = TRUE)
#' @export
approx_rank_relative <- function(P, iterative = TRUE, num.iter = 10) {
    if (!inherits(P, "Matrix") && !is.matrix(P)) {
        stop("P must be a dense or spare matrix")
    }
    if (!is.binary(P)) {
        stop("P is not a binary matrix")
    }

    # Equivalence classes ------------------------------------------------
    MSE <- Matrix::which((P + Matrix::t(P)) == 2, arr.ind = T)

    if (length(MSE) >= 1) {
        MSE <- t(apply(MSE, 1, sort))
        MSE <- MSE[!duplicated(MSE), ]
        g <- igraph::make_empty_graph()
        g <- igraph::add_vertices(g, nrow(P))
        g <- igraph::add_edges(g, c(t(MSE)))
        g <- igraph::as_undirected(g)
        MSE <- igraph::components(g)$membership
        equi <- which(duplicated(MSE))
        P <- P[-equi, -equi]
    } else {
        MSE <- seq_len(nrow(P))
    }

    if (length(unique(MSE)) == 1) {
        stop("all elements are structurally equivalent and have the same rank")
    }

    relative.rank <- approx_relative(colSums(P), rowSums(P), P, iterative, num.iter)
    mrp.full <- matrix(0, length(MSE), length(MSE))
    for (i in sort(unique(MSE))) {
        idx <- which(MSE == i)
        if (length(idx) > 1) {
            group.head <- i
            mrp.full[idx, ] <- do.call(rbind, replicate(length(idx), relative.rank[group.head, MSE], simplify = FALSE))
        } else if (length(idx) == 1) {
            group.head <- idx
            mrp.full[group.head, ] <- relative.rank[i, MSE]
        }
    }

    diag(mrp.full) <- 0
    return(mrp.full)
}
