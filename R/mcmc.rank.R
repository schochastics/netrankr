#' @title Estimate rank probabilities with Markov Chains
#' @description Performs a probabilistic rank analysis based on an almost uniform
#' sample of possible rankings that preserve a partial ranking.
#' @param P P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @param rp Integer indicating the number of samples to be drawn.
#' @details This function can be used instead of [exact_rank_prob]
#' if the number of elements in `P` is too large for an exact computation. As a rule of thumb,
#' the number of samples should be at least cubic in the number of elements in `P`.
#' See `vignette("benchmarks",package="netrankr")` for guidelines and benchmark results.
#' @return
#' \item{expected.rank}{Estimated expected ranks of nodes}
#' \item{relative.rank}{Matrix containing estimated relative rank probabilities:
#' \code{relative.rank[u,v]} is the probability that u is ranked lower than v.}
#'
#' @references Bubley, R. and Dyer, M., 1999. Faster random generation of linear extensions.
#' *Discrete Mathematics*, **201**(1):81-88
#'
#' @seealso [exact_rank_prob], [approx_rank_relative], [approx_rank_expected]
#' @author David Schoch
#' @examples
#' \dontrun{
#' data("florentine_m")
#' P <- neighborhood_inclusion(florentine_m)
#' res <- exact_rank_prob(P)
#' mcmc <- mcmc_rank_prob(P, rp = vcount(g)^3)
#'
#' # mean absolute error (expected ranks)
#' mean(abs(res$expected.rank - mcmc$expected.rank))
#' }
#' @export
mcmc_rank_prob <- function(P, rp = nrow(P)^3) {
    if (!inherits(P, "Matrix") && !is.matrix(P)) {
        stop("P must be a dense or spare matrix")
    }
    if (!is.binary(P)) {
        stop("P is not a binary matrix")
    }

    if (is.null(rownames(P)) && is.null(colnames(P))) {
        name_vec <- rownames(P) <- colnames(P) <- paste0("V", seq_len(nrow(P)))
    } else {
        name_vec <- rownames(P)
    }
    n.full <- nrow(P)
    MSE <- Matrix::which(P == Matrix::t(P) & P == 1, arr.ind = TRUE)
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

    init.rank <- as.vector(igraph::topo_sort(igraph::graph_from_adjacency_matrix(P, "directed")))
    if (inherits(P, "Matrix")) {
        res <- mcmc_rank_sparse(P, init.rank - 1, rp)
    } else {
        res <- mcmc_rank_dense(P, init.rank - 1, rp)
    }
    res$expected <- res$expected + 1
    expected.full <- c(0, n.full)
    rrp.full <- matrix(0, n.full, n.full)
    for (i in sort(unique(MSE))) {
        idx <- which(MSE == i)
        if (length(idx) > 1) {
            group.head <- i
            rrp.full[idx, ] <- do.call(rbind, replicate(length(idx), res$rrp[group.head, MSE], simplify = FALSE))
        } else if (length(idx) == 1) {
            rrp.full[idx, ] <- res$rrp[i, MSE]
        }
    }
    expected.full <- res$expected[MSE]
    for (val in sort(unique(expected.full), decreasing = TRUE)) {
        idx <- which(expected.full == val)
        expected.full[idx] <- expected.full[idx] + sum(duplicated(MSE[expected.full <= val]))
    }
    rownames(rrp.full) <- colnames(rrp.full) <- names(expected.full) <- name_vec
    res <- list(relative.rank = rrp.full, expected.rank = expected.full)
    class(res) <- "netrankr_mcmc"
    return(res)
}
