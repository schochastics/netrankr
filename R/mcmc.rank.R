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
    # evaluate the default before P is reduced to its equivalence classes
    force(rp)
    P <- check_partial_order(P)
    if (!is.numeric(rp) || length(rp) != 1 || is.na(rp) || rp < 1) {
        stop("rp must be a positive number")
    }

    if (is.null(rownames(P)) && is.null(colnames(P))) {
        name_vec <- rownames(P) <- colnames(P) <- paste0("V", seq_len(nrow(P)))
    } else {
        name_vec <- rownames(P)
    }
    reduced <- collapse_mse(P)
    P <- as.matrix(reduced$P)
    storage.mode(P) <- "integer"
    MSE <- reduced$mse

    init.rank <- as.vector(igraph::topo_sort(igraph::graph_from_adjacency_matrix(P, "directed")))
    res <- mcmc_rank_dense(P, init.rank - 1, floor(rp))
    res$expected <- res$expected + 1
    rrp.full <- res$rrp[MSE, MSE, drop = FALSE]
    expected.full <- expand_expected_relative(res$rrp, MSE)
    rownames(rrp.full) <- colnames(rrp.full) <- names(expected.full) <- name_vec
    res <- list(relative.rank = rrp.full, expected.rank = expected.full)
    class(res) <- "netrankr_mcmc"
    return(res)
}
