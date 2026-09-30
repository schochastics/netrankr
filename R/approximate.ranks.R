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
    method <- match.arg(method, c("lpom", "glpom", "loof1", "loof2"))
    P <- check_partial_order(P)

    # Equivalence classes ------------------------------------------------
    reduced <- collapse_mse(P)
    P <- reduced$P
    MSE <- reduced$mse
    if (method != "lpom") {
        P <- as.matrix(P)
    }

    g <- igraph::graph_from_adjacency_matrix(P, "directed")
    n <- nrow(P)
    if (method == "lpom") {
        sx <- igraph::degree(g, mode = "in")
        ix <- (n - 1) - igraph::degree(g, mode = "all")
        r.approx <- (sx + 1) * (n + 1) / (n + 1 - ix)
        r.approx <- unname(r.approx)
    } else if (method == "glpom") {
        storage.mode(P) <- "double"
        r.approx <- approx_glpom(P)
    } else if (method == "loof1") {
        s <- igraph::degree(g, mode = "in")
        l <- igraph::degree(g, mode = "out")
        incomp <- incomparable_matrix(P)
        r.approx <- s + 1 + rowSums(loof_term(s, l, incomp))
    } else if (method == "loof2") {
        s <- igraph::degree(g, mode = "in")
        l <- igraph::degree(g, mode = "out")
        incomp <- incomparable_matrix(P)
        term <- loof_term(s, l, incomp)
        s.approx <- s + rowSums(term)
        l.approx <- l + rowSums(incomp) - rowSums(term)
        r.approx <- s + 1 + rowSums(loof_term(s.approx, l.approx, incomp))
    }
    expand_expected(r.approx, MSE)
}

# incomp[x, y] = 1 if x and y are distinct and incomparable
incomparable_matrix <- function(P) {
    incomp <- (P == 0 & t(P) == 0) + 0
    diag(incomp) <- 0
    incomp
}

# term[x, y] = (s_x + 1)(l_y + 1) / ((s_x + 1)(l_y + 1) + (s_y + 1)(l_x + 1)) for incomparable x, y
loof_term <- function(s, l, incomp) {
    num <- outer(s + 1, l + 1)
    incomp * num / (num + outer(l + 1, s + 1))
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
    P <- check_partial_order(P)

    # Equivalence classes ------------------------------------------------
    reduced <- collapse_mse(P)
    P <- as.matrix(reduced$P)
    storage.mode(P) <- "integer"
    MSE <- reduced$mse

    relative.rank <- approx_relative(colSums(P), rowSums(P), P, iterative, num.iter)
    mrp.full <- relative.rank[MSE, MSE, drop = FALSE]
    diag(mrp.full) <- 0
    return(mrp.full)
}
