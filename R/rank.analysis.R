#' @title Probabilistic centrality rankings
#' @description  Performs a complete and exact rank analysis of a given partial ranking.
#' This includes rank probabilities, relative rank probabilities and expected ranks.
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib netrankr
#'
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @param only.results Logical. return only results (default) or additionally
#'     the ideal tree and lattice if \code{FALSE}.
#' @param verbose Logical. should diagnostics be printed. Defaults to \code{FALSE}.
#' @param force Logical. If \code{FALSE} (default), stops the analysis if the partial
#'     ranking has more than 40 elements and less than 0.4 comparable pairs.
#'     Only change if you know what you are doing.
#' @details The function derives rank probabilities from a given partial ranking
#' (for instance returned by [neighborhood_inclusion] or [positional_dominance]). This includes the
#' calculation of expected ranks, (relative) rank probabilities and the number of possible rankings.
#'  Note that the set of rankings grows exponentially in the number of elements and the exact
#'  calculation becomes infeasible quite quickly and approximations need to be used.
#'  See `vignette("benchmarks")` for guidelines and [approx_rank_relative],
#'  [approx_rank_expected], and [mcmc_rank_prob] for approximative methods.
#' @return
#' \item{lin.ext}{Number of possible rankings that extend `P`.}
#' \item{mse}{Array giving the equivalence classes of `P`.}
#' \item{rank.prob}{Matrix containing rank probabilities: \code{rank.prob[u,k]} is the probability that u has rank k.}
#' \item{relative.rank}{Matrix containing relative rank probabilities: \code{relative.rank[u,v]} is the probability that u is ranked lower than v.}
#' \item{expected.rank}{Expected ranks of nodes in any centrality ranking.}
#' \item{rank.spread}{Standard deviation of the ranking probabilities.}
#' \item{topo.order}{Random ranking used to build the lattice of ideals (if \code{only.results = FALSE}).}
#' \item{tree}{Adjacency list (incoming) of the tree of ideals (if \code{only.results = FALSE}).}
#' \item{lattice}{Adjacency list (incoming) of the lattice of ideals (if \code{only.results = FALSE}).}
#' \item{ideals}{List of order ideals (if \code{only.results = FALSE}).}
#' In all cases, higher numerical ranks imply a higher position in the ranking. That is,
#' the lowest ranked node has rank 1.
#' @author David Schoch, Julian Müller
#' @references De Loof, K. 2009. Efficient computation of rank probabilities in posets.
#' *Phd thesis*, Ghent University.
#'
#' De Loof, K., De Meyer, H. and De Baets, B., 2006. Exploiting the
#' lattice of ideals representation of a poset. *Fundamenta Informaticae*, 71(2,3):309-321.
#'
#' @seealso [approx_rank_relative], [approx_rank_expected], [mcmc_rank_prob]
#' @examples
#' P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
#' P
#' res <- exact_rank_prob(P)
#'
#' # a warning is displayed if only one ranking is possible
#' tg <- threshold_graph(20, 0.2)
#' P <- neighborhood_inclusion(tg)
#' res <- exact_rank_prob(P)
#' @export
exact_rank_prob <- function(P, only.results = TRUE, verbose = FALSE, force = FALSE) {
    if (!inherits(P, "Matrix") && !is.matrix(P)) {
        stop("P must be a dense or spare matrix")
    }
    if (!is.binary(P)) {
        stop("P is not a binary matrix")
    }
    # convert to dense matrix-----------------------------------------
    # exact_rank_prob only works with small matrices. Hence, we can safely convert
    # to a dense matrix
    P <- as.matrix(P)
    # Check for names ------------------------------------------------
    if (is.null(rownames(P)) & is.null(colnames(P))) {
        rownames(P) <- colnames(P) <- paste0("V", seq_len(nrow(P)))
    }
    n_full <- nrow(P)
    P_full <- P
    # Equivalence classes ------------------------------------------------
    MSE <- which((P + t(P)) == 2, arr.ind = TRUE)
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
    # names <- rownames(P)
    # number of Elements
    nElem <- nrow(P)

    # check for linear order ---------------------------------------------
    if (comparable_pairs(P) == 1) {
        warning("P is already a ranking.\nExpected Ranks correspond to the only possible ranking.")
        expected_full <- rank(colSums(P_full), ties.method = "max")
        rank.spread_full <- rep(0, nrow(P_full))
        mrp_full <- P_full
        mrp_full[mrp_full == t(mrp_full)] <- 0
        rp_full <- matrix(0, nrow(P_full), nrow(P_full))
        for (i in seq_len(nrow(P_full))) {
            rp_full[i, expected_full[i]] <- 1
        }
        # add names
        rownames(rp_full) <- rownames(mrp_full) <- colnames(mrp_full) <- rownames(P_full)
        names(expected_full) <- names(rank.spread_full) <- rownames(P_full)
        colnames(rp_full) <- seq_len(ncol(rp_full))
        res <- list(
            lin.ext = 1,
            mse = MSE,
            rank.prob = rp_full,
            relative.rank = mrp_full,
            expected.rank = expected_full,
            rank.spread = rank.spread_full,
            topo.order = NULL,
            tree = NULL,
            lattice = NULL,
            ideals = NULL
        )
        class(res) <- "netrankr_full"
        return(res)
    }

    # sanity check if applicable ------------------------------------------------
    if (nrow(P) > 40 & comparable_pairs(P) < 0.4 & force == F) {
        stop("Input data too big. Use approximations or set `force=TRUE` if you know what you are doing")
    }
    # Prepare Data structures---------------------
    topo.order <- as.vector(igraph::topo_sort(igraph::graph_from_adjacency_matrix(P, "directed")))

    P <- P[topo.order, topo.order]
    ImPred <- igraph::as_adj_list(igraph::graph_from_adjacency_matrix(P, "directed"), "in")
    ImPred <- lapply(ImPred, function(x) as.vector(x) - 1)

    ImSucc <- igraph::as_adj_list(igraph::graph_from_adjacency_matrix(P, "directed"), "out")
    ImSucc <- lapply(ImSucc, function(x) as.vector(x) - 1)
    # TREEOFIDEALS ----------------------------------------------------
    if (verbose == TRUE) {
        print("building tree of ideals")
    }
    tree <- treeOfIdeals(ImPred)
    nIdeals <- length(tree$label)
    if (verbose == TRUE) {
        print("tree of ideals built")
    }
    Ek <- sapply(0:(nElem - 1), function(x) {
        which(tree$label == x) - 1
    })
    # tree$child=lapply(tree$child,function(x) {idx=order(tree$label[x+1],decreasing=T);x[idx]})
    if (verbose == TRUE) {
        print("building lattice of Ideals")
    }
    latofI <- LatticeOfIdeals(tree$child, tree$parent, Ek, nElem, nIdeals)

    if (verbose == TRUE) {
        print("lattice of ideals built")
    }
    ideallist <- listingIdeals(ImSucc, nElem, nIdeals)
    # ideallist=lapply(ideallist,sort)

    if (verbose == TRUE) {
        print("ideals listed")
    }

    if (verbose == TRUE) {
        print(paste("No of ideals:", nIdeals))
        print("Calculating Rank Probabilities")
    }

    res <- rankprobs(latofI, ideallist, nElem, nIdeals)
    if (verbose == TRUE) {
        print(paste("No. of possible Rankings: ", res$linext))
    }
    res$rp <- res$rp[order(topo.order), ]
    res$mrp <- res$mrp[order(topo.order), order(topo.order)]

    ############################### END
    expected <- res$rp %*% seq_len(nElem)
    rank.spread <- rowSums((matrix(rep(seq_len(nElem), each = nElem), nElem, nElem) - c(expected))^2 * res$rp)
    expected <- c(expected)

    ###############################
    # Insert equivalent nodes again ----
    rp_full <- matrix(0, n_full, ncol(res$rp))
    mrp_full <- matrix(0, n_full, n_full)
    expected_full <- c(0, n_full)
    rank.spread_full <- rep(0, n_full)
    for (i in sort(unique(MSE))) {
        idx <- which(MSE == i)
        if (length(idx) > 1) {
            group.head <- i
            rp_full[idx, ] <- do.call(rbind, replicate(length(idx), res$rp[group.head, ], simplify = FALSE))
            mrp_full[idx, ] <- do.call(rbind, replicate(length(idx), res$mrp[group.head, MSE], simplify = FALSE))
            rank.spread_full[idx] <- rank.spread[group.head]
        } else if (length(idx) == 1) {
            rp_full[idx, ] <- res$rp[i, ]
            mrp_full[idx, ] <- res$mrp[i, MSE]
            rank.spread_full[idx] <- rank.spread[i]
        }
    }
    expected_full <- expected[MSE]
    for (val in sort(unique(expected_full), decreasing = TRUE)) {
        idx <- which(expected_full == val)
        expected_full[idx] <- expected_full[idx] + sum(duplicated(MSE[expected_full <= val]))
    }
    # add names
    rownames(rp_full) <- rownames(mrp_full) <- colnames(mrp_full) <- rownames(P_full)
    names(expected_full) <- names(rank.spread_full) <- rownames(P_full)
    colnames(rp_full) <- seq_len(ncol(rp_full))

    ###############################
    if (only.results) {
        res <- list(
            lin.ext = res$linext,
            mse = MSE,
            rank.prob = rp_full,
            relative.rank = t(mrp_full),
            expected.rank = expected_full,
            rank.spread = sqrt(rank.spread_full),
            topo.order = NULL,
            tree = NULL,
            lattice = NULL,
            ideals = NULL
        )
        class(res) <- "netrankr_full"
        return(res)
    } else {
        res <- list(
            lin.ext = res$linext,
            mse = MSE,
            rank.prob = rp_full,
            relative.rank = t(mrp_full),
            expected.rank = expected_full,
            rank.spread = sqrt(rank.spread_full),
            topo.order = topo.order,
            tree = tree,
            lattice = latofI,
            ideals = ideallist
        )
        class(res) <- "netrankr_full"
        return(res)
    }
}
