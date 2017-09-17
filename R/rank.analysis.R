#' @title Probabilistic centrality 
#' @description  Performs a complete and exact rank analysis of a given partial ranking.
#' This includes rank probabilities, relative rank probabilities and expected ranks.
#' 
#' @importFrom Rcpp evalCpp
#' @useDynLib netrankr
#' 
#' @param P matrix representing a partial ranking.
#' @param names optional argument for names if P does not have row/column names.
#' @param only.results logical. return only results (default) or additionally the ideal tree and lattice if FALSE.
#' @param verbose logical. should diagnostics be printed. Defaults to \code{FALSE}.
#' @param force logical. If FALSE(default), stops the analysis if the network has more than 50 nodes and less than 0.2 comparable pairs. Only change if you know what you are doing. 
#' @details The function derives rank probabilities from a given partial ranking 
#' (for instance returned by [neighborhood_inclusion] or [positional_dominance]). This includes the
#' calculation of expected ranks, (relative) rank probabilities and the number of possible rankings.
#' @return 
#' \item{lin.ext}{Number of possible rankings.}
#' \item{mse}{Array giving the equivalence classes of P.}
#' \item{rank.prob}{Matrix containing rank probabilities: \code{rank.prob[u,k]} is the probability that u has rank k.}
#' \item{relative.rank}{Matrix containing relative rank probabilities: \code{relative.rank[u,v]} is the probability that u is ranked lower than v.}
#' \item{expected.rank}{Expected ranks of nodes in any centrality ranking.}
#' \item{rank.spread}{Variance of the ranking probabilities.}
#' \item{topo.order}{Random ranking used to build the lattice of ideals (if \code{only.results=FALSE}).}
#' \item{tree}{igraph object. The tree of ideals (if \code{only.results=FALSE}).}
#' \item{lattice}{igraph object. The lattice of ideals (if \code{only.results=FALSE}).}
#' \item{ideals}{list. order ideals (if \code{only.results=FALSE}).}
#' In all cases, higher numerical ranks imply a higher position in the ranking. That is,
#' the lowest ranked node has rank 1.
#' @author David Schoch, Julian Müller  
#' @references De Loof, K., De Meyer, H. and De Baets, B., 2006. Exploiting the
#'lattice of ideals representation of a poset. *Fundamenta Informaticae*, 71(2,3):309-321.
#'
#' @seealso [approx_rank_relative], [approx_rank_expected], [mcmc_rank_prob]
#' @examples
#' P <- matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
#' P
#' res <- exact_rank_prob(P)
#' 
#' #a warning is displayed if only one ranking is possible
#' tg <- threshold_graph(20,0.2)
#' P <- neighborhood_inclusion(tg)
#' res <- exact_rank_prob(P)
#' @export
exact_rank_prob <- function(P, names = "", only.results = T, verbose = F, force = F) {
    # Check for names ------------------------------------------------
    if (is.null(rownames(P)) & length(names) != nrow(P)) {
        rownames(P) <- 1:nrow(P)
    } else if (is.null(rownames(P)) & length(names) == nrow(P)) {
        rownames(P) <- names
    }
    n_full <- nrow(P)
    P_full <- P
    # Equivalence classes ------------------------------------------------
    MSE <- which((P + t(P)) == 2, arr.ind = T)
    if (length(MSE) >= 1) {
        MSE <- t(apply(MSE, 1, sort))
        MSE <- MSE[!duplicated(MSE), ]
        g <- igraph::graph.empty()
        g <- igraph::add.vertices(g, nrow(P))
        g <- igraph::add.edges(g, c(t(MSE)))
        g <- igraph::as.undirected(g)
        MSE <- igraph::clusters(g)$membership
        equi <- which(duplicated(MSE))
        P <- P[-equi, -equi]
    } else {
        MSE <- 1:nrow(P)
    }
    if (is.null(nrow(P))) {
        warning("all elements are structurally equivalent and have the same rank")
        return()
    }
    names <- rownames(P)
    # number of Elements
    nElem <- length(names)
    
    # check for linear order ---------------------------------------------
    if (comparable_pairs(P) == 1) {
        warning("P is already a ranking.\nExpected Ranks correspond to the only possible ranking.")
        expected_full <- rank(colSums(P_full), ties.method = "max")
        rank.spread_full <- rep(0, nrow(P_full))
        mrp_full <- P_full
        mrp_full[mrp_full == t(mrp_full)] <- 0
        rp_full  <-  matrix(0, nrow(P_full), nrow(P_full))
        for (i in 1:nrow(P_full)) {
            rp_full[i, expected_full[i]] <- 1
        }
        
        return(list(lin.ext = 1, 
                    names = rownames(P_full), 
                    mse = MSE, 
                    rank.prob = rp_full, 
                    relative.rank = mrp_full, 
                    expected.rank = expected_full, 
                    rank.spread = rank.spread_full))
    }
    
    # sanity check if applicable ------------------------------------------------
    if (nrow(P) > 40 & comparable_pairs(P) < 0.4 & force == F) {
        stop("Input data too big. Use approximations or set force=T if you know what you are doing")
    }
    # Prepare Data structures---------------------
    topo.order <- as.vector(igraph::topological.sort(igraph::graph_from_adjacency_matrix(P, "directed")))
    
    P <- P[topo.order, topo.order]
    ImPred <- igraph::get.adjlist(igraph::graph_from_adjacency_matrix(P, "directed"), "in")
    ImPred <- lapply(ImPred, function(x) as.vector(x) - 1)
    
    ImSucc <- igraph::get.adjlist(igraph::graph_from_adjacency_matrix(P, "directed"), "out")
    ImSucc <- lapply(ImSucc, function(x) as.vector(x) - 1)
    # TREEOFIDEALS ----------------------------------------------------
    if (verbose == TRUE) {
        print("building tree of ideals")
    }
    tree <- treeOfIdeals(ImPred)
    nIdeals <-  length(tree$label)
    if (verbose == TRUE) {
        print("tree of ideals built")
    }
    Ek <-  sapply(0:(nElem - 1), function(x) {
        which(tree$label == x) - 1
    })
    # tree$child=lapply(tree$child,function(x) {idx=order(tree$label[x+1],decreasing=T);x[idx]})
    if (verbose == TRUE) {
        print("building lattice of Ideals")
    }
    latofI <-  LatticeOfIdeals(tree$child, tree$parent, Ek, nElem, nIdeals)
    
    if (verbose == TRUE) {
        print("lattice of ideals built")
    }
    ideallist <-  listingIdeals(ImSucc, nElem, nIdeals)
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
    expected <- res$rp %*% 1:nElem
    rank.spread <- rowSums((matrix(rep(1:nElem, each = nElem), nElem, nElem) - c(expected))^2 * res$rp)
    expected <- c(expected)
    ############################### Insert equivalent nodes again
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
    for (val in sort(unique(expected_full), decreasing = T)) {
        idx <- which(expected_full == val)
        expected_full[idx] <- expected_full[idx] + sum(duplicated(MSE[expected_full <= val]))
    }
    ############################### 
    if (only.results) {
        return(list(lin.ext = res$linext, 
                    names = rownames(P_full), 
                    mse = MSE, rank.prob = rp_full, 
                    relative.rank = t(mrp_full), 
                    expected.rank = expected_full, 
                    rank.spread = sqrt(rank.spread_full)))
    } else {
        return(list(lin.ext = res$linext, 
                    names = rownames(P_full), 
                    mse = MSE, 
                    rank.prob = rp_full, 
                    relative.rank = t(mrp_full), 
                    expected.rank = expected_full, 
                    rank.spread = sqrt(rank.spread_full), 
                    topo.order = topo.order, 
                    tree = tree, 
                    lattice = latofI, 
                    ideals = ideallist))
    }
}

