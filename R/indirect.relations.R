#' @title Indirect relations in a network
#' @description Derive indirect relations for a given network.
#' Observed relations, like presents or absence of a relation, are commonly not the center
#' of analysis, but are transformed in a new set of indirect relation like shortest path
#' distances among nodes. These transformations are usually an implicit step when centrality
#' indices are used. Making this step explicit gives more possibilities, for example
#' calculating partial centrality rankings with [positional_dominance].
#' @param g igraph object. The network for which relations should be derived.
#' @param type String giving the relation to be calculated. See Details for options.
#' @param lfparam Numeric parameter. Only used if type = "dist_lf".
#' @param dwparam Numeric parameter. Only used if type = "dist_walk".
#' @param netflowmode String, one of raw, frac, or norm. Only used if type = "depend_netflow".
#' @param rspxparam Numeric parameter. Only used if type = "depend_rsps" or type = "depend_rspn".
#' @param FUN A function that allows the transformation of relations. See Details.
#' @param ... Additional arguments passed to FUN.
#' @details The `type` parameter has the following options.
#'
#' \emph{'adjacency'} returns the adjacency matrix of the network.
#'
#' \emph{'weights'} returns the weighted adjacency matrix of the network if an edge
#' attribute 'weight' is present.
#'
#' \emph{'dist_sp'} returns shortest path distances between all pairs of nodes.
#'
#' \emph{'depend_sp'} returns dyadic dependencies
#' \deqn{\delta(u,s) = \sum_{t \in V} \frac{\sigma(s,t|u)}{\sigma(s,t)}}
#' where \eqn{\sigma(s,t|u)} is the number of shortest paths from s to t that include u and
#' \eqn{\sigma(s,t)} is the total number of shortest (s,t)-paths. This relation is used
#' for betweenness-like centrality indices.
#'
#' \emph{'walks'} returns walk counts between pairs of nodes, usually they are
#' weighted decreasingly in their lengths or other properties which can be done by adding
#' a function in \code{FUN}.  See [transform_relations] for options.
#'
#' \emph{'dist_resist'} returns the resistance distance between all pairs of nodes.
#'
#' \emph{'dist_lf'} returns a logarithmic forest distance \eqn{d_\alpha(s,t)}. The logarithmic forest
#' distances form a one-parametric family of distances, converging to shortest path distances as \eqn{\alpha -> 0}
#' and to the resistance distance as \eqn{\alpha -> \infty}. See (Chebotarev, 2011) for more details.
#' The parameter `lfparam` can be used to tune \eqn{\alpha}.
#'
#' \emph{'dist_walk'} returns the walk distance \eqn{d_\alpha^W(s,t)} between nodes. The walk distances form a one-parametric
#' family of distances, converging to shortest path distances as \eqn{\alpha -> 0} and to longest
#' walk distances for \eqn{\alpha -> \infty}. Walk distances contain the logarithmic forest
#' distances as a special case. See (Chebotarev, 2012) for more details.
#'
#' \emph{'dist_rwalk'} returns the expected length of a random walk between two nodes. For more
#' details see (Noh and Rieger, 2004)
#'
#' \emph{'depend_netflow'} returns dependencies based on network flow (See Freeman et al.,1991).
#' If `netflowmode="raw"`, the function returns
#' \deqn{\delta(u,s) = \sum_{t \in V} f(s,t,G)-f(s,t,G-v)}
#' where f(s,t,G) is the maximum flow from s to t in G and f(s,t,G-v) in G without the node v.
#' For `netflowmode="frac"` it returns dependencies in the form, similar to shortest path dependencies:
#' \deqn{\delta(u,s) = \sum_{t \in V} \frac{f(s,t,G)-f(s,t,G-v)}{f(s,t,G)}}
#'
#' \emph{'depend_curflow'} returns pairwise dependencies based on current flow. The relation is
#' based on the same idea as 'depend_sp' and 'depend_netflow'. However, instead of considering
#' shortest paths or network flow, the current flow (or equivalent: random walks) between nodes
#' are of interest. See (Newman, 2005) for details.
#'
#' \emph{'depend_exp'} returns pairwise dependencies based on 'communicability':
#' \deqn{\delta(u,s)=\frac{1}{(n-1)^2-(n-1)}\sum_{t \in V} \frac{exp(A)_{st}-exp(A+E(u))_{st}}{exp(A)_{st}},}
#' where E(u) removes all edges incident to u, i.e. \eqn{A+E(u)} is A with zeros
#' in row and column u. The normalisation by \eqn{(n-1)^2-(n-1)}
#' follows (Estrada et al., 2009), see there for additional details.
#'
#' \emph{'depend_rsps'}. Simple randomized shortest path dependencies.
#' The simple RSP dependency of a node u with respect to absorbing paths from s to t,
#' is defined as the expected number of visits through u over all s-t-walks. The
#' parameter `rspxparam` is the "inverse temperature parameter".
#' If it converges to infinity, only shortest paths are considered and the expected
#' number of visits to a node on a shortest path approaches the probability of
#' following that particular path. When the parameter converges to zero, then the
#' dependencies converge to the expected number of visits to a node over all absorbing
#' walks with respect to the unbiased random walk probabilities. This means for undirected networks,
#' that the relations converge to adjacency. See (Kivimäki et al., 2016) for details.
#'
#' \emph{'depend_rspn'} Net randomized shortest path dependencies.
#' The parameter `rspxparam` is the "inverse temperature parameter". The asymptotic
#' for the infinity case are the same as for 'depend_rsps'. If the parameter approaches zero, then
#' it converges to 'depend_curflow'. The net randomized shortest path dependencies
#' are closely related to the random walk interpretation of current flows.
#'  See (Kivimäki et al., 2016) for technical details.
#'
#'
#' All relations except 'weights' are computed on the unweighted graph: an edge attribute
#' 'weight' is ignored with a warning. Before version 2.0.0, 'dist_sp', 'dist_resist' and
#' 'dist_lf' used it. For weighted shortest path distances use [igraph::distances()] directly. 'dist_resist', 'depend_curflow', 'dist_rwalk', 'depend_exp',
#' 'depend_rsps', 'depend_rspn' and 'depend_netflow' with `netflowmode = "frac"` are only
#' defined for connected graphs and raise an error otherwise.
#'
#' The function \code{FUN} is used to transform the indirect
#' relation. See [transform_relations] for predefined functions and additional help.
#'
#' @return A matrix containing indirect relations in a network.
#' @author David Schoch
#' @seealso [aggregate_positions] to build centrality indices, [positional_dominance] to derive dominance relations
#' @references Chebotarev, P., 2012. The walk distances in graphs. *Discrete Applied Mathematics*, 160(10), pp.1484-1500.
#'
#' Chebotarev, P., 2011. A class of graph-geodetic distances generalizing the shortest-path and
#' the resistance distances. *Discrete Applied Mathematics* 159,295-302.
#'
#' Noh, J.D. and Rieger, H., 2004. Random walks on complex networks. *Physical Review Letters*, 92(11), p.118701.
#'
#' Freeman, L.C., Borgatti, S.P., and White, D.R., 1991.
#' Centrality in Valued Graphs: A Measure of Betweenness Based on Network Flow. *Social Networks* 13(2), 141-154.
#'
#' Newman, M.E., 2005. A measure of betweenness centrality based on random walks. *Social Networks*, 27(1), pp.39-54.
#'
#' Estrada, E., Higham, D.J., and Hatano, N., 2009.
#' Communicability betweenness in complex networks. *Physica A* 388,764-774.
#'
#' Kivimäki, I., Lebichot, B., Saramäki, J., and Saerens, M., 2016.
#' Two betweenness centrality measures based on Randomized Shortest Paths
#' *Scientific Reports* 6: 19668
#' @examples
#' library(igraph)
#' data("dbces11")
#'
#' # shortest path distances
#' D <- indirect_relations(dbces11, type = "dist_sp")
#'
#' # inverted shortest path distances
#' D <- indirect_relations(dbces11, type = "dist_sp", FUN = dist_inv)
#'
#' # shortes path dependencies (used for betweenness)
#' D <- indirect_relations(dbces11, type = "depend_sp")
#'
#' # walks attenuated exponentially by their length
#' W <- indirect_relations(dbces11, type = "walks", FUN = walks_exp)
#' @export
indirect_relations <- function(g,
                               type = "dist_sp",
                               lfparam = NULL,
                               dwparam = NULL,
                               netflowmode = "",
                               rspxparam = NULL,
                               FUN = identity, ...) {
    if (!igraph::is_igraph(g)) {
        stop("g must be an igraph object")
    }

    if (igraph::is_directed(g)) {
        stop("g must be an undirected graph")
    }

    if (!is.character(type) || length(type) != 1) {
        stop("type must be a single string")
    }
    if (type == "dependencies") {
        warning('type="dependencies" is deprecated. Using "depend_sp" instead.\n')
        type <- "depend_sp"
    }
    if (type == "geodesic") {
        warning('type="geodesic" is deprecated. Using "dist_sp" instead.\n')
        type <- "dist_sp"
    }
    if (type == "resistance") {
        warning('type="resistance" is deprecated. Using "dist_resist" instead.\n')
        type <- "dist_resist"
    }
    if (type == "identity") {
        warning('type="identity" is deprecated. Using "adjacency" instead.\n')
        type <- "adjacency"
    }
    if (type %in% connected_types ||
        (type == "depend_netflow" && netflowmode == "frac")) {
        if (!igraph::is_connected(g)) {
            stop('type = "', type, '" is only defined for connected graphs')
        }
    }
    # all relations except "weights" are defined on unweighted graphs
    if (type != "weights" && "weight" %in% igraph::edge_attr_names(g)) {
        warning(
            'the edge attribute "weight" is ignored for type = "', type, '". ',
            "Since netrankr 2.0.0 all relations except \"weights\" use the unweighted graph.",
            call. = FALSE
        )
        g <- strip_weights(g)
    }

    if (type == "dist_sp") {
        rel <- igraph::distances(g, mode = "all")
        rel <- FUN(rel, ...)
    } else if (type == "weights") {
        if (is.null(igraph::edge_attr(g, "weight"))) {
            warning('no weight attribute present. using "adjacency" instead.\n')
            rel <- adjacency_matrix(g)
        } else {
            rel <- weighted_adjacency_matrix(g)
        }
        rel <- FUN(rel, ...)
        diag(rel) <- 0
    } else if (type == "adjacency") {
        rel <- adjacency_matrix(g)
        rel <- FUN(rel, ...)
        diag(rel) <- 0
    } else if (type == "depend_sp") {
        adj <- lapply(igraph::as_adj_list(g), function(x) x - 1)
        rel <- dependency(adj)
    } else if (type == "walks") {
        eigen.decomp <- eigen(adjacency_matrix(g), symmetric = TRUE)
        lambda <- eigen.decomp$values
        X <- eigen.decomp$vectors
        rel <- X %*% (FUN(lambda, ...) * t(X))
    } else if (type == "dist_resist") {
        L <- igraph::laplacian_matrix(g, sparse = FALSE)
        n <- igraph::vcount(g)
        A <- L + matrix(1 / n, n, n)
        C <- solve(A)
        rel <- outer(diag(C), diag(C), "+") - 2 * C
        rel <- FUN(rel, ...)
    } else if (type == "dist_lf") {
        if (is.null(lfparam)) {
            stop('argument "lfparam" is missing for "dist_lf", with no default')
        }
        rel <- log_forest_fct(g, lfparam)
        rel <- FUN(rel, ...)
    } else if (type == "dist_walk") {
        if (is.null(dwparam)) {
            stop('argument "dwparam" is missing for "dist_walk", with no default')
        }
        rel <- dist_walk_fct(g, dwparam)
        rel <- FUN(rel, ...)
    } else if (type == "depend_netflow") {
        if (length(netflowmode) != 1 || !netflowmode %in% c("raw", "frac", "norm")) {
            stop('netflowmode must be one of "raw", "frac", "norm"')
        }
        rel <- depend_netflow_fct(g, netflowmode)
        rel <- FUN(rel, ...)
    } else if (type == "depend_exp") {
        rel <- depend_exp_fct(g)
        rel <- FUN(rel, ...)
    } else if (type == "depend_rsps") {
        if (is.null(rspxparam)) {
            stop('argument "rspxparam" is missing for "depend_rsps", with no default')
        }
        rel <- depend_rsps_fct(g, rspxparam)
        rel <- FUN(rel, ...)
    } else if (type == "depend_rspn") {
        if (is.null(rspxparam)) {
            stop('argument "rspxparam" is missing for "depend_rspn", with no default')
        }
        rel <- depend_rspn_fct(g, rspxparam)
        rel <- FUN(rel, ...)
    } else if (type == "depend_curflow") {
        rel <- depend_curflow_fct(g)
        rel <- FUN(rel, ...)
    } else if (type == "dist_rwalk") {
        rel <- dist_rwalk_fct(g)
        rel <- FUN(rel, ...)
    } else {
        stop(paste(type, "is not defined as indirect relation."))
    }
    # add names if present
    if (is.null(rownames(rel)) && !is.null(igraph::V(g)$name)) {
        rownames(rel) <- colnames(rel) <- igraph::V(g)$name
    }
    return(rel)
}

# -------------------------------------------------------------------------------

# relations that are only defined for connected graphs
connected_types <- c(
    "dist_resist", "depend_curflow", "dist_rwalk",
    "depend_exp", "depend_rsps", "depend_rspn"
)

strip_weights <- function(g) {
    if ("weight" %in% igraph::edge_attr_names(g)) {
        g <- igraph::delete_edge_attr(g, "weight")
    }
    g
}

# unweighted adjacency matrix (multiple edges are counted)
adjacency_matrix <- function(g) {
    igraph::as_adjacency_matrix(strip_weights(g), type = "both", sparse = FALSE)
}

# weighted adjacency matrix (weights of multiple edges are summed)
weighted_adjacency_matrix <- function(g) {
    n <- igraph::vcount(g)
    el <- igraph::as_edgelist(g, names = FALSE)
    w <- igraph::edge_attr(g, "weight")
    A <- as.matrix(Matrix::sparseMatrix(
        i = c(el[, 1], el[, 2]), j = c(el[, 2], el[, 1]),
        x = c(w, w), dims = c(n, n)
    ))
    dimnames(A) <- NULL
    A
}

log_forest_fct <- function(g, lfparam) {
    n <- igraph::vcount(g)
    gamma <- log(exp(1) + lfparam^(2 / n))

    L <- igraph::laplacian_matrix(g, sparse = FALSE)
    I <- diag(1, n)
    Q <- solve(I + lfparam * L)

    if (lfparam == 1) {
        H <- gamma * log(Q)
    } else {
        H <- gamma * (lfparam - 1) * logb(Q, lfparam)
    }
    rel <- 0.5 * (diag(H) %*% t(rep(1, n)) + rep(1, n) %*% t(diag(H))) - H
    return(rel)
}

depend_netflow_fct <- function(g, netflowmode) {
    n <- igraph::vcount(g)
    # maximum flows are symmetric in undirected graphs, so each pair is computed once
    pair_flows <- function(g) {
        m <- igraph::vcount(g)
        flow <- matrix(0, m, m)
        for (s in seq_len(m - 1)) {
            for (t in (s + 1):m) {
                flow[s, t] <- flow[t, s] <- igraph::max_flow(g, s, t)$value
            }
        }
        flow
    }
    mflow <- if (n > 1) pair_flows(g) else matrix(0, n, n)
    if (netflowmode == "norm") {
        maxoflo <- vapply(seq_len(n), function(i) sum(mflow[-i, -i]), numeric(1))
    }
    flow_smat <- matrix(0, n, n)
    for (i in seq_len(n)) {
        others <- seq_len(n)[-i]
        mflow_i <- mflow[others, others, drop = FALSE]
        flow_i <- if (n > 2) pair_flows(igraph::delete_vertices(g, i)) else matrix(0, n - 1, n - 1)
        loss <- mflow_i - flow_i
        if (netflowmode == "frac") {
            loss <- loss / mflow_i
        }
        diag(loss) <- 0
        flow_smat[i, others] <- rowSums(loss)
    }
    if (netflowmode == "norm") {
        flow_smat <- flow_smat / maxoflo * 2
    }
    return(flow_smat)
}

depend_exp_fct <- function(g) {
    A <- adjacency_matrix(g)
    n <- nrow(A)
    eigen_A <- eigen(A, symmetric = TRUE)
    expA <- eigen_A$vectors %*% (exp(eigen_A$values) * t(eigen_A$vectors))
    C <- (n - 1)^2 - (n - 1)
    combet <- matrix(0, n, n)
    if (C == 0) {
        return(combet)
    }
    for (i in seq_len(n)) {
        # removing the edges of i leaves exp(A[-i, -i]) on the other nodes
        eigen_E <- eigen(A[-i, -i, drop = FALSE], symmetric = TRUE)
        expE <- eigen_E$vectors %*% (exp(eigen_E$values) * t(eigen_E$vectors))
        rel <- (expA[-i, -i, drop = FALSE] - expE) / expA[-i, -i, drop = FALSE]
        diag(rel) <- 0
        combet[i, -i] <- rowSums(rel) / C
    }

    return(combet)
}

rsp_fundamental <- function(g, rspxparam) {
    A <- adjacency_matrix(g)
    n <- nrow(A)
    P_ref <- A / rowSums(A)
    C <- 1 / A
    C[is.infinite(C)] <- 0
    W <- P_ref * exp(-rspxparam * C)
    Z <- solve(diag(1, n) - W)
    list(Z = Z, W = W, n = n)
}

depend_rsps_fct <- function(g, rspxparam) {
    rsp <- rsp_fundamental(g, rspxparam)
    Z <- rsp$Z
    n <- rsp$n
    Zd <- diag(Z)
    # bet.mat[u, t]: expected visits to u on walks from any s != u absorbed at t,
    # sum_s z_su z_ut / z_st - (n - 1) z_tu z_ut / z_tt - z_uu
    bet.mat <- Z * crossprod(Z, 1 / Z) -
        (n - 1) * t(Z) * Z / matrix(Zd, n, n, byrow = TRUE) -
        Zd
    diag(bet.mat) <- 0

    return(bet.mat)
}

depend_rspn_fct <- function(g, rspxparam) {
    rsp <- rsp_fundamental(g, rspxparam)
    adj <- igraph::as_adj_list(g, "all")
    A <- lapply(adj, function(x) as.vector(x) - 1)
    bet.mat <- dependRspn(A, rsp$Z, 1 / rsp$Z, rsp$W, rsp$n)
    return(bet.mat)
}

depend_curflow_fct <- function(g) {
    n <- igraph::vcount(g)
    L <- igraph::laplacian_matrix(g, sparse = FALSE)

    Tmat <- solve(L[1:(n - 1), 1:(n - 1)])
    Tmat <- rbind(cbind(Tmat, 0), 0)

    el <- igraph::as_edgelist(g, names = FALSE)
    m <- igraph::ecount(g)
    el <- el - 1L

    bet.mat <- dependCurFlow(Tmat, el, m, n)
    return(bet.mat)
}

dist_rwalk_fct <- function(g) {
    # hitting times h(i -> j) from the pseudo-inverse of the Laplacian
    L <- igraph::laplacian_matrix(g, sparse = FALSE)
    n <- nrow(L)
    d <- diag(L)
    J <- matrix(1 / n, n, n)
    Lp <- solve(L + J) - J
    r <- c(Lp %*% d)
    H <- outer(r, r, "-") + sum(d) * (matrix(diag(Lp), n, n, byrow = TRUE) - Lp)
    H <- t(H) # H[j, i] = h(i -> j) to fit the framework with rowSums
    diag(H) <- 0
    dimnames(H) <- NULL
    return(H)
}

dist_walk_fct <- function(g, dwparam) {
    n <- igraph::vcount(g)
    A <- adjacency_matrix(g)
    bigeig <- eigen(A, symmetric = TRUE, only.values = TRUE)$values[1]
    t <- (1 / dwparam + bigeig)^(-1)
    I <- diag(1, n)
    Rt <- solve((I - t * A))
    Ht <- log(Rt)
    Dt <- 0.5 * (diag(Ht) %*% t(rep(1, n)) + rep(1, n) %*% t(diag(Ht))) - Ht
    alpha <- dwparam # (1/dwparam - bigeig)^(-1)
    if (alpha != 1) {
        gamma <- log(exp(1) + alpha^(2 / n)) * (alpha - 1) / log(alpha)
    } else {
        gamma <- log(exp(1) + 1)
    }
    Dt <- gamma * Dt
    return(Dt)
}
