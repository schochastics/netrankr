connected_gnp <- function(n, p, seed) {
    set.seed(seed)
    repeat {
        g <- igraph::sample_gnp(n, p)
        if (igraph::is_connected(g)) {
            return(g)
        }
    }
}

test_that("edge weights are ignored by all types except 'weights'", {
    g <- connected_gnp(10, 0.4, 1)
    gw <- g
    igraph::E(gw)$weight <- seq_len(igraph::ecount(g))
    for (type in c("dist_sp", "adjacency", "depend_sp", "dist_resist", "depend_curflow", "dist_rwalk", "depend_exp")) {
        expect_equal(indirect_relations(gw, type), indirect_relations(g, type), info = type)
    }
    expect_equal(
        indirect_relations(gw, "walks", FUN = walks_exp),
        indirect_relations(g, "walks", FUN = walks_exp)
    )
    expect_equal(
        indirect_relations(gw, "depend_rsps", rspxparam = 1),
        indirect_relations(g, "depend_rsps", rspxparam = 1)
    )
    W <- indirect_relations(gw, "weights")
    el <- igraph::as_edgelist(gw, names = FALSE)
    expect_equal(W[el], igraph::E(gw)$weight)
    expect_equal(W, t(W))
})

test_that("relations for connected graphs reject disconnected graphs", {
    g <- igraph::make_graph(c(1, 2, 2, 3, 4, 5, 5, 6, 4, 6), directed = FALSE)
    for (type in c("dist_resist", "depend_curflow", "dist_rwalk", "depend_exp")) {
        expect_error(indirect_relations(g, type), "connected", info = type)
    }
    expect_error(indirect_relations(g, "depend_rsps", rspxparam = 1), "connected")
    expect_error(indirect_relations(g, "depend_netflow", netflowmode = "frac"), "connected")
    expect_true(all(is.finite(indirect_relations(g, "depend_netflow", netflowmode = "raw"))))
    expect_error(indirect_relations(g, c("dist_sp", "adjacency")), "single string")
    expect_error(indirect_relations(g, "depend_netflow"), "netflowmode")
})

test_that("depend_rsps matches the definition via absorbing walks", {
    g <- connected_gnp(8, 0.4, 5)
    theta <- 1
    A <- igraph::as_adjacency_matrix(g, sparse = FALSE)
    C <- 1 / A
    C[is.infinite(C)] <- 0
    W <- A / rowSums(A) * exp(-theta * C)
    n <- nrow(W)
    ref <- matrix(0, n, n)
    for (t in seq_len(n)) {
        Wt <- W
        Wt[t, ] <- 0
        Zt <- solve(diag(n) - Wt)
        for (s in setdiff(seq_len(n), t)) {
            for (u in setdiff(seq_len(n), c(s, t))) {
                ref[u, t] <- ref[u, t] + Zt[s, u] * Zt[u, t] / Zt[s, t]
            }
        }
    }
    expect_equal(indirect_relations(g, "depend_rsps", rspxparam = theta), ref)
})

test_that("dist_rwalk returns hitting times", {
    g <- igraph::make_ring(6)
    H <- indirect_relations(g, "dist_rwalk")
    # hitting time between nodes at distance k on a cycle of length n is k(n - k)
    d <- igraph::distances(g)
    expect_equal(H, d * (6 - d))
})

test_that("depend_exp handles multi-edges", {
    g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 3, 4, 4, 1), directed = FALSE)
    D <- indirect_relations(g, "depend_exp")
    expect_true(all(is.finite(D)))
    expect_gt(sum(D[1, ]), 0)
})

test_that("walks_uptok includes the identity term and handles k = 0", {
    expect_equal(walks_uptok(2, alpha = 1, k = 0), 1)
    expect_equal(walks_uptok(2, alpha = 0.5, k = 2), 1 + 1 + 1)
})

test_that("hyperbolic_index validates input and handles isolates", {
    g <- igraph::make_graph(c(1, 2, 2, 3), n = 4, directed = FALSE)
    expect_equal(hyperbolic_index(g, "odd")[4], 0)
    expect_equal(hyperbolic_index(g, "even")[4], 0)
    expect_error(hyperbolic_index(igraph::make_ring(3, directed = TRUE)), "undirected")
    expect_error(hyperbolic_index(g, "foo"), "even or odd")
})

test_that("aggregate_positions works on Matrix objects", {
    M <- Matrix::Matrix(matrix(c(1, 2, 3, 4), 2, 2), sparse = TRUE)
    expect_equal(aggregate_positions(M, "self"), c(1, 4))
    expect_equal(aggregate_positions(M, "sum"), c(4, 6))
    expect_equal(aggregate_positions(M, "invsum"), 1 / c(4, 6))
    expect_equal(aggregate_positions(as.matrix(M), "prod"), c(3, 8))
})

test_that("walks relations work on a single node", {
    g <- igraph::make_empty_graph(1, directed = FALSE)
    expect_equal(unname(indirect_relations(g, "walks", FUN = walks_exp_odd)), matrix(0, 1, 1))
})
