test_that("exact_rank_prob handles structurally equivalent nodes", {
    # 1 and 2 are equivalent, both dominated by 3; 4 is incomparable to all
    P <- matrix(0, 4, 4)
    P[1, 2] <- P[2, 1] <- 1
    P[1, 3] <- P[2, 3] <- 1
    res <- exact_rank_prob(P, only.results = FALSE)
    expect_equal(res$mse, c(1, 1, 2, 3))
    expect_equal(res$lin.ext, 3)
    expect_equal(unname(res$rank.prob[1, ]), unname(res$rank.prob[2, ]))
    # extensions of the classes: 4 < {1,2} < 3, {1,2} < 4 < 3, {1,2} < 3 < 4 with ties at the maximum
    expect_equal(unname(res$expected.rank), c(7 / 3, 7 / 3, 11 / 3, 8 / 3))
    rk <- get_rankings(res)
    expect_equal(ncol(rk), 3)
    expect_equal(rk[1, ], rk[2, ])
    # the same result via the mcmc and approximation code paths
    set.seed(1)
    mc <- mcmc_rank_prob(P, rp = 5e4)
    expect_lt(max(abs(mc$expected.rank - res$expected.rank)), 0.1)
    expect_equal(approx_rank_relative(P)[1, 2], 0)
})

test_that("exact_rank_prob reports progress when verbose", {
    P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
    expect_output(exact_rank_prob(P, verbose = TRUE), "tree of ideals built")
})

test_that("exact_rank_prob refuses large sparse posets unless forced", {
    P <- matrix(0, 45, 45)
    P[1, 2] <- 1
    expect_error(exact_rank_prob(P), "force")
})

test_that("dominance_graph returns the directed dominance relation", {
    P <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
    P[1, 2] <- P[2, 3] <- P[1, 3] <- 1
    d <- dominance_graph(P)
    expect_true(igraph::is_directed(d))
    expect_equal(igraph::ecount(d), 3)
    expect_equal(igraph::V(d)$name, letters[1:3])
    expect_error(dominance_graph("a"), "matrix")
})

test_that("resistance distances match the pseudo-inverse definition", {
    g <- igraph::make_ring(5)
    R <- indirect_relations(g, "dist_resist")
    # on a cycle of length n the resistance between nodes at distance k is k(n - k) / n
    d <- igraph::distances(g)
    expect_equal(R, d * (5 - d) / 5)
})
