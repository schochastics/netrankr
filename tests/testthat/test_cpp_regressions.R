test_that("mcmc_rank_prob is unbiased on a small poset", {
    P <- matrix(0, 5, 5)
    P[1, 2] <- P[2, 3] <- P[1, 3] <- P[4, 5] <- 1
    ex <- exact_rank_prob(P)
    set.seed(1)
    mc <- mcmc_rank_prob(P, rp = 2e5)
    expect_lt(max(abs(ex$expected.rank - mc$expected.rank)), 0.05)
    expect_lt(max(abs(ex$relative.rank - mc$relative.rank)), 0.02)
    expect_equal(unname(mc$relative.rank + t(mc$relative.rank) + diag(5)), matrix(1, 5, 5))
})

test_that("mcmc_rank_prob validates rp and uses the full sample size", {
    P <- matrix(0, 4, 4)
    P[1, 2] <- P[2, 1] <- 1
    expect_error(mcmc_rank_prob(P, rp = 0), "rp must be")
    expect_error(mcmc_rank_prob(P, rp = "a"), "rp must be")
    expect_no_error(mcmc_rank_prob(P))
})

test_that("depend_sp does not overflow on graphs with many shortest paths", {
    g <- igraph::make_lattice(c(20, 20))
    d <- indirect_relations(g, type = "depend_sp")
    expect_equal(unname(rowSums(d) / 2), igraph::betweenness(g), tolerance = 1e-8)
})

test_that("depend_rspn converges to depend_curflow", {
    set.seed(127)
    g <- igraph::sample_gnp(20, 0.4)
    a <- indirect_relations(g, type = "depend_rspn", rspxparam = 1e-5)
    b <- indirect_relations(g, type = "depend_curflow")
    expect_equal(a, b, tolerance = 1e-3)
})

test_that("transitive_reduction ignores a reflexive diagonal", {
    P <- matrix(0, 3, 3)
    P[upper.tri(P)] <- 1
    R <- matrix(0, 3, 3)
    R[1, 2] <- R[2, 3] <- 1
    expect_equal(transitive_reduction(P), R)
    expect_equal(transitive_reduction(P + diag(3)), R)
})

test_that("positional_dominance rejects non-square one-mode input", {
    A <- matrix(c(1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0), 4, 3)
    expect_error(positional_dominance(A, map = FALSE), "square")
})
