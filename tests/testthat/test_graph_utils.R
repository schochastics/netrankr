elec_graph <- function() {
    elec <- matrix(ncol = 2, byrow = TRUE, c(
        11, 1, 11, 10, 1, 2, 2, 3, 2, 9,
        3, 4, 3, 8, 4, 5, 5, 6, 5, 7,
        6, 7, 7, 8, 8, 9, 9, 10
    ))
    igraph::graph_from_edgelist(elec, directed = FALSE)
}

test_that("swan_connectivity counts disconnected pairs", {
    g <- igraph::make_star(5, "undirected")
    # removing the center disconnects all 4 * 3 ordered leaf pairs
    expect_equal(swan_connectivity(g), c(12, 0, 0, 0, 0))
    expect_equal(swan_connectivity(elec_graph()), rep(0, 11))
    expect_error(swan_connectivity("a"), "igraph")
})

test_that("swan_closeness and swan_efficiency match their definitions", {
    g <- elec_graph()
    inv <- function(g) {
        d <- 1 / igraph::distances(g)
        d[is.infinite(d)] <- 0
        sum(d)
    }
    d <- igraph::distances(g)
    for (i in c(1, 5, 11)) {
        gi <- igraph::delete_vertices(g, i)
        dinv <- 1 / d
        dinv[is.infinite(dinv)] <- 0
        expect_equal(swan_closeness(g)[i], inv(gi) - (inv(g) - sum(dinv[i, ]) - sum(dinv[, i])))
        expect_equal(swan_efficiency(g)[i], sum(igraph::distances(gi)) - (sum(d) - sum(d[i, ]) - sum(d[, i])))
    }
    star <- igraph::make_star(5, "undirected")
    expect_equal(swan_efficiency(star)[1], Inf)
})

test_that("swan_combinatory removes all nodes in every scenario", {
    g <- elec_graph()
    set.seed(1)
    res <- swan_combinatory(g, 3)
    expect_equal(dim(res), c(11, 5))
    expect_equal(res[, 1], seq_len(11) / 11)
    expect_equal(res[11, 2:5], rep(1, 4))
    expect_true(all(diff(res[, 5]) >= 0))
    # more repetitions than nodes used to fail
    expect_no_error(swan_combinatory(igraph::make_ring(4), 10))
    expect_error(swan_combinatory(g, 0), "positive")
    expect_error(swan_combinatory(igraph::make_empty_graph(1), 2), "two vertices")
})

test_that("majorization_gap normalises disconnected graphs globally", {
    set.seed(1)
    g1 <- igraph::sample_gnp(12, 0.4)
    g2 <- igraph::sample_gnp(6, 0.5)
    g <- igraph::disjoint_union(g1, g2)
    raw <- majorization_gap(g1, norm = FALSE) + majorization_gap(g2, norm = FALSE)
    expect_warning(expect_equal(majorization_gap(g, norm = FALSE), raw), "not connected")
    expect_warning(gap <- majorization_gap(g), "not connected")
    expect_equal(gap, raw / igraph::ecount(g))
    expect_lte(gap, 1)
    expect_warning(expect_equal(majorization_gap(igraph::make_empty_graph(3, directed = FALSE)), 0))
})

test_that("threshold_graph validates its input", {
    expect_error(threshold_graph(1, 0.5), "greater than 1")
    expect_error(threshold_graph(10, 2), "probability")
    expect_error(threshold_graph(bseq = c(1, 0, 2)), "binary")
    g <- threshold_graph(bseq = c(0, 0, 1, 0, 1))
    expect_equal(igraph::vcount(g), 5)
    expect_equal(igraph::ecount(g), 2 + 4)
    expect_equal(comparable_pairs(neighborhood_inclusion(g)), 1)
})

test_that("spectral_gap validates its input", {
    g <- igraph::make_full_graph(5)
    # eigenvalues of K5 are 4 and -1
    expect_equal(spectral_gap(g, "abs"), 5)
    expect_equal(spectral_gap(g, "frac"), 1.25)
    expect_equal(spectral_gap(igraph::make_empty_graph(3, directed = FALSE)), 0)
    expect_error(spectral_gap(igraph::make_ring(3, directed = TRUE)), "undirected")
    expect_error(spectral_gap(g, "foo"), "method")
    expect_error(spectral_gap("a"), "igraph")
})
