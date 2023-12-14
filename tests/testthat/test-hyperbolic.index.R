test_that("hyperbolic index works", {
    g <- igraph::make_full_graph(2)
    expect_true(all(hyperbolic_index(g, "odd") == 0))
    expect_true(all(hyperbolic_index(g, "even") != 0))
    expect_error(hyperbolic_index(g, "wrong"))
})
