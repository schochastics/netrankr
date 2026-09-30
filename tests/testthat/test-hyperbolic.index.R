test_that("hyperbolic index works", {
  skip_on_cran()
  g <- igraph::make_full_graph(2)
  expect_equal(hyperbolic_index(g, "odd"), c(0, 0), tolerance = 1e-8)
  expect_true(all(hyperbolic_index(g, "even") != 0))
  expect_error(hyperbolic_index(g, "wrong"))
})
