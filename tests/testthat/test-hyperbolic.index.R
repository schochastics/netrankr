test_that("hyperbolic index works", {
  skip_on_cran()
  g <- igraph::make_full_graph(2)
  expect_true(all(round(hyperbolic_index(g, "odd"), 6) == 0))
  expect_true(all(hyperbolic_index(g, "even") != 0))
  expect_error(hyperbolic_index(g, "wrong"))
})
