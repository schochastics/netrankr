context("indirect relations")
library(igraph)
library(magrittr)
library(Matrix)

test_that("adjacency is correct", {
  g <- make_empty_graph(n = 11, directed = FALSE)
  g <- add_edges(g, c(
    1, 11, 2, 4, 3, 5, 3, 11, 4, 8, 5, 9, 5, 11, 6, 7, 6, 8,
    6, 10, 6, 11, 7, 9, 7, 10, 7, 11, 8, 9, 8, 10, 9, 10
  ))
  A <- indirect_relations(g, type = "adjacency")
  expect_equal(A, as_adjacency_matrix(g, sparse = FALSE))
})

test_that("shortest path distances are correct", {
  g <- make_empty_graph(n = 11, directed = FALSE)
  g <- add_edges(g, c(
    1, 11, 2, 4, 3, 5, 3, 11, 4, 8, 5, 9, 5, 11, 6, 7, 6, 8,
    6, 10, 6, 11, 7, 9, 7, 10, 7, 11, 8, 9, 8, 10, 9, 10
  ))

  D_igraph <- distances(g)
  D_igraph_inv <- D_igraph^(-1)
  diag(D_igraph_inv) <- 0
  D_igraph_dpow <- D_igraph^-0.5
  diag(D_igraph_dpow) <- 0

  D <- indirect_relations(g, type = "dist_sp")
  D_inv <- indirect_relations(g, type = "dist_sp", FUN = dist_inv)
  D_2pow <- indirect_relations(g, type = "dist_sp", FUN = dist_2pow)
  D_dpow <- indirect_relations(g, type = "dist_sp", FUN = dist_dpow, alpha = 0.5)

  expect_equal(D_igraph, D)
  expect_equal(D_igraph_inv, D_inv)
  expect_equal(2^-D_igraph, D_2pow)
  expect_equal(round(D_igraph_dpow, 7), round(D_dpow, 7))
})


test_that("walk counts are correct", {
  g <- make_empty_graph(n = 11, directed = FALSE)
  g <- add_edges(g, c(
    1, 11, 2, 4, 3, 5, 3, 11, 4, 8, 5, 9, 5, 11, 6, 7, 6, 8,
    6, 10, 6, 11, 7, 9, 7, 10, 7, 11, 8, 9, 8, 10, 9, 10
  ))

  spec_decomp <- eigen(as_adjacency_matrix(g, sparse = F))

  W_spec <- spec_decomp$vectors %*% diag(exp(spec_decomp$values)) %*%
    t(spec_decomp$vectors)
  W_exp <- indirect_relations(g, type = "walks", FUN = walks_exp)

  expect_equal(round(W_spec, 7), round(W_exp, 7))
})

test_that("walk distances are correct", {
  g <- make_star(4, mode = "undirected")
  expect_equal(indirect_relations(g, type = "dist_walk", dwparam = 1e-08), distances(g),
    tolerance = 1e-04
  )
  expect_error(indirect_relations(g, type = "dist_walk"))
})

test_that("resistance distance is correct", {
  g <- make_star(5, mode = "undirected")
  R <- indirect_relations(g, type = "dist_resist")
  D <- distances(g)
  expect_equal(R, D)
})

test_that("log forest distance is correct", {
  g <- make_full_graph(5)
  expect_equal(indirect_relations(g, type = "dist_lf", lfparam = 1e-10), distances(g),
    tolerance = 1e-03
  )
  expect_error(indirect_relations(g, type = "dist_lf"))
})

test_that("random walk distance is correct", {
  g <- make_full_graph(5)
  D <- indirect_relations(g, type = "dist_rwalk")
  exact <- matrix(4, 5, 5) - diag(4, 5)
  expect_equal(D, exact)
})

test_that("rspx is correct", {
  g <- make_star(5, "undirected")
  R <- round(indirect_relations(g, type = "depend_rsps", rspxparam = 50), 8)
  expect_equal(rowSums(R), c(12, 0, 0, 0, 0))
  set.seed(127)
  g <- sample_gnp(30, 0.4)
  test_cor <- cor(
    rowSums(indirect_relations(g, type = "depend_rspn", rspxparam = 0.001)),
    rowSums(indirect_relations(g, type = "depend_curflow"))
  )
  expect_equal(test_cor, 1)
})

test_that("walk transform is correct", {
  g <- make_full_graph(5, directed = FALSE)
  A <- indirect_relations(g, type = "walks", FUN = walks_uptok, alpha = 1, k = 2)
  expect_equal(A, matrix(4, 5, 5))

  katz <- rowSums(indirect_relations(g, type = "walks", FUN = walks_attenuated, alpha = 0.1))
  expect_equal(katz, rep(5 / 3, 5))

  even_and_odd <- indirect_relations(g, type = "walks", FUN = walks_exp_even) +
    indirect_relations(g, type = "walks", FUN = walks_exp_odd)
  all <- indirect_relations(g, type = "walks", FUN = walks_exp)
  expect_equal(even_and_odd, all)
})
