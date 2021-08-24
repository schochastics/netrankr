context("helper functions")
library(igraph)
library(magrittr)
library(Matrix)
test_that("majorization gap is correct",{
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  
  expect_equal(round(majorization_gap(g),7),0.3529412)
  expect_equal(majorization_gap(g,norm = FALSE),6)
  
  tg <- threshold_graph(20,0.2)
  expect_equal(round(majorization_gap(tg),7),0)
  expect_equal(majorization_gap(tg,norm = FALSE),0)
  
  
})

test_that("spectral gap is correct",{
  g <- graph.star(10,"undirected")
  
  expect_equal(spectral_gap(g,method = "frac"),1)
  expect_equal(spectral_gap(g,method = "abs"),3)
  expect_error(spectral_gap(g,method = "hello"))
})

test_that("compare_ranks is correct",{

  tg <- threshold_graph(20,0.3)
  dc <- degree(tg)
  cc <- closeness(tg)
  res <- compare_ranks(dc,cc)
  expect_equal(res$discordant,0)
  expect_equal(res$right,0)
  expect_equal(res$left,0)
  expect_equal(res$concordant+res$ties,190)
  
  expect_error(compare_ranks(1:5,1:7))
})

test_that("transitive_reduction is correct",{

  P <- matrix(1,10,10)
  P[lower.tri(P,diag = TRUE)] <- 0
  T_red <- transitive_reduction(P)
  expect_equal(sum(T_red),9)
})

test_that("is_preserved is correct",{
  library(igraph)
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  
  P <- neighborhood_inclusion(g, sparse = FALSE)
  expect_equal(is_preserved(P,degree(g)),TRUE)
  expect_equal(is_preserved(P,closeness(g)),TRUE)
  expect_equal(is_preserved(P,betweenness(g)),TRUE)
  expect_equal(is_preserved(P,rowSums(P)),FALSE)
})


test_that("rank_intervals is correct",{
  library(igraph)
  library(magrittr)
  P <- matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
  res <- rank_intervals(P)
  
  expect_equal(res$min_rank,c(1,1,2,3,3))
  expect_equal(res$max_rank,c(2,4,4,5,5))
  expect_equal(res$mid_point,c(1.5,2.5,3.0,4.0,4.0))
  
})