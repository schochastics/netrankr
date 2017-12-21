context("threshold graphs")
library(igraph)

test_that("complete and star graph correct",{
  comp <- threshold_graph(10,1)
  star <- threshold_graph(10,0)
  star_igraph <- graph.star(10,"undirected")
  
  expect_equal(graph.density(comp),1) 
  expect_equal(max(degree(star)),9)
  expect_equal(graph.density(star),graph.density(star_igraph))
})

test_that("all pairs are comparable by ni",{
  tg <- threshold_graph(20,0.2)
  cp <- comparable_pairs(neighborhood_inclusion(tg))
  expect_equal(cp,1)
})

test_that("error handling is correct",{
  testthat::expect_error(threshold_graph(p=0.4))
  testthat::expect_error(threshold_graph(n=10))
})
