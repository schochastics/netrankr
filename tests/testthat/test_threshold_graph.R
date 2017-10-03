context("threshold graphs")

test_that("complete and star graph correct",{
  library(igraph)
  comp <- threshold_graph(10,1)
  star <- threshold_graph(10,0)
  star_igraph <- graph.star(10,"undirected")
  
  expect_equal(graph.density(comp),1) 
  expect_equal(max(degree(star)),9)
  expect_equal(graph.density(star),graph.density(star_igraph))
})

test_that("all pairs are comparable by ni",{
  library(igraph)
  tg <- threshold_graph(20,0.2)
  cp <- comparable_pairs(neighborhood_inclusion(tg))
  expect_equal(cp,1)
})

