context("build centrality indices")

test_that("betweenness correct",{
  library(igraph)
  library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  bc <- g %>% 
    indirect_relations("dependencies") %>% 
    aggregate_positions()
  expect_equal(round(bc/2,4),round(betweenness(g),4))
})

test_that("closeness correct",{
  library(igraph)
  library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  cc <- g %>% 
    indirect_relations("geodesic") %>% 
    aggregate_positions(type = "invsum")
  expect_equal(round(cc,4),round(closeness(g),4))
})

test_that("evcent correct",{
  library(igraph)
  library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  ec <- g %>% 
    indirect_relations("walks",FUN = walks_limit_prop) %>% 
    aggregate_positions(type = "sum")
  expect_equal(round(ec/max(ec),4),round(evcent(g)$vector,4))
})

test_that("subgraph centrality correct",{
  library(igraph)
  library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  sc <- g %>% 
    indirect_relations("walks",FUN = walks_exp) %>% 
    aggregate_positions(type = "self") %>% round(4)
  expect_equal(sc,round(subgraph_centrality(g),4))
})

test_that("communicability centrality correct",{
  library(igraph)
  library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  comc <- g %>% 
    indirect_relations("walks",FUN = walks_exp) %>% 
    aggregate_positions(type = "sum")
  res <- 
  expect_equal(sc,subgraph_centrality(g))
})