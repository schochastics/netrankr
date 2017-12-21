context("build centrality indices")
library(igraph)
library(magrittr)
test_that("betweenness correct",{

  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  bc <- g %>% 
    indirect_relations("depend_sp") %>% 
    aggregate_positions()
  expect_equal(round(bc/2,4),round(betweenness(g),4))
})

test_that("closeness correct",{
  # library(igraph)
  # library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  cc <- g %>% 
    indirect_relations("dist_sp") %>% 
    aggregate_positions(type = "invsum")
  expect_equal(round(cc,4),round(closeness(g),4))
})

test_that("evcent correct",{
  # library(igraph)
  # library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  ec <- g %>% 
    indirect_relations("walks",FUN = walks_limit_prop) %>% 
    aggregate_positions(type = "sum")
  expect_equal(round(ec/max(ec),4),round(evcent(g)$vector,4))
})

test_that("subgraph centrality correct",{
  # library(igraph)
  # library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  sc <- g %>% 
    indirect_relations("walks",FUN = walks_exp) %>% 
    aggregate_positions(type = "self") %>% round(4)
  expect_equal(sc,round(subgraph_centrality(g),4))
})

test_that("current flow betweenness correct",{
  # library(igraph)
  # library(magrittr)

  g <- graph.empty(n=11,directed=FALSE)
  g <- add_edges(g,c(1,2,1,3,1,4,1,5,2,3,2,4,2,5,3,4,3,5,4,5))
  g <- add_edges(g,c(1,2,1,3,1,4,1,5,2,3,2,4,2,5,3,4,3,5,4,5)+5)
  g <- add_edges(g,c(5,11,6,11,5,6))  
  n <- vcount(g)
  cent <-  
    (indirect_relations(g,type = "depend_curflow", FUN = identity) +diag(n*(n-1),11)) %>%
    aggregate_positions(type = "sum")
  exact <- c(0.6703,0.6703,0.3333,0.2691,0.2691) #A,B,C,X,Y from Newman Paper
  expect_equal(round(cent[c(5,6,11,1,7)]/(11*10)-9/11,4),exact)
})

test_that("flow betweenness correct",{
  # library(igraph)
  # library(magrittr)
  g <- graph.empty(n=11,directed=FALSE)
  g <- add_edges(g,c(1,2,1,3,1,4,1,5,2,3,2,4,2,5,3,4,3,5,4,5))
  g <- add_edges(g,c(1,2,1,3,1,4,1,5,2,3,2,4,2,5,3,4,3,5,4,5)+5)
  g <- add_edges(g,c(5,11,6,11,5,6))  
  n <- vcount(g)
  exact <- c(12, 12, 12, 12, 118, 118, 12, 12, 12, 12, 50)
  cent <- g %>%
	indirect_relations(type = "depend_netflow", netflowmode = "raw", FUN = identity) %>%
	aggregate_positions(type = "sum")
  expect_equal(cent,exact)
})

test_that("communicability betweenness correct",{
  # library(igraph)
  # library(magrittr)
  g <- graph.empty(n=11,directed=FALSE)
  g <- add_edges(g,c(1,2,1,3,1,4,1,5,2,3,2,4,2,5,3,4,3,5,4,5))
  g <- add_edges(g,c(1,2,1,3,1,4,1,5,2,3,2,4,2,5,3,4,3,5,4,5)+5)
  g <- add_edges(g,c(5,11,6,11,5,6))  
  n <- vcount(g)
  exact <-c(0.2156, 0.2156, 0.2156, 0.2156, 0.6574, 0.6574, 0.2156, 0.2156, 
            0.2156, 0.2156, 0.1396)
  cent <- g %>%
    indirect_relations(type = "depend_exp", FUN = identity) %>%
    aggregate_positions(type = "sum")
  expect_equal(round(cent,4),exact)
})

test_that("all aggregate functions work",{
  tau_x <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
  expect_equal(aggregate_positions(tau_x,type = "prod"),c(2,12))
  expect_equal(aggregate_positions(tau_x,type = "mean"),c(1.5,3.5))
  expect_equal(aggregate_positions(tau_x,type = "max"),c(2,4))
  expect_equal(aggregate_positions(tau_x,type = "min"),c(1,3))
  expect_error(aggregate_positions(tau_x,type = "other"))
})