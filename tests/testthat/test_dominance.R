context("dominance")

test_that("neighborhood-inclusion is correct",{
  library(igraph)
  library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  A <- matrix(0,11,11)
  A[c(23, 35, 45, 47, 56, 67, 79, 111, 113)] <- 1
  
  expect_equal(neighborhood_inclusion(g),A)
  
  #### 
  
  tg <- threshold_graph(10,0)
  A <- matrix(1,10,10)
  diag(A) <- 0
  A[10,] <- 0
  expect_equal(neighborhood_inclusion(tg),A)
  
})

test_that("positional dominance is correct",{
  library(igraph)
  library(magrittr)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  
  D <- g %>% 
    indirect_relations() %>% 
    positional_dominance(map = FALSE, benefit = FALSE)
  
  Dmap <- g %>% 
    indirect_relations() %>% 
    positional_dominance(map = TRUE, benefit = FALSE)
  
  A <- matrix(0,11,11)
  A[c(23, 35, 45, 47, 56, 67, 79, 111, 113)] <- 1
  Amap <- matrix(0,11,11)
  Amap[c(2, 23, 24, 34, 35, 45, 46, 47, 48, 56, 57, 58, 59, 60,
         62, 63, 64, 65, 67, 68, 69, 70, 71, 78, 79, 80, 81, 82, 
         84, 86, 87, 89, 90, 91, 92, 93, 95, 96, 98, 100, 101, 102, 
         103, 111, 112, 113, 114, 115, 117)] <- 1
  
  expect_equal(D,A)
  expect_equal(Dmap,Amap)
})
