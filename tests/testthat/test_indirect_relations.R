context("indirect relations")

test_that("distance based relations are correct",{
  library(igraph)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  
  D_igraph <- distances(g)
  D_igraph_inv <- D_igraph^(-1)
  diag(D_igraph_inv) <- 0
  D_igraph_dpow <- D_igraph^-0.5
  diag(D_igraph_dpow) <- 0
  
  D <- indirect_relations(g, type = "geodesic")
  D_inv <- indirect_relations(g, type = "geodesic",FUN = dist_inv)
  D_2pow <- indirect_relations(g, type = "geodesic",FUN = dist_2pow)
  D_dpow <- indirect_relations(g, type = "geodesic",FUN = dist_dpow,alpha = 0.5)
  
  expect_equal(D_igraph,D)
  expect_equal(D_igraph_inv,D_inv)
  expect_equal(2^-D_igraph,D_2pow)
  expect_equal(round(D_igraph_dpow,7),round(D_dpow,7))
})


test_that("walks are correct",{
  library(igraph)
  
  g <- graph.empty(n=11,directed = FALSE)
  g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                     6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
  
  spec_decomp <- eigen(get.adjacency(g,sparse=F))
  
  W_spec <- spec_decomp$vectors %*% diag(exp(spec_decomp$values)) %*% 
            t(spec_decomp$vectors)
  W_exp <- indirect_relations(g, type = "walks",FUN = walks_exp)
  
  expect_equal(round(W_spec,7),round(W_exp,7))
  
  
})
