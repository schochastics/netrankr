context("probabilistic centrality")

test_that("exact_rank_prob is correct",{
  library(igraph)
  library(magrittr)
  P <- matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
  res <- exact_rank_prob(P)
  exp_rank <- c(4/3,19/9,26/9,4+2/9,4+4/9)
  
  expect_equal(res$lin.ext,9)
  expect_equal(round(res$expected.rank,6),round(exp_rank,6))
  
  #### 
  
  tg <- threshold_graph(20,0.2)
  P <- neighborhood_inclusion(tg)
  expect_warning(exact_rank_prob(P))
  
  #### 
  
  A <- matrix(1,10,10)
  diag(A) <- 0
  expect_warning(exact_rank_prob(A))
  
})

test_that("ideal lattice is correct",{
  library(igraph)
  library(magrittr)
  P <- matrix(c(0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,rep(0,10)),5,5,byrow=TRUE)
  res <- exact_rank_prob(P,only.results = FALSE)
  rks <- get_rankings(res)
  expect_equal(ncol(rks),9)
  expect_equal(length(res$lattice),11) #no of ideals
  expect_equal(length(unlist(res$lattice)),15) #no of edges in ideal lattice
  
})
