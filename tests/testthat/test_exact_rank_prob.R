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
  
  
})