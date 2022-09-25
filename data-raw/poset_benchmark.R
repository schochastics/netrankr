library(igraph)
library(netrankr)
library(progress)
library(tidyverse)
library(rgraph6)

# load all benchmark posets in digraph6 format
posets <- readLines("data-raw/posets.txt") 

res_all <- data.frame(id = seq_len(length(posets)),
                  n = NA,
                  density = NA,
                  runtime_exact = 0,
                  runtime_mcmc = 0,
                  err_approx_exp_lpom  = 0,
                  err_approx_exp_glpom = 0,
                  err_approx_exp_loof1 = 0,
                  err_approx_exp_loof2 = 0,
                  err_approx_exp_mcmc  = 0,
                  err_approx_rel_it1 = 0,
                  err_approx_rel_it5 = 0,
                  err_approx_rel_it10 = 0,
                  err_approx_rel_it15 = 0,
                  err_approx_rel_mcmc = 0,
                  rtime_approx_exp_lpom  = 0,
                  rtime_approx_exp_glpom = 0,
                  rtime_approx_exp_loof1 = 0,
                  rtime_approx_exp_loof2 = 0,
                  rtime_approx_rel_it1 = 0,
                  rtime_approx_rel_it5 = 0,
                  rtime_approx_rel_it10 = 0,
                  rtime_approx_rel_it15 = 0,
                  dgraph6 = posets)

total <- length(posets)

pb <- progress_bar$new(
  format = "[:bar] (:current/:total) eta: :eta",
  total = total, clear = FALSE, width= 100)

for(k in 1:total){
  pb$tick()
  po <- posets[k]
  A <- rgraph6::adjacency_from_digraph6(po)[[1]]
  res_all$n[k] <- nrow(A)
  res_all$density[k] <- comparable_pairs(A)
  if(res_all$density[k]==1){
    next()
  }
  #exact
  res_all$runtime_exact[k] <- system.time(res_exact <- exact_rank_prob(A))[3]
  #mcmc
  res_all$runtime_mcmc[k] <- system.time(res_mcmc <- netrankr::mcmc_rank_prob(A,nrow(A)^5))[3]
  #approx expected
  res_all$rtime_approx_exp_lpom[k] <- system.time(lpom  <- approx_rank_expected(A, method="lpom"))[3]
  res_all$rtime_approx_exp_glpom[k] <- system.time(glpom <- approx_rank_expected(A, method="glpom"))[3]
  res_all$rtime_approx_exp_loof1[k] <- system.time(loof1 <- approx_rank_expected(A, method="loof1"))[3]
  res_all$rtime_approx_exp_loof2[k] <- system.time(loof2 <- approx_rank_expected(A, method="loof2"))[3]
  #approx relative
  res_all$rtime_approx_rel_it1[k] <- system.time(rel_it1 <- approx_rank_relative(A, iterative = FALSE))[3]
  res_all$rtime_approx_rel_it5[k] <- system.time(rel_it5 <- approx_rank_relative(A, iterative = TRUE,num.iter = 5))[3]
  res_all$rtime_approx_rel_it10[k] <- system.time(rel_it10 <-approx_rank_relative(A, iterative = TRUE,num.iter = 10) )[3]
  res_all$rtime_approx_rel_it15[k] <- system.time(rel_it15 <- approx_rank_relative(A, iterative = TRUE,num.iter = 15))[3]
  
  res_all$err_approx_exp_mcmc[k] <- mean(abs(res_exact$expected.rank-res_mcmc$expected.rank))
  res_all$err_approx_exp_lpom[k] <- mean(abs(res_exact$expected.rank-lpom))
  res_all$err_approx_exp_glpom[k] <- mean(abs(res_exact$expected.rank-glpom))
  res_all$err_approx_exp_loof1[k] <- mean(abs(res_exact$expected.rank-loof1))
  res_all$err_approx_exp_loof2[k] <- mean(abs(res_exact$expected.rank-loof2))
  res_all$err_approx_rel_mcmc[k] <- mean(abs(res_exact$relative.rank-res_mcmc$relative.rank))
  res_all$err_approx_rel_it1[k] <- mean(abs(res_exact$relative.rank-rel_it1))
  res_all$err_approx_rel_it5[k] <- mean(abs(res_exact$relative.rank-rel_it5))
  res_all$err_approx_rel_it10[k] <- mean(abs(res_exact$relative.rank-rel_it10))
  res_all$err_approx_rel_it15[k] <- mean(abs(res_exact$relative.rank-rel_it15))
}
write_csv(res_all,"data-raw/poset_results.csv")

df <- read_csv("data-raw/poset_results.csv")

# plot runtime of exact algorithm
ggplot(df,aes(x=1-density,y=runtime_exact,col=as.factor(n)))+
  geom_point()+ 
  scale_color_manual(values = c("#1d3557", "#457b9d", "#432818"),name="no. of elements")+
  scale_y_log10()+theme(legend.position = "bottom")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x="density of preorder",y="runtime (in sec)")

ggsave("vignettes/runtimes_exact.png",width = 8,height = 6)

# plot error of expected rank approximations
df %>% 
  select(n,density,contains("err_approx_exp")) %>% 
  pivot_longer(err_approx_exp_lpom:err_approx_exp_mcmc) %>% 
  mutate(name=str_remove_all(name,"err_approx_exp_")) %>% 
  mutate(name=factor(name,levels=c("lpom","glpom","loof1","loof2","mcmc"))) %>% 
  mutate(n = paste0("n = ",n)) %>% 
  ggplot(aes(x=density,y=value,col=name))+
  # geom_point(alpha=0.1)+ otherwise too big file size
  geom_smooth(method="loess",se=FALSE)+
  scale_color_manual(values=c("#1d3557", "#457b9d", "#432818","#99582a", "#e63946"),name="")+
  facet_wrap(~n,ncol=3)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x="density of preorder",y="mean absolute error")

ggsave("vignettes/quality_expected_mse.png",width = 8,height = 6)

# plot error of relative rank approximations
df %>% 
  select(n,density,contains("err_approx_rel")) %>% 
  pivot_longer(err_approx_rel_it1:err_approx_rel_mcmc) %>% 
  mutate(n = paste0("n = ",n)) %>% 
  mutate(name=str_remove_all(name,"err_approx_rel_")) %>% 
  mutate(name=case_when(name=="it1" ~ "no iteration",
                        name=="it5" ~ "5 iterations",
                        name=="it10" ~ "10 iterations",
                        name=="it15" ~ "15 iterations",
                        name=="mcmc" ~ "mcmc")) %>% 
  mutate(name=factor(name,levels = c("no iteration","5 iterations","10 iterations","15 iterations","mcmc"))) %>% 
  ggplot(aes(x=density,y=value,col=name))+
  # geom_point(alpha=0.1)+ otherwise too big file size
  geom_smooth(method="loess",se=FALSE)+
  scale_color_manual(values=c("#1d3557", "#457b9d", "#a8dadc", "#d8e1d6", "#e63946"),name="")+
  facet_wrap(~n,ncol=3)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x="density of preorder",y="mean absolute error")

ggsave("vignettes/quality_relative_mse.png",width=8,height=6)

# benchmarking the infuence of the number of samples for the mcmc method
exp_rks <- rep(5.5,10)
rel_rk <- 0.5
mcmc_mae_exp <- data.frame(
  mae = c(
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 10)$expected.rank-exp_rks)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 100)$expected.rank-exp_rks)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 1000)$expected.rank-exp_rks)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 10000)$expected.rank-exp_rks)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 100000)$expected.rank-exp_rks))))),
  samples = rep(c(10,100,1000,10000,100000),each=100)
)

ggplot(mcmc_mae_exp,aes(x=format(samples,big.mark=",",scientific = FALSE),y=mae))+
  geom_boxplot()+
  theme_minimal()+
  labs(x="no. of samples",y="mean absolute error")

ggsave("vignettes/mcmc_samples_exp.png",width = 8,height=6)

mcmc_mae_rel <- data.frame(
  mae = c(
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 10)$relative.rank-rel_rk)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 100)$relative.rank-rel_rk)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 1000)$relative.rank-rel_rk)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 10000)$relative.rank-rel_rk)))),
    replicate(100,mean(abs((mcmc_rank_prob(P = matrix(0,10,10),rp = 100000)$relative.rank-rel_rk))))),
  samples = rep(c(10,100,1000,10000,100000),each=100)
)

ggplot(mcmc_mae_rel,aes(x=format(samples,big.mark=",",scientific = FALSE),y=mae))+
  geom_boxplot()+
  theme_minimal()+
  labs(x="no. of samples",y="mean absolute error")

ggsave("vignettes/mcmc_samples_rel.png",width = 8,height=6)

