
<!-- [![CRAN Status Badge](http://www.r-pkg.org/badges/version/netrankr)](https://cran.r-project.org/package=netrankr) -->
<!-- [![CRAN Downloads Per Month](http://cranlogs.r-pkg.org/badges/netrankr)](https://CRAN.R-project.org/package=netrankr) -->
Overview
--------

netrankr is an R package to analyze partial rankings in the context of networks centrality. While the package includes the possibility to build a variety of indices, its main focus lies on index-free assessment of centrality. Computed partial rankings can be analyzed with a variety of methods. These include probabilistic methods like computing expected node ranks and relative rank probabilities (how likely is it that a node is more central than another?).

Most implemented methods are very general and can be used whenever partial rankings have to be analysed.

Install
-------

To install from CRAN: \[not yet published\]

To install the developer version from github:

``` r
require(devtools)
install_github("schochastics/netrankr")
```

Details
-------

The core functions of the package are:

-   Computing the neighborhood inclusion preorder with `neighborhood_inclusion()`. The resulting partial ranking is the foundation for any centrality related analysis on undirected and unweighted graphs. More details can be found in the dedicated vignette: `vignette("neighborhood_inclusion",package="netrankr")`.
    A generalizded version of neighborhood inclusion is implemented in `positional_dominance()`. See `vignette("positional_dominance",package="netrankr")` for help. Any partial ranking computed with the mentioned functions form the basis

-   Constructing graphs with a unique centrality ranking with `threshold_graph()`. This class of graphs, known as threshold graphs, can be used to benchmark centrality indices, since they only allow for one ranking of the nodes. For more details consult the vignette: `vignette("threshold_graph",package="netrankr")`.

-   Computing probabilistic centrality rankings. The package includes several function to calculate rank probabilities of nodes in a network, including expected ranks (how central do we expect a node to be?) and relative rank probabilities (how likely is it that a node is more central than another?). These probabilities can either be computed exactly for small networks (`exact_rank_prob()`), based on an almost uniform sample (`mcmc_rank_prob()`) or approximated via several heuristics (`approx_rank_expected()`,`approx_rank_relative()`). Consult `vignette('probabilistic_cent',package='netrankr')` for more information and `vignette('benchmarks',package='netrankr')` for applicability.

-   Although the focus of the package lies on an index-free assessement of centrality, the package provides the possibility to build a variety of indices. Consult `vignette('centrality_indices',package='netrankr')` for more information.

The package includes several additional vignettes, which can be viewed with `browseVignettes(package = "netrankr")` or [online](http://schochastics.github.io/netrankr)

Example
-------

``` r
library(igraph)
library(netrankr)
set.seed(123)
g <- graph.empty(n=11,directed = FALSE)
g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
# plot(g)
```

Calculate centrality scores with the `igraph` package.

``` r
cent_scores <- data.frame(
   degree = degree(g),
   betweenness = round(betweenness(g),4),
   closeness = round(closeness(g),4),
   eigenvector = round(eigen_centrality(g)$vector,4),
   subgraph = round(subgraph_centrality(g),4))

# What are the most central nodes for each index?
apply(cent_scores,2,which.max)
```

    #>      degree betweenness   closeness eigenvector    subgraph 
    #>          11           8           6           7          10

Each index assigns the highest value to a different vertex!

Calculate the neighborhood inclusion preorder.

``` r
P <- neighborhood_inclusion(g)
P
```

    #>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
    #>  [1,]    0    0    1    0    1    1    1    0    0     0     1
    #>  [2,]    0    0    0    1    0    0    0    1    0     0     0
    #>  [3,]    0    0    0    0    1    0    0    0    0     0     1
    #>  [4,]    0    0    0    0    0    0    0    0    0     0     0
    #>  [5,]    0    0    0    0    0    0    0    0    0     0     0
    #>  [6,]    0    0    0    0    0    0    0    0    0     0     0
    #>  [7,]    0    0    0    0    0    0    0    0    0     0     0
    #>  [8,]    0    0    0    0    0    0    0    0    0     0     0
    #>  [9,]    0    0    0    0    0    0    0    0    0     0     0
    #> [10,]    0    0    0    0    0    0    0    0    0     0     0
    #> [11,]    0    0    0    0    0    0    0    0    0     0     0

If `P[u,v]=1`, then *N*(*u*)⊆*N*\[*v*\] which implies that *c*(*u*)≤*c*(*v*) for all centrality indices *c*!

Neighborhood-inclusion defines a partial ranking on the set of nodes. Each ranking that is in accordance with this partial ranking yields a proper centrality ranking. Each of these ranking can thus potentially be the outcome of a centrality index.

The function `exact_rank_prob()` can be used to calculate all these ranking and produce probabilistic centrality rankings.

``` r
res <- exact_rank_prob(P)
str(res)
```

    #> List of 7
    #>  $ lin.ext      : num 739200
    #>  $ names        : chr [1:11] "1" "2" "3" "4" ...
    #>  $ mse          : int [1:11] 1 2 3 4 5 6 7 8 9 10 ...
    #>  $ rank.prob    : num [1:11, 1:11] 0.545 0.273 0 0 0 ...
    #>  $ relative.rank: num [1:11, 1:11] 0 0.3333 0 0.0476 0 ...
    #>  $ expected.rank: num [1:11] 1.71 3 4.29 7.5 8.14 ...
    #>  $ rank.spread  : num [1:11] 0.958 1.897 1.725 2.54 2.16 ...

`lin.ext` is the number of possible rankings. For the graph `g` we could therefore come up with 739,200 indices that would rank the nodes differently.

`rank.prob` contains the probabilities for each node to occupy a certain rank. For instance, the probability for each node to be the most central one is as follows.

``` r
round(res$rank.prob[ ,11],2)
```

    #>  [1] 0.00 0.00 0.00 0.14 0.16 0.11 0.11 0.14 0.09 0.09 0.16

`relative.rank` contains the relative rank probabilities. An entry `relative.rank[u,v]` indicates how likely it is that `v` is more central than `u`.

``` r
# How likely is it, that 6 is more central than 3?
round(res$relative.rank[3,6],2)
```

    #> [1] 0.75

`expected.ranks` contains the expected centrality ranks for all nodes. They are derived on the basis of `rank.prob`.

``` r
round(res$expected.rank,2)
```

    #>  [1] 1.71 3.00 4.29 7.50 8.14 6.86 6.86 7.50 6.00 6.00 8.14

The higher the value, the more central a node is expected to be.

**Note**: The set of rankings grows exponentially in the number of nodes and the exact calculation becomes infeasible quite quickly and approximations need to be used.
Check the vignettes for help and further package functionality.
