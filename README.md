
netrankr <img src="logo.png" align="right" height="auto" width="125" />
=======================================================================

Overview
--------

netrankr is an R package that can be used for centrality analyses of networks. Although it implements some indices, the main focus lies on an index-free assessment of centrality in networks. Most implemented methods are, however, more general and can be used whenever partial rankings have to be analysed.

Install
-------

``` r
require(devtools)
install_github("schochastics/netrankr")
```

Details
-------

Some features of the package are:

-   Working with the neighborhood inclusion preorder. This forms the bases for any centrality analysis on undirected and unweighted graphs. More details can be found in the dedicated vignette: `vignette("neighborhood_inclusion",package="netrankr")`.
-   Constructing graphs with a unique centrality ranking. This class of graphs, known as threshold graphs, can be used to benchmark centrality indices, since they only allow for one ranking of the nodes. For more details consult the vignette: `vignette("threshold_graph",package="netrankr")`.
-   Probabilistic ranking methods. The package includes several function to perform probabilistic rank analyses of nodes in a network. These include expected ranks and relative rank probabilities (how likely is it that a node is more central than another). An extensive example is given at the end of this document.

To browse all vignettes use: `browseVignettes(package = "netrankr")`

Notable functions
-----------------

-   `neighborhood_inclusion` and `positional_dominance` can be used to construct partial orders on a network. While `neighborhood_inclusion` is very specific (undirected, unweighted networks), `positional_dominance` can be used with any kind of input network. If cost variables (e.g. distances) are used, set `benefit=FALSE`. If actor identities don't matter set `map=TRUE`. Consult the respective vignettes for more detailed explanations.
-   `threshold_graphs` constructs a random uniquely ranked graph. That is, a graph where all centrality indices yield the same ranking.
-   `exact_rank_prob` performs a complete and exact rank analysis of a network, including expected ranks and relative rank probabilities (how likely is it that a node is more central than another?) as well as the number of possible centrality rankings. For larger networks, various approximation functions can be used. See vignettes for help.

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
   degree=degree(g),
   betweenness=round(betweenness(g),4),
   closeness=round(closeness(g),4),
   eigenvector=round(eigen_centrality(g)$vector,4),
   subgraph=round(subgraph_centrality(g),4))

apply(cent_scores,2,which.max)
```

    ##      degree betweenness   closeness eigenvector    subgraph 
    ##          11           8           6           7          10

Each index assigns the highest value to a different vertex!

More generic approach via neighborhood-inclusion.

``` r
P <- neighborhood_inclusion(g)
P
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
    ##  [1,]    0    0    1    0    1    1    1    0    0     0     1
    ##  [2,]    0    0    0    1    0    0    0    1    0     0     0
    ##  [3,]    0    0    0    0    1    0    0    0    0     0     1
    ##  [4,]    0    0    0    0    0    0    0    0    0     0     0
    ##  [5,]    0    0    0    0    0    0    0    0    0     0     0
    ##  [6,]    0    0    0    0    0    0    0    0    0     0     0
    ##  [7,]    0    0    0    0    0    0    0    0    0     0     0
    ##  [8,]    0    0    0    0    0    0    0    0    0     0     0
    ##  [9,]    0    0    0    0    0    0    0    0    0     0     0
    ## [10,]    0    0    0    0    0    0    0    0    0     0     0
    ## [11,]    0    0    0    0    0    0    0    0    0     0     0

``` r
D <- dominance_graph(P)
# plot(D)
```

If `P[u,v]=1` or equivalently `(u,v)` is an edge in `D`, then *N*(*u*)⊆*N*\[*v*\] holds, which implies that *c*(*u*)≤*c*(*v*) for all centrality indices *c*!

Neighborhood-inclusion defines a partial ranking on the set of nodes. Each ranking that is in accordance with this partial ranking defines a proper centrality ranking. Each of these ranking can potentially be the outcome of a centrality index.

The function `exact_rank_prob()` can be used to calculate all these ranking and produce probabilistic centrality rankings.

``` r
res <- exact_rank_prob(P)
str(res)
```

    ## List of 7
    ##  $ lin.ext      : num 739200
    ##  $ names        : chr [1:11] "1" "2" "3" "4" ...
    ##  $ mse          : int [1:11] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ rank.prob    : num [1:11, 1:11] 0.545 0.273 0 0 0 ...
    ##  $ relative.rank: num [1:11, 1:11] 0 0.3333 0 0.0476 0 ...
    ##  $ expected.rank: num [1:11] 1.71 3 4.29 7.5 8.14 ...
    ##  $ rank.spread  : num [1:11] 0.958 1.897 1.725 2.54 2.16 ...

`lin.ext` is the number of possible rankings. For the graph `g` we could thus come up with 739,200 indices that would rank the nodes differently.

`rank.prob` contains the probabilities for each node to occupy a certain rank. For instance, the probability for each node to be the most central one is as follows.

``` r
round(res$rank.prob[,11],2)
```

    ##  [1] 0.00 0.00 0.00 0.14 0.16 0.11 0.11 0.14 0.09 0.09 0.16

`relative.rank` contains the relative rank probabilities. An entry `relative.rank[u,v]` indicates how likely it is that `v` is more central than `u`.

``` r
# How likely is it, that 6 is more central than 3?
round(res$relative.rank[3,6],2)
```

    ## [1] 0.75

`expected.ranks` contains the expected centrality ranks for all nodes. They are derived on the basis of `rank.prob`.

``` r
round(res$expected.rank,2)
```

    ##  [1] 1.71 3.00 4.29 7.50 8.14 6.86 6.86 7.50 6.00 6.00 8.14

The higher the value, the more central a node is expected to be.

**Note**: The set of rankings grows exponentially in the number of nodes. The exact calculation thus becomes infeasible quite quickly. The package also implements a great variety of approximation methods for larger networks. Check the manual for options.
