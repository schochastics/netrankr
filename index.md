
<img src="logo.png" align="right" height="auto" width="125"/>

netrankr: Centrality (almost) without Indices
---------------------------------------------

The literature is flooded with [centrality indices](https://en.wikipedia.org/wiki/Centrality) and new ones are still introduced on a regular basis. Just take a look at the [periodic table](http://schochastics.net/sna/periodic.html) of network centrality to get an overview of the variety of indices. Although there exist several theoretical and empirical guidelines on when to use certain indices, there still exists plenty of ambiguity in the concept of network centrality.

The `netrankr` package introduces alternative and complementary methods to the index-driven assessment of centrality. See the

From <!--html_preserve-->

<script type="application/json" data-for="htmlwidget-0168982c03be55bede2f">{"x":{"diagram":"\n graph LR\n A-->B\n"},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
to

Simple Example
--------------

This example briefly explains some of the functionality of the package and the difference to an index driven approach. For a more realistic application see the [use case](use_case.html) example.

We work with the following small graph.

``` r
library(igraph)
library(netrankr)

g <- graph.empty(n = 11,directed = FALSE)
g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
```

<img src="dbces-neutral.png" width="100%" style="display: block; margin: auto;" />

Say we are interested in the most central node of the graph and simply compute some standard centrality scores with the `igraph` package.

``` r
cent_scores <- data.frame(
   degree = degree(g),
   betweenness = round(betweenness(g),4),
   closeness = round(closeness(g),4),
   eigenvector = round(eigen_centrality(g)$vector,4),
   subgraph = round(subgraph_centrality(g),4))
```

    #> Warning in eigen_centrality(g): '.Random.seed' is not an integer vector but
    #> of type 'NULL', so ignored

``` r
# What are the most central nodes for each index?
apply(cent_scores,2,which.max)
```

    #>      degree betweenness   closeness eigenvector    subgraph 
    #>          11           8           6           7          10

<img src="dbces-color.png" width="100%" style="display: block; margin: auto;" />

As you can see, each index assigns the highest value to a different vertex.

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

Installation
------------

To install from CRAN: \[not yet published\]

To install the developer version from github:

``` r
require(devtools)
install_github("schochastics/netrankr")
```

Theoretical Background
----------------------

`netrankr` is based on a series of papers that appeared in recent years. If you want to learn more about the theoretical background of the package, consult the following literature:

> Brandes, Ulrik. (2016). Network Positions. *Methodological Innovations*, **9**, 2059799116630650. ([link](http://dx.doi.org/10.1177/2059799116630650))

> Schoch, David & Brandes, Ulrik. (2016). Re-conceptualizing centrality in social networks. *European Journal of Appplied Mathematics*, **27**(6), 971–985. ([link](https://doi.org/10.1017/S0956792516000401))

> Schoch, David & Valente, Thomas W., & Brandes, Ulrik. (2017). Correlations among centrality indices and a class of uniquely ranked graphs. *Social Networks*, **50**, 46-54.([link](http://doi.org/10.1016/j.socnet.2017.03.010))

> Schoch, David. (2017). Centrality without Indices: Partial Rankings and Rank Probabilities in Networks. *under review*
