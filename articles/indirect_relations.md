# Indirect relations in networks

This vignette describes the importance of indirect relations on
networks, how they are used in centrality indices and how they are
implemented in the `netrankr` package.

------------------------------------------------------------------------

## Theoretical Background

A one-mode network can be described as a *dyadic variable*
$`x\in \mathcal{W}^\mathcal{D}`$, where $`\mathcal{W}`$ is the value
range of the network (in the simple case of unweighted networks
$`\mathcal{W}=\{0,1\}`$) and
$`\mathcal{D}=\mathcal{N}\times\mathcal{N}`$ describes the dyadic domain
of actors $`\mathcal{N}`$.  
  
Observed presence or absence of ties (the value range is binary) is
usually not the relation of interest for network analytic tasks.
Instead, mostly implicitly, relations are *transformed* into a new set
of *indirect* relations on the basis of the *observed* relations. As an
example, consider (shortest path) distances in the underlying graph.
While they are fairly easy to derive from an observed network of
contacts, it is impossible for actors in a network to answer the
question “How far away are you from others you are not connected with?”.
We denote generic transformed networks from an observed network $`x`$ as
$`\tau(x)`$.  
  

With this notion of indirect relations, we can express centrality
indices in a common framework as
``` math
c_\tau(i)=\sum\limits_{t \in \mathcal{N}} \tau(x)_{it}
```
Degree and closeness centrality, for instance, can be obtained by
setting $`\tau=id`$ and $`\tau=dist`$, respectively. Others need several
additional specifications which can be found in [Brandes
(2016)](https://dx.doi.org/10.1177/2059799116630650) or [Schoch &
Brandes (2016)](https://doi.org/10.1017/S0956792516000401).  
With this framework, we can characterize centrality indices as
degree-like measures in a suitably transformed network $`\tau(x)`$.

------------------------------------------------------------------------

## Indirect relations in the `netrankr` package

``` r

library(netrankr)
library(igraph)
```

The `netrankr` package implements a great variety of indirect relations
that are (or could be) used for centrality related considerations in a
network. All indirect relations can be computed with the
[`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
function, by specifying the `type` parameter.

``` r

data("dbces11")
g <- dbces11

# adjacency
A <- indirect_relations(g, type = "adjacency")
# shortest path distances
D <- indirect_relations(g, type = "dist_sp")
# dyadic dependencies (as used in betweenness centrality)
B <- indirect_relations(g, type = "depend_sp")
# resistance distance (as used in information centrality)
R <- indirect_relations(g, type = "dist_resist")
# Logarithmic forest distance (parametrized family of distances)
LF <- indirect_relations(g, type = "dist_lf", lfparam = 1)
# Walk distance (parametrized family of distances)
WD <- indirect_relations(g, type = "dist_walk", dwparam = 0.001)
# Random walk distance
WD <- indirect_relations(g, type = "dist_rwalk")
# See ?indirect_relations for further options
```

Indirect relations are represented as matrices, similar to the adjacency
matrix. The below matrices show the distance matrix based on sahortest
paths, and the pairwise dependencies (used for e.g. betweenness).

``` r

D
```

    ##   A B C D E F G H I J K
    ## A 0 5 2 4 2 2 2 3 3 3 1
    ## B 5 0 5 1 4 3 4 2 3 3 4
    ## C 2 5 0 4 1 2 2 3 2 3 1
    ## D 4 1 4 0 3 2 3 1 2 2 3
    ## E 2 4 1 3 0 2 2 2 1 2 1
    ## F 2 3 2 2 2 0 1 1 2 1 1
    ## G 2 4 2 3 2 1 0 2 1 1 1
    ## H 3 2 3 1 2 1 2 0 1 1 2
    ## I 3 3 2 2 1 2 1 1 0 1 2
    ## J 3 3 3 2 2 1 1 1 1 0 2
    ## K 1 4 1 3 1 1 1 2 2 2 0

``` r

B
```

    ##     A         B         C         D   E         F   G         H         I
    ## A 0.0 0.0000000 0.0000000 0.0000000 0.0 0.0000000 0.0 0.0000000 0.0000000
    ## B 0.0 0.0000000 0.0000000 0.0000000 0.0 0.0000000 0.0 0.0000000 0.0000000
    ## C 0.0 0.0000000 0.0000000 0.0000000 0.0 0.0000000 0.0 0.0000000 0.0000000
    ## D 1.0 9.0000000 1.0000000 0.0000000 1.0 1.0000000 1.0 1.0000000 1.0000000
    ## E 0.5 0.5000000 2.8333333 0.5000000 0.0 0.0000000 0.0 0.5000000 2.0000000
    ## F 3.5 2.8333333 1.8333333 2.8333333 0.0 0.0000000 1.0 2.8333333 0.0000000
    ## G 1.0 0.0000000 0.3333333 0.0000000 0.0 0.3333333 0.0 0.0000000 1.3333333
    ## H 2.0 8.0000000 2.0000000 8.0000000 2.0 2.3333333 2.0 0.0000000 2.3333333
    ## I 0.0 1.8333333 1.8333333 1.8333333 4.5 0.0000000 1.5 1.8333333 0.0000000
    ## J 0.0 0.3333333 0.0000000 0.3333333 0.0 0.3333333 1.0 0.3333333 0.3333333
    ## K 9.0 1.5000000 5.1666667 1.5000000 2.5 3.0000000 2.5 1.5000000 1.0000000
    ##           J   K
    ## A 0.0000000 0.0
    ## B 0.0000000 0.0
    ## C 0.0000000 0.0
    ## D 1.0000000 1.0
    ## E 0.3333333 0.5
    ## F 1.3333333 3.5
    ## G 1.3333333 1.0
    ## H 2.0000000 2.0
    ## I 1.3333333 0.0
    ## J 0.0000000 0.0
    ## K 1.6666667 0.0

The function takes an additional parameter `FUN` which can be used to
pass a function to further transform relations. The main use is to
obtain indirect relations based on walk counts.

``` r

# count the limit proportion of walks (used for eigenvector centrality)
W <- indirect_relations(g, type = "walks", FUN = walks_limit_prop)
# count the number of walks of arbitrary length between nodes, weighted by
# the inverse factorial of their length (used for subgraph centrality)
S <- indirect_relations(g, type = "walks", FUN = walks_exp)
```

Additional parameters can also be passed to calculate parameterized
versions of relations.

``` r

# Calculate dist(s,t)^-alpha
D <- indirect_relations(g, type = "dist_sp", FUN = dist_dpow, alpha = 2)
```

To view all predefined transformation functions see
[`?transform_relations`](https://schochastics.github.io/netrankr/reference/transform_relations.md).
The predefined functions follow the naming scheme
`<relation>_<transformation>`. The functions `dist_` are thus only
meaningful fordistance type relations such as `type="dist_sp"` or
`type="dist_resist"`. Equivalently, `walks_` for `type="walks"`. The
predefined functions are not exhaustive and just constitute the most
common transformations. It is, however, straightforward to pass your own
transformation function.

``` r

dist_integration <- function(x) {
    x <- 1 - (x - 1) / max(x)
}
D <- indirect_relations(g, type = "dist_sp", FUN = dist_integration)
```

The function `dist_integration()` computes
``` math
\tau(x)_{ij}=1-\frac{dist(i,j)-1}{max_{i,j}\; dist(i,j)}
```
which is used in the centrality index *integration* defined by [Valente
and Foreman (1998)](https://doi.org/10.1016/S0378-8733(97)00007-5)

The computed relations CAN be used to build centrality indices
(e.g. with the provided Rstudio
[`index_builder()`](https://schochastics.github.io/netrankr/reference/index_builder.md)),
but also to derive partial rankings with
[`positional_dominance()`](https://schochastics.github.io/netrankr/reference/positional_dominance.md).
Consult the respective
[vignette](https://schochastics.github.io/netrankr/articles/positional_dominance.md)
for help.
