# Neighborhood-inclusion preorder

Calculates the neighborhood-inclusion preorder of an undirected graph.

## Usage

``` r
neighborhood_inclusion(g, sparse = FALSE)
```

## Arguments

- g:

  An igraph object

- sparse:

  Logical scalar, whether to create a sparse matrix

## Value

The neighborhood-inclusion preorder of `g` as matrix object. `P[u,v]=1`
if \\N(u)\subseteq N\[v\]\\

## Details

Neighborhood-inclusion is defined as \$\$N(u)\subseteq N\[v\]\$\$ where
\\N(u)\\ is the neighborhood of \\u\\ and \\N\[v\]=N(v)\cup \lbrace
v\rbrace\\ is the closed neighborhood of \\v\\. \\N(u) \subseteq
N\[v\]\\ implies that \\c(u) \leq c(v)\\, where \\c\\ is a centrality
index based on a specific path algebra. Indices falling into this
category are closeness (and variants), betweenness (and variants) as
well as many walk-based indices (eigenvector and subgraph centrality,
total communicability,...).

## References

Schoch, D. and Brandes, U., 2016. Re-conceptualizing centrality in
social networks. *European Journal of Applied Mathematics* 27(6),
971-985.

Brandes, U. Heine, M., Müller, J. and Ortmann, M., 2017. Positional
Dominance: Concepts and Algorithms. *Conference on Algorithms and
Discrete Applied Mathematics*, 60-71.

## See also

[positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md),
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
# the neighborhood inclusion preorder of a star graph is complete
g <- make_star(5, "undirected")
P <- neighborhood_inclusion(g)
comparable_pairs(P)
#> [1] 1

# the same holds for threshold graphs
tg <- threshold_graph(50, 0.1)
P <- neighborhood_inclusion(tg)
comparable_pairs(P)
#> [1] 1

# standard centrality indices preserve neighborhood-inclusion
data("dbces11")
P <- neighborhood_inclusion(dbces11)

is_preserved(P, degree(dbces11))
#> [1] TRUE
is_preserved(P, closeness(dbces11))
#> [1] TRUE
is_preserved(P, betweenness(dbces11))
#> [1] TRUE
```
