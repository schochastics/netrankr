# Random threshold graphs

Constructs a random threshold graph. A threshold graph is a graph where
the neighborhood inclusion preorder is complete.

## Usage

``` r
threshold_graph(n, p, bseq)
```

## Arguments

- n:

  The number of vertices in the graph.

- p:

  The probability of inserting dominating vertices. Equates
  approximately to the density of the graph. See Details.

- bseq:

  (0,1)-vector a binary sequence that produces a threshold grah. See
  details

## Value

A threshold graph as igraph object

## Details

Either `n` and `p`, or `bseq` must be specified. Threshold graphs can be
constructed with a binary sequence. For each 0, an isolated vertex is
inserted and for each 1, a vertex is inserted that connects to all
previously inserted vertices. The probability of inserting a dominating
vertices is controlled with parameter `p`. If `bseq` is given instead, a
threshold graph is constructed from that sequence. An important property
of threshold graphs is, that all centrality indices induce the same
ranking.

## References

Mahadev, N. and Peled, U. N. , 1995. Threshold graphs and related
topics.

Schoch, D., Valente, T. W. and Brandes, U., 2017. Correlations among
centrality indices and a class of uniquely ranked graphs. *Social
Networks* 50, 46–54.

## See also

[neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md),
[positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- threshold_graph(10, 0.3)
if (FALSE) { # \dontrun{
plot(g)

# star graphs and complete graphs are threshold graphs
complete <- threshold_graph(10, 1) # complete graph
plot(complete)

star <- threshold_graph(10, 0) # star graph
plot(star)
} # }

# centrality scores are perfectly rank correlated
cor(degree(g), closeness(g), method = "kendall")
#> [1] 1
```
