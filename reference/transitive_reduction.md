# Transitive Reduction

Calculates the transitive reduction of a partial ranking.

## Usage

``` r
transitive_reduction(P)
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

## Value

transitive reduction of `P`

## Author

David Schoch

## Examples

``` r
library(igraph)

g <- threshold_graph(100, 0.1)
P <- neighborhood_inclusion(g)
sum(P)
#> [1] 5417

R <- transitive_reduction(P)
sum(R)
#> [1] 179
```
