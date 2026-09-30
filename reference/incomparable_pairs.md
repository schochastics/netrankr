# Incomparable pairs in a partial order

Calculates the fraction of incomparable pairs in a partial order.

## Usage

``` r
incomparable_pairs(P)
```

## Arguments

- P:

  A partial order as matrix object, e.g. calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

## Value

Fraction of incomparable pairs in `P`.

## See also

[comparable_pairs](https://schochastics.github.io/netrankr/reference/comparable_pairs.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- sample_gnp(100, 0.1)
P <- neighborhood_inclusion(g)
comparable_pairs(P)
#> [1] 0
# All pairs of vertices are comparable in a threshold graph
tg <- threshold_graph(100, 0.3)
P <- neighborhood_inclusion(tg)
incomparable_pairs(P)
#> [1] 0
```
