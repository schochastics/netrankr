# Check preservation

Checks if a partial ranking is preserved in the ranking induced by
`scores`.

## Usage

``` r
is_preserved(P, scores)
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

- scores:

  Numeric vector containing the scores of a centrality index.

## Value

Logical scaler whether `scores` preserves the relations in `P`.

## Details

In order for a score vector to preserve a partial ranking, the following
condition must be fulfilled: `P[u,v]==1 & scores[i]<=scores[j]`.

## Author

David Schoch

## Examples

``` r

library(igraph)
# standard measures of centrality preserve the neighborhood inclusion preorder
data("dbces11")
P <- neighborhood_inclusion(dbces11)

is_preserved(P, degree(dbces11))
#> [1] TRUE
is_preserved(P, betweenness(dbces11))
#> [1] TRUE
is_preserved(P, closeness(dbces11))
#> [1] TRUE
```
