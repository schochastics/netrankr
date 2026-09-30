# Plot rank intervals

This function is deprecated. Use `plot(rank_intervals(P))` instead

## Usage

``` r
plot_rank_intervals(P, cent.df = NULL, ties.method = "min")
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

- cent.df:

  A data frame containing centrality scores of indices (optional). See
  Details.

- ties.method:

  String specifying how ties are treated in the base
  [`rank`](https://rdrr.io/r/base/rank.html) function.

## See also

[rank_intervals](https://schochastics.github.io/netrankr/reference/rank_intervals.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
data("dbces11")
P <- neighborhood_inclusion(dbces11)
if (FALSE) { # \dontrun{
plot_rank_intervals(P)
} # }

# adding index based rankings
cent_scores <- data.frame(
    degree = degree(dbces11),
    betweenness = round(betweenness(dbces11), 4),
    closeness = round(closeness(dbces11), 4),
    eigenvector = round(eigen_centrality(dbces11)$vector, 4)
)
if (FALSE) { # \dontrun{
plot_rank_intervals(P, cent.df = cent_scores)
} # }
```
