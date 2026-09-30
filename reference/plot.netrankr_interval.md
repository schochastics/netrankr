# plot netrankr_interval objects

Plots results from
[rank_intervals](https://schochastics.github.io/netrankr/reference/rank_intervals.md)

## Usage

``` r
# S3 method for class 'netrankr_interval'
plot(x, cent_scores = NULL, cent_cols = NULL, ties.method = "min", ...)
```

## Arguments

- x:

  A netrank object

- cent_scores:

  A data frame containing centrality scores of indices (optional)

- cent_cols:

  colors for centrality indices. If NULL a default palette is used.
  Length must be equal to columns in cent_scores.

- ties.method:

  how to treat ties in the rankings. see
  [rank](https://rdrr.io/r/base/rank.html) for details

- ...:

  additional arguments to plot

## Author

David Schoch
