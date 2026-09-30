# Rankings that extend a partial ranking

Returns all possible rankings that extend a partial ranking.

## Usage

``` r
get_rankings(data, force = FALSE)
```

## Arguments

- data:

  List as returned by
  [exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)
  when run with `only.results = FALSE`

- force:

  Logical scalar. Stops function if the number of rankings is too large.
  Only change to TRUE if you know what you are doing

## Value

A matrix containing ranks of nodes in all possible rankings.

## Details

The `i`th row of the matrix contains the rank of node `i` in all
possible rankings that are in accordance with the partial ranking `P`.
The lowest rank possible is associated with `1`.

## Author

David Schoch

## Examples

``` r
P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
P
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    0    1    1    1
#> [2,]    0    0    0    1    0
#> [3,]    0    0    0    0    1
#> [4,]    0    0    0    0    0
#> [5,]    0    0    0    0    0
res <- exact_rank_prob(P, only.results = FALSE)
get_rankings(res)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#> [1,]    1    1    2    1    1    1    2    1    2
#> [2,]    3    2    1    4    3    2    1    2    1
#> [3,]    2    3    3    2    2    3    3    4    4
#> [4,]    5    5    5    5    4    4    4    3    3
#> [5,]    4    4    4    3    5    5    5    5    5
```
