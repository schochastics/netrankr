# Rank interval of nodes

Calculate the maximal and minimal rank possible for each node in any
ranking that is in accordance with the partial ranking `P`.

## Usage

``` r
rank_intervals(P)
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

## Value

An object of type netrankr_interval

## Details

Note that the returned `mid_point` is not the same as the expected rank,
for instance computed with
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md).
It is simply the average of `min_rank` and `max_rank`. For exact rank
probabilities use
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md).

## See also

[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)

## Author

David Schoch

## Examples

``` r
P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
rank_intervals(P)
#>  node:V1 rank interval: [1, 2]
#>  node:V2 rank interval: [1, 4]
#>  node:V3 rank interval: [2, 4]
#>  node:V4 rank interval: [3, 5]
#>  node:V5 rank interval: [3, 5]
```
