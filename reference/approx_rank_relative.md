# Approximation of relative rank probabilities

Approximate relative rank probabilities \\P(rk(u)\<rk(v))\\. In a
network context, \\P(rk(u)\<rk(v))\\ is the probability that u is less
central than v, given the partial ranking P.

## Usage

``` r
approx_rank_relative(P, iterative = TRUE, num.iter = 10)
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

- iterative:

  Logical scalar if iterative approximation should be used.

- num.iter:

  Number of iterations to be used. defaults to 10 (see Details).

## Value

a matrix containing approximation of relative rank probabilities.
`relative.rank[i,j]` is the probability that i is ranked lower than j

## Details

The iterative approach generally gives better approximations than the
non iterative, if only slightly. The default number of iterations is
based on the observation, that the approximation does not improve
significantly beyond this value. This observation, however, is based on
very small networks such that increasing it for large network may yield
better results. See
[`vignette("benchmarks",package="netrankr")`](https://schochastics.github.io/netrankr/articles/benchmarks.md)
for more details.

## References

De Loof, K. and De Baets, B and De Meyer, H., 2008. Properties of mutual
rank probabilities in partially ordered sets. In *Multicriteria Ordering
and Ranking: Partial Orders, Ambiguities and Applied Issues*, 145-165.

## See also

[approx_rank_expected](https://schochastics.github.io/netrankr/reference/approx_rank_expected.md),
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md),
[mcmc_rank_prob](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)

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
approx_rank_relative(P, iterative = FALSE)
#>           [,1]      [,2]      [,3] [,4]      [,5]
#> [1,] 0.0000000 0.3333333 1.0000000 1.00 1.0000000
#> [2,] 0.6666667 0.0000000 0.3333333 1.00 0.1428571
#> [3,] 0.0000000 0.6666667 0.0000000 0.25 1.0000000
#> [4,] 0.0000000 0.0000000 0.7500000 0.00 0.5000000
#> [5,] 0.0000000 0.8571429 0.0000000 0.50 0.0000000
approx_rank_relative(P, iterative = TRUE)
#>           [,1]      [,2]      [,3]      [,4]      [,5]
#> [1,] 0.0000000 0.6858378 1.0000000 1.0000000 1.0000000
#> [2,] 0.3141622 0.0000000 0.6026415 1.0000000 0.8103500
#> [3,] 0.0000000 0.3973585 0.0000000 0.7130124 1.0000000
#> [4,] 0.0000000 0.0000000 0.2869876 0.0000000 0.5313942
#> [5,] 0.0000000 0.1896500 0.0000000 0.4686058 0.0000000
```
