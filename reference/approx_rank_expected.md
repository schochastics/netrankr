# Approximation of expected ranks

Implements a variety of functions to approximate expected ranks for
partial rankings.

## Usage

``` r
approx_rank_expected(P, method = "lpom")
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

- method:

  String indicating which method to be used. see Details.

## Value

A vector containing approximated expected ranks.

## Details

The *method* parameter can be set to

- lpom:

  local partial order model

- glpom:

  extension of the local partial order model.

- loof1:

  based on a connection with relative rank probabilities.

- loof2:

  extension of the previous method.

Which of the above methods performs best depends on the structure and
size of the partial ranking. See
[`vignette("benchmarks",package="netrankr")`](https://schochastics.github.io/netrankr/articles/benchmarks.md)
for more details.

## References

Brüggemann R., Simon, U., and Mey,S, 2005. Estimation of averaged ranks
by extended local partial order models. *MATCH Commun. Math. Comput.
Chem.*, 54:489-518.

Brüggemann, R. and Carlsen, L., 2011. An improved estimation of averaged
ranks of partial orders. *MATCH Commun. Math. Comput. Chem.*,
65(2):383-414.

De Loof, L., De Baets, B., and De Meyer, H., 2011. Approximation of
Average Ranks in Posets. *MATCH Commun. Math. Comput. Chem.*,
66:219-229.

## See also

[approx_rank_relative](https://schochastics.github.io/netrankr/reference/approx_rank_relative.md),
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md),
[mcmc_rank_prob](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)

## Author

David Schoch

## Examples

``` r
P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
# Exact result
exact_rank_prob(P)$expected.rank
#>       V1       V2       V3       V4       V5 
#> 1.333333 2.111111 2.888889 4.222222 4.444444 

approx_rank_expected(P, method = "lpom")
#> [1] 1.2 2.0 3.0 4.5 4.5
approx_rank_expected(P, method = "glpom")
#> [1] 1.250000 2.166667 2.833333 4.333333 4.416667
```
