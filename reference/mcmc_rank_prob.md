# Estimate rank probabilities with Markov Chains

Performs a probabilistic rank analysis based on an almost uniform sample
of possible rankings that preserve a partial ranking.

## Usage

``` r
mcmc_rank_prob(P, rp = nrow(P)^3)
```

## Arguments

- P:

  P A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

- rp:

  Integer indicating the number of samples to be drawn.

## Value

- expected.rank:

  Estimated expected ranks of nodes

- relative.rank:

  Matrix containing estimated relative rank probabilities:
  `relative.rank[u,v]` is the probability that u is ranked lower than v.

## Details

This function can be used instead of
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)
if the number of elements in `P` is too large for an exact computation.
As a rule of thumb, the number of samples should be at least cubic in
the number of elements in `P`. See
[`vignette("benchmarks",package="netrankr")`](https://schochastics.github.io/netrankr/articles/benchmarks.md)
for guidelines and benchmark results.

## References

Bubley, R. and Dyer, M., 1999. Faster random generation of linear
extensions. *Discrete Mathematics*, **201**(1):81-88

## See also

[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md),
[approx_rank_relative](https://schochastics.github.io/netrankr/reference/approx_rank_relative.md),
[approx_rank_expected](https://schochastics.github.io/netrankr/reference/approx_rank_expected.md)

## Author

David Schoch

## Examples

``` r
if (FALSE) { # \dontrun{
data("florentine_m")
P <- neighborhood_inclusion(florentine_m)
res <- exact_rank_prob(P)
mcmc <- mcmc_rank_prob(P, rp = vcount(g)^3)

# mean absolute error (expected ranks)
mean(abs(res$expected.rank - mcmc$expected.rank))
} # }
```
