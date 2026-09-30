# Probabilistic centrality rankings

Performs a complete and exact rank analysis of a given partial ranking.
This includes rank probabilities, relative rank probabilities and
expected ranks.

## Usage

``` r
exact_rank_prob(P, only.results = TRUE, verbose = FALSE, force = FALSE)
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

- only.results:

  Logical. return only results (default) or additionally the ideal tree
  and lattice if `FALSE`.

- verbose:

  Logical. should diagnostics be printed. Defaults to `FALSE`.

- force:

  Logical. If `FALSE` (default), stops the analysis if the partial
  ranking has more than 40 elements and less than 0.4 comparable pairs.
  Only change if you know what you are doing.

## Value

- lin.ext:

  Number of possible rankings that extend `P`.

- mse:

  Array giving the equivalence classes of `P`.

- rank.prob:

  Matrix containing rank probabilities: `rank.prob[u,k]` is the
  probability that u has rank k.

- relative.rank:

  Matrix containing relative rank probabilities: `relative.rank[u,v]` is
  the probability that u is ranked lower than v.

- expected.rank:

  Expected ranks of nodes in any centrality ranking.

- rank.spread:

  Standard deviation of the ranking probabilities.

- topo.order:

  Random ranking used to build the lattice of ideals (if
  `only.results = FALSE`).

- tree:

  Adjacency list (incoming) of the tree of ideals (if
  `only.results = FALSE`).

- lattice:

  Adjacency list (incoming) of the lattice of ideals (if
  `only.results = FALSE`).

- ideals:

  List of order ideals (if `only.results = FALSE`).

In all cases, higher numerical ranks imply a higher position in the
ranking. That is, the lowest ranked node has rank 1.

## Details

The function derives rank probabilities from a given partial ranking
(for instance returned by
[neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
or
[positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md)).
This includes the calculation of expected ranks, (relative) rank
probabilities and the number of possible rankings. Note that the set of
rankings grows exponentially in the number of elements and the exact
calculation becomes infeasible quite quickly and approximations need to
be used. See
[`vignette("benchmarks")`](https://schochastics.github.io/netrankr/articles/benchmarks.md)
for guidelines and
[approx_rank_relative](https://schochastics.github.io/netrankr/reference/approx_rank_relative.md),
[approx_rank_expected](https://schochastics.github.io/netrankr/reference/approx_rank_expected.md),
and
[mcmc_rank_prob](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)
for approximative methods.

## References

De Loof, K. 2009. Efficient computation of rank probabilities in posets.
*Phd thesis*, Ghent University.

De Loof, K., De Meyer, H. and De Baets, B., 2006. Exploiting the lattice
of ideals representation of a poset. *Fundamenta Informaticae*,
71(2,3):309-321.

## See also

[approx_rank_relative](https://schochastics.github.io/netrankr/reference/approx_rank_relative.md),
[approx_rank_expected](https://schochastics.github.io/netrankr/reference/approx_rank_expected.md),
[mcmc_rank_prob](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)

## Author

David Schoch, Julian Müller

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
res <- exact_rank_prob(P)

# a warning is displayed if only one ranking is possible
tg <- threshold_graph(20, 0.2)
P <- neighborhood_inclusion(tg)
res <- exact_rank_prob(P)
#> Warning: P is already a ranking.
#> Expected Ranks correspond to the only possible ranking.
```
