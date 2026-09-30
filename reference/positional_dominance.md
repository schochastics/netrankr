# Generalized Dominance Relations

generalized dominance relations that can be computed on one and two mode
networks.

## Usage

``` r
positional_dominance(A, type = "one-mode", map = FALSE, benefit = TRUE)
```

## Arguments

- A:

  Matrix containing attributes or relations, for instance calculated by
  [indirect_relations](https://schochastics.github.io/netrankr/reference/indirect_relations.md).

- type:

  A string which is either 'one-mode' (Default) if `A` is a regular
  one-mode network or 'two-mode' if `A` is a general data matrix.

- map:

  Logical scalar, whether rows can be sorted or not (Default). See
  Details.

- benefit:

  Logical scalar, whether the attributes or relations are benefit or
  cost variables.

## Value

Dominance relations as matrix object. An entry `[u,v]` is `1` if u is
dominated by v.

## Details

Positional dominance is a generalization of neighborhood-inclusion for
arbitrary network data. In the default case, it checks for all pairs
\\u,v\\ if \\A\_{ut} \ge A\_{vt}\\ holds for all \\t\\ if
`benefit = TRUE` or \\A\_{ut} \le A\_{vt}\\ holds for all \\t\\ if
`benefit = FALSE`. This form of dominance is referred to as *dominance
under total heterogeneity*. If `map=TRUE`, the rows of \\A\\ are sorted
decreasingly (`benefit = TRUE`) or increasingly (`benefit = FALSE`) and
then the dominance condition is checked. This second form of dominance
is referred to as *dominance under total homogeneity*, while the first
is called *dominance under total heterogeneity*.

## References

Brandes, U., 2016. Network positions. *Methodological Innovations* 9,
2059799116630650.

Schoch, D. and Brandes, U., 2016. Re-conceptualizing centrality in
social networks. *European Journal of Applied Mathematics* 27(6),
971-985.

## See also

[neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md),
[indirect_relations](https://schochastics.github.io/netrankr/reference/indirect_relations.md),
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)

## Author

David Schoch

## Examples

``` r
library(igraph)

data("dbces11")

P <- neighborhood_inclusion(dbces11)
comparable_pairs(P)
#> [1] 0.1636364

# positional dominance under total heterogeneity
dist <- indirect_relations(dbces11, type = "dist_sp")
D <- positional_dominance(dist, map = FALSE, benefit = FALSE)
comparable_pairs(D)
#> [1] 0.1636364

# positional dominance under total homogeneity
D_map <- positional_dominance(dist, map = TRUE, benefit = FALSE)
comparable_pairs(D_map)
#> [1] 0.8727273
```
