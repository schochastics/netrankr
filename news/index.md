# Changelog

## netrankr 2.0.0

This release fixes a large number of bugs found in a full code review.
Several of them change numerical results for code that ran without
errors before, hence the major version.

### Breaking changes

These change results **without an error**. Please re-check analyses that
use them.

- [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
  ignores an edge attribute `weight` for all types except `"weights"`
  and warns when it does so. Previously `"dist_sp"`, `"dist_resist"` and
  `"dist_lf"` used weights silently, while all other types ignored them
  (and with igraph \>= 3.0 the adjacency-based types would have picked
  them up too, giving meaningless values for `"depend_curflow"`). For
  weighted shortest path distances use `igraph::distances(g)` directly,
  for the weighted adjacency matrix `type = "weights"`.
- `indirect_relations(type = "depend_rspn")` counted every edge twice
  and omitted the 1/2 net flow factor. Values are now 1/4 of the
  previous ones and, as documented, converge to `"depend_curflow"` for
  `rspxparam -> 0`. Rankings are unaffected. Multiply by 4 to obtain the
  old values.
- The entries of `indirect_relations(type = "depend_rsps")` now match
  the documented definition (expected number of visits on absorbing
  randomized shortest paths). Their row sums, the RSP betweenness, and
  hence rankings are unchanged. The old entries were wrong and cannot be
  restored.
- [`walks_uptok()`](https://schochastics.github.io/netrankr/reference/transform_relations.md)
  includes the `j = 0` (identity) term, as documented. Use
  `FUN = function(x, ...) walks_uptok(x, ...) - 1` for the old
  behaviour.
- `majorization_gap(norm = TRUE)` on disconnected graphs divides the sum
  of the gaps of all components by the total number of edges, so the
  value stays in \[0, 1\]. Previously the normalised gaps of the
  components were added up.
- For partial rankings with structurally equivalent nodes,
  [`exact_rank_prob()`](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)
  and
  [`mcmc_rank_prob()`](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)
  compute the expected ranks exactly (equivalent nodes are tied at the
  highest rank of their class, as for linear orders). Previously a
  heuristic was used.
- [`mcmc_rank_prob()`](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)
  was biased (rejected moves were not counted as samples), so its
  estimates change.

Other changes that may require changes to code:

- All functions taking a partial ranking `P` share one input validation:
  non-square input and `NA` are errors, and a non-zero diagonal is set
  to 0 with a warning (previously crashes, `NA`s or silently wrong
  results).
- [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
  errors for relations that are only defined on connected graphs
  (`"dist_resist"`, `"depend_curflow"`, `"dist_rwalk"`, `"depend_exp"`,
  `"depend_rsps"`, `"depend_rspn"`, and `"depend_netflow"` with
  `netflowmode = "frac"`) instead of failing in LAPACK or returning
  `NaN`s.
- [`compare_ranks()`](https://schochastics.github.io/netrankr/reference/compare_ranks.md)
  and
  [`is_preserved()`](https://schochastics.github.io/netrankr/reference/is_preserved.md)
  error on `NA`
  ([`compare_ranks()`](https://schochastics.github.io/netrankr/reference/compare_ranks.md)
  counted such pairs as ties).
- [`neighborhood_inclusion()`](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  simplifies graphs with loops or multiple edges with a warning (these
  gave a wrong preorder).
- [`hyperbolic_index()`](https://schochastics.github.io/netrankr/reference/hyperbolic_index.md)
  and
  [`spectral_gap()`](https://schochastics.github.io/netrankr/reference/spectral_gap.md)
  reject directed graphs.
- Removed the unexported, deprecated `plot_rank_intervals()`. Use
  `plot(rank_intervals(P))`.
- Requires R \>= 3.5.0 and igraph \>= 2.1.0.

### Bug fixes

- `indirect_relations(type = "depend_sp")` overflowed on graphs with
  many shortest paths.
- `positional_dominance(type = "two-mode")` failed for matrices with
  column names and ignored `benefit` and `map`; with `map = FALSE`
  non-square one-mode input read out of bounds.
- The random failure scenario of
  [`swan_combinatory()`](https://schochastics.github.io/netrankr/reference/swan_combinatory.md)
  only removed `k` nodes (the number of repetitions) instead of all
  nodes, and failed for `k > n`.
- [`majorization_gap()`](https://schochastics.github.io/netrankr/reference/majorization_gap.md)
  recycled vectors for disconnected graphs.
- [`transitive_reduction()`](https://schochastics.github.io/netrankr/reference/transitive_reduction.md)
  returned an empty matrix for reflexive input.
- [`compare_ranks()`](https://schochastics.github.io/netrankr/reference/compare_ranks.md)
  overflowed for more than 65536 elements.
- [`mcmc_rank_prob()`](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)
  evaluated the default `rp` after collapsing structurally equivalent
  nodes and silently did nothing for `rp` beyond the integer range.
- `indirect_relations(type = "depend_exp")` ignored edges with
  multiplicity \> 1.
- [`hyperbolic_index()`](https://schochastics.github.io/netrankr/reference/hyperbolic_index.md)
  returned `NaN` for isolated nodes,
  [`spectral_gap()`](https://schochastics.github.io/netrankr/reference/spectral_gap.md)
  complex numbers for directed graphs.
- [`index_builder()`](https://schochastics.github.io/netrankr/reference/index_builder.md):
  fixed the generated code for `"dist_walk"` (used the log forest
  parameter) and without pipes (ignored the network name), the alpha
  sliders (duplicated input ids) and presets resetting the
  transformation.
- `aggregate_positions(type = "self")` failed on `Matrix` objects.
- Sparse (including pattern) and dense `Matrix` input failed in
  [`approx_rank_expected()`](https://schochastics.github.io/netrankr/reference/approx_rank_expected.md),
  [`approx_rank_relative()`](https://schochastics.github.io/netrankr/reference/approx_rank_relative.md),
  [`mcmc_rank_prob()`](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)
  and
  [`positional_dominance()`](https://schochastics.github.io/netrankr/reference/positional_dominance.md).
- Plot methods did not restore
  [`par()`](https://rdrr.io/r/graphics/par.html) on error and failed for
  a single index or more than 15 indices.

### Improvements

- Much faster: `"depend_sp"` (2000 nodes: 128s -\> 0.8s), `"dist_rwalk"`
  (300 nodes: 3.2s -\> 0.01s), `"depend_rspn"` (~3x, far less memory),
  [`mcmc_rank_prob()`](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md)
  (O(1) instead of O(n^2) per step),
  [`neighborhood_inclusion()`](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  (2-4x), `"depend_netflow"` (half the maximum flows),
  [`swan_combinatory()`](https://schochastics.github.io/netrankr/reference/swan_combinatory.md)
  and
  [`swan_connectivity()`](https://schochastics.github.io/netrankr/reference/swan_connectivity.md)
  (components instead of all shortest paths), vectorised
  `"loof1"`/`"loof2"`.
- Informative errors for invalid `type`/`method` arguments and input of
  [`threshold_graph()`](https://schochastics.github.io/netrankr/reference/threshold_graph.md),
  [`spectral_gap()`](https://schochastics.github.io/netrankr/reference/spectral_gap.md),
  `swan_*()`.
- [`get_rankings()`](https://schochastics.github.io/netrankr/reference/get_rankings.md)
  returns the single ranking for linear orders.
- [`print.netrankr_interval()`](https://schochastics.github.io/netrankr/reference/print.netrankr_interval.md)
  returns its input invisibly.
- [`index_builder()`](https://schochastics.github.io/netrankr/reference/index_builder.md)
  checks for all required packages.
- Documented the normalisation of `"depend_exp"`, the behaviour of
  [`swan_efficiency()`](https://schochastics.github.io/netrankr/reference/swan_efficiency.md)
  on disconnected graphs and the handling of edge weights.

### Housekeeping

- Tests use testthat 3rd edition; many new regression tests.
- Replaced deprecated igraph calls; no use of the `attr` argument
  deprecated in igraph 3.0.
- Native routines are registered; removed unused OpenMP flags.

## netrankr 1.2.4

CRAN release: 2025-02-05

- added functions from archived NetSwan package
- removed deprecated igraph calls

## netrankr 1.2.3

CRAN release: 2023-12-19

- removed test causing issues on some platforms

## netrankr 1.2.2

CRAN release: 2023-12-15

- fixed goodpractice warnings
  [\#17](https://github.com/schochastics/netrankr/issues/17)
- added more tests
- added more documentation for index_builder
  [\#15](https://github.com/schochastics/netrankr/issues/15)
- **possibly breaking**: removed mid point calculation from
  rank_intervals
  [\#12](https://github.com/schochastics/netrankr/issues/12)
- upgraded `igraph` graph versions
  [\#23](https://github.com/schochastics/netrankr/issues/23)
- printed some matrices in vignette indirect_relations
  [\#11](https://github.com/schochastics/netrankr/issues/11)

## netrankr 1.2.1

CRAN release: 2023-08-20

- fixed PKGNAME-package as per “Documenting packages” in R-exts.
- fixed bibentry issue
- fixed [\#21](https://github.com/schochastics/netrankr/issues/21)

## netrankr 1.2.0

CRAN release: 2022-09-26

- benchmark vignette is now reproducible with code and data from
  `data-raw`
- internal changes after JOSS submission

## netrankr 1.1.1

CRAN release: 2021-12-21

- removed `hcl.colors` due to backward compatibility for R \<3.6
  ([\#9](https://github.com/schochastics/netrankr/issues/9))

## netrankr 1.1.0

CRAN release: 2021-09-03

- [`neighborhood_inclusion()`](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  can return a sparse matrix (Matrix package now imported)
- all functions support sparse matrices as inputs
- added `summary` method for `netrankr_full` objects
- added `as.matrix` method for `netrankr_full` objects to extract
  probability distributions
- changed to `on.exit(par(op))` in plot functions
- all functions now through errors instead of warnings when the network
  is vertex transitive
- better error handling if the input is not as expected
- added legends to the default plot function for `netrankr_full` objects
- added legend to the default plot function for `netrankr_mcmc` objects
- changed default colors of the plot function for `netrankr_interval`
  objects to be more colorblind friendly

## netrankr 1.0.0

CRAN release: 2021-07-16

- added S3 class `netrankr_full` (result of
  [`exact_rank_prob()`](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md))
  with print and plot functions
  ([\#8](https://github.com/schochastics/netrankr/issues/8))
- added S3 class `netrankr_interval` (result of
  [`rank_intervals()`](https://schochastics.github.io/netrankr/reference/rank_intervals.md))
  with print and plot functions
  ([\#8](https://github.com/schochastics/netrankr/issues/8))
- added S3 class `netrankr_mcmc` (result of
  [`mcmc_rank_prob()`](https://schochastics.github.io/netrankr/reference/mcmc_rank_prob.md))
  with print and plot functions
  ([\#8](https://github.com/schochastics/netrankr/issues/8))
- added `dbces11` graph (smallest graph with 5 different centers)
- `plot_rank_intervals()` is now deprecated
- ggplot2 no longer suggested

## netrankr 0.3.0

CRAN release: 2020-09-09

- extended
  [`majorization_gap()`](https://schochastics.github.io/netrankr/reference/majorization_gap.md)
  to unconnected graphs
- added
  [`incomparable_pairs()`](https://schochastics.github.io/netrankr/reference/incomparable_pairs.md)

## netrankr 0.2.1

CRAN release: 2018-09-18

- fixed a bug in `index_builder` which prevented the building of self
  defined indices
- fixed a still existing bug in
  [`transitive_reduction()`](https://schochastics.github.io/netrankr/reference/transitive_reduction.md)
- added `type = weights` in
  [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
- `type = "identity"` in
  [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
  is now deprecated. Use `type = "adjacency"` instead.
- `type = "weight"` added to
  [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
  to return the weighted adjacency matrix
- vertex names are now properly added as column and rownames to matrices
  produced by
  [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
  and
  [`exact_rank_prob()`](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md).

## netrankr 0.2.0

CRAN release: 2018-01-08

- added Rstudio addin to build more than 20 centrality indices
- added indirect relations: dist_lf,dist_walk, depend_netflow,
  depend_exp, depend_rsps, depend_rspn, depend_curflow, dist_rwalk,
  dist_walk
- API breaking: changed “dependencies” to “depend_sp” in
  [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
- API breaking: changed “geodesic” to “dist_sp” in
  [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
- API breaking: changed “resistance” to “dist_resist” in
  [`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
- The above old types still work in this version
- changed `require` to `library` in examples

## netrankr 0.1.1

- fixed a bug in
  [`transitive_reduction()`](https://schochastics.github.io/netrankr/reference/transitive_reduction.md)
- fixed some errors in the documentation of
  [`exact_rank_prob()`](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)
- rephrasing of some strong statements

## netrankr 0.1.0

- first public release

## netrankr 0.0.5

- most function reimplemented in C++ for efficiency.
- vignettes added: `browseVignettes("netrankr")`
- added visualization function `plot_rank_intervals()`
- spell checked and extended help

## netrankr 0.0.1-0.0.4

initial builds, predominantely written in R.
