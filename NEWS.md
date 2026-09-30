# netrankr (development version)

* require R >= 3.5.0 and igraph >= 2.1.0
* tests use testthat 3rd edition
* replaced remaining deprecated igraph calls (`get.edge.attribute()`, `graph.density()`, `get.edgelist()`)
* removed unused OpenMP flags from Makevars
* **bug fix**: `mcmc_rank_prob()` was biased because rejected moves of the Markov chain were not counted as samples.
  It now also runs in O(1) per step (was O(n^2)), accepts `rp` beyond the integer range and evaluates the default `rp`
  before structurally equivalent nodes are collapsed
* **bug fix**: `indirect_relations(type = "depend_sp")` overflowed on graphs with many shortest paths.
  It is now also orders of magnitude faster (2000 nodes: 128s -> 0.8s)
* **possibly breaking**: `indirect_relations(type = "depend_rspn")` counted every edge twice and omitted the 1/2 net flow
  factor. Values are now 1/4 of the previous ones and, as documented, converge to `"depend_curflow"`. Rankings are unaffected
* `depend_rspn` is about 3x faster and uses much less memory
* `transitive_reduction()` no longer returns an empty matrix for reflexive input
* `positional_dominance()` errors on non-square input if `map = FALSE` instead of reading out of bounds
* `compare_ranks()` no longer overflows for more than 65536 elements
* all functions taking a partial ranking `P` share one input validation:
  non-square input and `NA` are errors, and a non-zero diagonal is set to 0 with a warning
  (previously it caused crashes, `NA`s or silently wrong results)
* sparse (including pattern) and dense `Matrix` input now works in `approx_rank_expected()`,
  `approx_rank_relative()`, `mcmc_rank_prob()` and `positional_dominance()`
* **bug fix**: `positional_dominance(type = "two-mode")` failed for matrices with column names and
  ignored `benefit` and `map`
* `neighborhood_inclusion()` simplifies graphs with loops or multiple edges (with a warning); these
  previously gave a wrong preorder
* `compare_ranks()` and `is_preserved()` error on `NA` and on invalid input instead of returning
  wrong counts or reading out of bounds
* `approx_rank_expected()` validates `method`; the "loof1" and "loof2" methods are vectorised
* `get_rankings()` returns the single ranking for linear orders
* plot methods restore `par()` also on error and handle a single index and more than 15 indices
* `print.netrankr_interval()` returns its input invisibly
* removed the unexported, deprecated `plot_rank_intervals()`

# netrankr 1.2.4

* added functions from archived NetSwan package
* removed deprecated igraph calls

# netrankr 1.2.3

* removed test causing issues on some platforms

# netrankr 1.2.2

* fixed goodpractice warnings #17
* added more tests
* added more documentation for index_builder #15
* **possibly breaking**: removed mid point calculation from rank_intervals #12
* upgraded `igraph` graph versions #23
* printed some matrices in vignette indirect_relations #11 

# netrankr 1.2.1

* fixed PKGNAME-package \alias as per "Documenting packages" in R-exts.
* fixed bibentry issue
* fixed #21

# netrankr 1.2.0

* benchmark vignette is now reproducible with code and data from `data-raw`
* internal changes after JOSS submission

# netrankr 1.1.1

* removed `hcl.colors` due to backward compatibility for R <3.6 (#9)

# netrankr 1.1.0

* `neighborhood_inclusion()` can return a sparse matrix (Matrix package now imported)
* all functions support sparse matrices as inputs
* added `summary` method for `netrankr_full` objects
* added `as.matrix` method for `netrankr_full` objects to extract probability distributions
* changed to `on.exit(par(op))` in plot functions
* all functions now through errors instead of warnings when the network is vertex transitive
* better error handling if the input is not as expected
* added legends to the default plot function for `netrankr_full` objects
* added legend to the default plot function for `netrankr_mcmc` objects
* changed default colors of the plot function for `netrankr_interval` objects to be more colorblind friendly

# netrankr 1.0.0

* added S3 class `netrankr_full` (result of `exact_rank_prob()`) with print and plot functions (#8)
* added S3 class `netrankr_interval` (result of `rank_intervals()`) with print and plot functions (#8)
* added S3 class `netrankr_mcmc` (result of `mcmc_rank_prob()`) with print and plot functions (#8)
* added `dbces11` graph (smallest graph with 5 different centers)
* `plot_rank_intervals()` is now deprecated 
* ggplot2 no longer suggested


# netrankr 0.3.0

* extended `majorization_gap()` to unconnected graphs
* added `incomparable_pairs()`

# netrankr 0.2.1

* fixed a bug in `index_builder` which prevented the building of self defined indices
* fixed a still existing bug in `transitive_reduction()`
* added `type = weights` in `indirect_relations()`
* `type = "identity"` in `indirect_relations()` is now deprecated. Use `type = "adjacency"` instead.
* `type = "weight"` added to `indirect_relations()` to return the weighted adjacency matrix
* vertex names are now properly added as column and rownames to matrices produced by `indirect_relations()` and
`exact_rank_prob()`.

# netrankr 0.2.0

* added Rstudio addin to build more than 20 centrality indices
* added indirect relations: dist_lf,dist_walk, depend_netflow, 
depend_exp, depend_rsps, depend_rspn, depend_curflow, dist_rwalk, dist_walk
* API breaking: changed "dependencies" to "depend_sp" in `indirect_relations()`
* API breaking: changed "geodesic" to "dist_sp" in `indirect_relations()`
* API breaking: changed "resistance" to "dist_resist" in `indirect_relations()`
* The above old types still work in this version
* changed `require` to `library` in examples

# netrankr 0.1.1

* fixed a bug in `transitive_reduction()`
* fixed some errors in the documentation of `exact_rank_prob()`
* rephrasing of some strong statements

# netrankr 0.1.0

* first public release

# netrankr 0.0.5

* most function reimplemented in C++ for efficiency. 
* vignettes added: `browseVignettes("netrankr")`
* added visualization function `plot_rank_intervals()`
* spell checked and extended help

# netrankr 0.0.1-0.0.4

initial builds, predominantely written in R.

