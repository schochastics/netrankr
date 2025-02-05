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

