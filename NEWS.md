## netrankr 0.2.0

* added Rstudio addin to build more than 20 centrality indices
* added indirect relations: dist_lf,dist_walk, depend_netflow, 
depend_exp, depend_rsps, depend_rspn, depend_curflow, dist_rwalk, dist_walk
* API breaking: changed "dependencies" to "depend_sp" in `indirect_relations()`
* API breaking: changed "geodesic" to "dist_sp" in `indirect_relations()`
* API breaking: changed "resistance" to "dist_resist" in `indirect_relations()`
* The above old types still work in this version
* changed `require` to `library` in examples

## netrankr 0.1.1

* fixed a bug in `transitive_reduction()`
* fixed some errors in the documentation of `exact_rank_prob()`
* rephrasing of some strong statements

## netrankr 0.1.0

* first public release

## netrankr 0.0.5

* most function reimplemented in C++ for efficiency. 
* vignettes added: `browseVignettes("netrankr")`
* added visualization function `plot_rank_intervals`
* spell checked and extended help

## netrankr 0.0.1-0.0.4

initial builds, predominantely written in R.

