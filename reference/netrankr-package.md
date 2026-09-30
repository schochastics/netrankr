# netrankr: An R package for centrality and partial rankings in networks

netrankr provides several functions to analyze partial rankings for
network centrality. The main focus lies on methods that do not
necessarily rely on indices like degree, betweenness or closeness.
However, the package also provides more than 20 indices, which can be
constructed via a Rstudio addin.

The package follows the philosophy, that centrality can be decomposed in
a series of micro steps. Starting from a network,
[indirect_relations](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
can be derived which can either be aggregated into an index with
[aggregate_positions](https://schochastics.github.io/netrankr/reference/aggregate_positions.md),
or alternatively turned into a partial ranking with
[positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).
The partial ranking can then be further analyzed with
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md),
to obtain probabilistic centrality rankings.

## Details

Some features of the package are:

- Working with the neighborhood inclusion preorder. This forms the bases
  for any centrality analysis on undirected and unweighted graphs. More
  details can be found in the dedicated vignette:
  [`vignette("neighborhood_inclusion",package = "netrankr")`](https://schochastics.github.io/netrankr/articles/neighborhood_inclusion.md)

- Constructing graphs with a unique centrality ranking. This class of
  graphs, known as threshold graphs, can be used to benchmark centrality
  indices, since they only allow for one ranking of the nodes. For more
  details consult the vignette:
  [`vignette("threshold_graph",package = "netrankr")`](https://schochastics.github.io/netrankr/articles/threshold_graph.md)

- Probabilistic centrality. Why apply a handful of indices and choosing
  the one that fits best, when it is possible to analyze **all**
  centrality rankings at once? The package includes several function to
  calculate rank probabilities of nodes in a network. These include
  expected ranks and relative rank probabilities (how likely is it that
  a node is more central than another?) Consult
  [`vignette("probabilistic_cent",package = "netrankr")`](https://schochastics.github.io/netrankr/articles/probabilistic_cent.md)
  for more info.

The package provides several additional vignettes that explain the
functionality of netrankr and its conceptual ideas. See
`browseVignettes(package = 'netrankr')`

## See also

Useful links:

- <https://github.com/schochastics/netrankr/>

- <https://schochastics.github.io/netrankr/>

- Report bugs at <https://github.com/schochastics/netrankr/issues>

## Author

**Maintainer**: David Schoch <david@schochastics.net>
([ORCID](https://orcid.org/0000-0003-2952-4812))

Other contributors:

- Julian Müller <julian.mueller@gess.ethz.ch> \[contributor\]
