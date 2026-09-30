# Error and attack tolerance of complex networks

`swan_combinatory` assesses network vulnerability and the resistance of
networks to node removals, whether due to random failures or intentional
attacks.

## Usage

``` r
swan_combinatory(g, k)
```

## Arguments

- g:

  An `igraph` object representing the graph to analyze.

- k:

  The number of iterations for assessing the impact of random failures.

## Value

A matrix with five columns:

- Column 1: Fraction of nodes removed.

- Column 2: Connectivity loss from betweenness-based attack.

- Column 3: Connectivity loss from degree-based attack.

- Column 4: Connectivity loss from cascading failure.

- Column 5: Connectivity loss from random failures (averaged over `k`
  iterations).

## Details

Many complex systems display a surprising degree of tolerance against
random failures. However, this resilience often comes at the cost of
extreme vulnerability to targeted attacks, where removing key nodes
(high-degree or high-betweenness nodes) can severely impact network
connectivity.

`swan_combinatory` simulates different attack strategies:

- **Random failure:** Nodes are removed randomly over multiple
  iterations.

- **Degree-based attack:** Nodes are removed in decreasing order of
  their degree.

- **Betweenness-based attack:** Nodes are removed in decreasing order of
  their betweenness centrality.

- **Cascading failure:** Nodes are removed based on recalculated
  betweenness after each removal.

The function returns a matrix showing the connectivity loss for each
attack scenario.

The code is an adaptation from the NetSwan package that was archived on
CRAN.

## References

Albert R., Jeong H., Barabási A. (2000). *Error and attack tolerance of
complex networks*. Nature, 406(6794), 378-382.

## Examples

``` r
library(igraph)
# Example electrical network graph
elec <- matrix(ncol = 2, byrow = TRUE, c(
  11,1, 11,10, 1,2, 2,3, 2,9,
  3,4, 3,8, 4,5, 5,6, 5,7,
  6,7, 7,8, 8,9, 9,10
))
gra <- graph_from_edgelist(elec, directed = FALSE)

# Compute vulnerability measures
f4 <- swan_combinatory(gra, 10)
```
