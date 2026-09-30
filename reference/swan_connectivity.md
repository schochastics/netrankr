# Impact on connectivity when a node is removed

`swan_connectivity` measures the loss of connectivity when a node is
removed from the network.

## Usage

``` r
swan_connectivity(g)
```

## Arguments

- g:

  An `igraph` object representing the graph to analyze.

## Value

A numeric vector where each entry represents the connectivity loss when
the corresponding node is removed.

## Details

Connectivity loss indices quantify the decrease in the number of
relationships between nodes when one or more components are removed.
`swan_connectivity` computes the connectivity loss by systematically
excluding each node and evaluating the resulting changes in the network
structure.

The code is an adaptation from the NetSwan package that was archived on
CRAN.

## References

Lhomme S. (2015). *Analyse spatiale de la structure des réseaux
techniques dans un contexte de risques*. Cybergeo: European Journal of
Geography.

## Examples

``` r
library(igraph)
# Example graph (electrical network structure)
elec <- matrix(ncol = 2, byrow = TRUE, c(
  11,1, 11,10, 1,2, 2,3, 2,9,
  3,4, 3,8, 4,5, 5,6, 5,7,
  6,7, 7,8, 8,9, 9,10
))
gra <- graph_from_edgelist(elec, directed = FALSE)

# Compute connectivity loss
f3 <- swan_connectivity(gra)
```
