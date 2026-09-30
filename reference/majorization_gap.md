# Majorization gap

Calculates the (normalized) majorization gap of an undirected graph. The
majorization gap indicates how far the degree sequence of a graph is
from a degree sequence of a
[threshold_graph](https://schochastics.github.io/netrankr/reference/threshold_graph.md).

## Usage

``` r
majorization_gap(g, norm = TRUE)
```

## Arguments

- g:

  An igraph object

- norm:

  `True` (Default) if the normalized majorization gap should be
  returned.

## Value

Majorization gap of an undirected graph.

## Details

The distance is measured by the number of *reverse unit transformations*
necessary to turn the degree sequence into a threshold sequence. First,
the *corrected conjugated degree sequence* d' is calculated from the
degree sequence d as follows: \$\$d'\_k= \|\\ i : i\<k \land d_i\geq k-1
\\ \| + \| \\ i : i\>k \land d_i\geq k \\ \|.\$\$ the majorization gap
is then defined as \$\$1/2 \sum\_{k=1}^n \max\\d'\_k - d_k,0\\\$\$ The
higher the value, the further away is a graph to be a threshold graph.

## References

Schoch, D., Valente, T. W. and Brandes, U., 2017. Correlations among
centrality indices and a class of uniquely ranked graphs. *Social
Networks* **50**, 46–54.

Arikati, S.R. and Peled, U.N., 1994. Degree sequences and majorization.
*Linear Algebra and its Applications*, **199**, 179-211.

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_star(5, "undirected")
majorization_gap(g) # 0 since star graphs are threshold graphs
#> [1] 0

g <- sample_gnp(100, 0.15)
majorization_gap(g, norm = TRUE) # fraction of reverse unit transformation
#> [1] 0.7296248
majorization_gap(g, norm = FALSE) # number of reverse unit transformation
#> [1] 564
```
