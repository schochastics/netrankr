# Partial ranking as directed graph

Turns a partial ranking into a directed graph. An edge (u,v) is present
if `P[u,v]=1`, meaning that u is dominated by v.

## Usage

``` r
dominance_graph(P)
```

## Arguments

- P:

  A partial ranking as matrix object calculated with
  [neighborhood_inclusion](https://schochastics.github.io/netrankr/reference/neighborhood_inclusion.md)
  or
  [positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

## Value

Directed graph as an igraph object.

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- threshold_graph(20, 0.1)
P <- neighborhood_inclusion(g)
d <- dominance_graph(P)
if (FALSE) { # \dontrun{
plot(d)
} # }

# to reduce overplotting use transitive reduction
P <- transitive_reduction(P)
d <- dominance_graph(P)
if (FALSE) { # \dontrun{
plot(d)
} # }
```
