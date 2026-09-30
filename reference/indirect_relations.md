# Indirect relations in a network

Derive indirect relations for a given network. Observed relations, like
presents or absence of a relation, are commonly not the center of
analysis, but are transformed in a new set of indirect relation like
shortest path distances among nodes. These transformations are usually
an implicit step when centrality indices are used. Making this step
explicit gives more possibilities, for example calculating partial
centrality rankings with
[positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md).

## Usage

``` r
indirect_relations(
  g,
  type = "dist_sp",
  lfparam = NULL,
  dwparam = NULL,
  netflowmode = "",
  rspxparam = NULL,
  FUN = identity,
  ...
)
```

## Arguments

- g:

  igraph object. The network for which relations should be derived.

- type:

  String giving the relation to be calculated. See Details for options.

- lfparam:

  Numeric parameter. Only used if type = "dist_lf".

- dwparam:

  Numeric parameter. Only used if type = "dist_walk".

- netflowmode:

  String, one of raw, frac, or norm. Only used if type =
  "depend_netflow".

- rspxparam:

  Numeric parameter. Only used if type = "depend_rsps" or type =
  "depend_rspn".

- FUN:

  A function that allows the transformation of relations. See Details.

- ...:

  Additional arguments passed to FUN.

## Value

A matrix containing indirect relations in a network.

## Details

The `type` parameter has the following options.

*'adjacency'* returns the adjacency matrix of the network.

*'weights'* returns the weighted adjacency matrix of the network if an
edge attribute 'weight' is present.

*'dist_sp'* returns shortest path distances between all pairs of nodes.

*'depend_sp'* returns dyadic dependencies \$\$\delta(u,s) = \sum\_{t \in
V} \frac{\sigma(s,t\|u)}{\sigma(s,t)}\$\$ where \\\sigma(s,t\|u)\\ is
the number of shortest paths from s to t that include u and
\\\sigma(s,t)\\ is the total number of shortest (s,t)-paths. This
relation is used for betweenness-like centrality indices.

*'walks'* returns walk counts between pairs of nodes, usually they are
weighted decreasingly in their lengths or other properties which can be
done by adding a function in `FUN`. See
[transform_relations](https://schochastics.github.io/netrankr/reference/transform_relations.md)
for options.

*'dist_resist'* returns the resistance distance between all pairs of
nodes.

*'dist_lf'* returns a logarithmic forest distance \\d\_\alpha(s,t)\\.
The logarithmic forest distances form a one-parametric family of
distances, converging to shortest path distances as \\\alpha -\> 0\\ and
to the resistance distance as \\\alpha -\> \infty\\. See (Chebotarev,
2011) for more details. The parameter `lfparam` can be used to tune
\\\alpha\\.

*'dist_walk'* returns the walk distance \\d\_\alpha^W(s,t)\\ between
nodes. The walk distances form a one-parametric family of distances,
converging to shortest path distances as \\\alpha -\> 0\\ and to longest
walk distances for \\\alpha -\> \infty\\. Walk distances contain the
logarithmic forest distances as a special case. See (Chebotarev, 2012)
for more details.

*'dist_rwalk'* returns the expected length of a random walk between two
nodes. For more details see (Noh and Rieger, 2004)

*'depend_netflow'* returns dependencies based on network flow (See
Freeman et al.,1991). If `netflowmode="raw"`, the function returns
\$\$\delta(u,s) = \sum\_{t \in V} f(s,t,G)-f(s,t,G-v)\$\$ where f(s,t,G)
is the maximum flow from s to t in G and f(s,t,G-v) in G without the
node v. For `netflowmode="frac"` it returns dependencies in the form,
similar to shortest path dependencies: \$\$\delta(u,s) = \sum\_{t \in V}
\frac{f(s,t,G)-f(s,t,G-v)}{f(s,t,G)}\$\$

*'depend_curflow'* returns pairwise dependencies based on current flow.
The relation is based on the same idea as 'depend_sp' and
'depend_netflow'. However, instead of considering shortest paths or
network flow, the current flow (or equivalent: random walks) between
nodes are of interest. See (Newman, 2005) for details.

*'depend_exp'* returns pairwise dependencies based on 'communicability':
\$\$\delta(u,s)=\sum\_{t \in V}
\frac{exp(A)\_{st}-exp(A+E(u))\_{st}}{exp(A)\_{st}},\$\$ where E(u) has
nonzeros only in row and column u, and in this row and column has -1 if
A has +1. See (Estrada et al., 2009) for additional details.

*'depend_rsps'*. Simple randomized shortest path dependencies. The
simple RSP dependency of a node u with respect to absorbing paths from s
to t, is defined as the expected number of visits through u over all
s-t-walks. The parameter `rspxparam` is the "inverse temperature
parameter". If it converges to infinity, only shortest paths are
considered and the expected number of visits to a node on a shortest
path approaches the probability of following that particular path. When
the parameter converges to zero, then the dependencies converge to the
expected number of visits to a node over all absorbing walks with
respect to the unbiased random walk probabilities. This means for
undirected networks, that the relations converge to adjacency. See
(Kivimäki et al., 2016) for details.

*'depend_rspn'* Net randomized shortest path dependencies. The parameter
`rspxparam` is the "inverse temperature parameter". The asymptotic for
the infinity case are the same as for 'depend_rsps'. If the parameter
approaches zero, then it converges to 'depend_curflow'. The net
randomized shortest path dependencies are closely related to the random
walk interpretation of current flows. See (Kivimäki et al., 2016) for
technical details.

The function `FUN` is used to transform the indirect relation. See
[transform_relations](https://schochastics.github.io/netrankr/reference/transform_relations.md)
for predefined functions and additional help.

## References

Chebotarev, P., 2012. The walk distances in graphs. *Discrete Applied
Mathematics*, 160(10), pp.1484-1500.

Chebotarev, P., 2011. A class of graph-geodetic distances generalizing
the shortest-path and the resistance distances. *Discrete Applied
Mathematics* 159,295-302.

Noh, J.D. and Rieger, H., 2004. Random walks on complex networks.
*Physical Review Letters*, 92(11), p.118701.

Freeman, L.C., Borgatti, S.P., and White, D.R., 1991. Centrality in
Valued Graphs: A Measure of Betweenness Based on Network Flow. *Social
Networks* 13(2), 141-154.

Newman, M.E., 2005. A measure of betweenness centrality based on random
walks. *Social Networks*, 27(1), pp.39-54.

Estrada, E., Higham, D.J., and Hatano, N., 2009. Communicability
betweenness in complex networks. *Physica A* 388,764-774.

Kivimäki, I., Lebichot, B., Saramäki, J., and Saerens, M., 2016. Two
betweenness centrality measures based on Randomized Shortest Paths
*Scientific Reports* 6: 19668

## See also

[aggregate_positions](https://schochastics.github.io/netrankr/reference/aggregate_positions.md)
to build centrality indices,
[positional_dominance](https://schochastics.github.io/netrankr/reference/positional_dominance.md)
to derive dominance relations

## Author

David Schoch

## Examples

``` r
library(igraph)
data("dbces11")

# shortest path distances
D <- indirect_relations(dbces11, type = "dist_sp")

# inverted shortest path distances
D <- indirect_relations(dbces11, type = "dist_sp", FUN = dist_inv)

# shortes path dependencies (used for betweenness)
D <- indirect_relations(dbces11, type = "depend_sp")

# walks attenuated exponentially by their length
W <- indirect_relations(dbces11, type = "walks", FUN = walks_exp)
```
