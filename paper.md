---
title: 'netrankr: An R package for total, partial, and probabilistic rankings in networks'
tags:
- R
- network analysis
- network centrality
- partial orders
date: "10 August 2022"
output: pdf_document
bibliography: paper.bib
affiliations:
- name: GESIS - Leibniz Institute for the Social Sciences
  index: 1
authors:
- name: David Schoch
  orcid: 0000-0003-2952-4812
  affiliation: 1
---

# Summary

One of the key concepts in network science is network centrality. Centrality
seeks to provide the answer to the question of who (or what) is important in
a network depending on the underlying process forming the network and the
empirical phenomenon in question. In a nutshell, an actor in a network is more
central if they have better relations, where the definition of better relations
depends on the conceptualization of structural importance. Applications of centrality can be 
found in any field where networks arise. In social networks, we may simply be interested in finding 
the most popular user. In bioinformatics, centrality is used to detect essential proteins in 
protein-protein interaction networks [@jmbo-lcpn-01].
Even in sports, centrality is applied to rank athletes or teams [@r-wbpecnahpt-11].
A myriad of indices have been proposed, all with differing interpretations of what
constitutes a central position within a network. Although `netrankr` offers this
traditional approach to network centrality, its main focus lies on alternative assessments
of centrality based on partial and probabilistic rankings in networks. 

# Statement of need

General purpose packages for network analysis such as `igraph` [@cn-ispcnr-06] and
 `sna` [@b-snas-08] implement methods for the most frequently used centrality indices.

However, there also exist dedicated centrality packages, such as 
`centiserve` [@jsaayga-ccrwarpca-15], `CINNA` [@amj-crpdcinna-19], and `influenceR` [@sa-istqsinn-15].
The biggest in terms of implemented indices is `centiserve` which includes $33$ indices.
The primary purpose of `CINNA` is to facilitate the choice of indices by visual 
and statistical tools. `influenceR` is a relatively tiny package which implements 
only a few specialized measures.

The `netrankr` package also offers 
the opportunity to apply more than $30$ indices, but the main focus lies on alternative assessment methods.
Recent results suggest that a variety of partial rankings exists in networks which are preserved by many index based centrality rankings [@sb-sninc-15; @sb-rcsn-16; @svb-ccicurg-17]. 
These partial rankings can be leveraged to assess centrality on a more general level, without necessarily resorting to indices. 
Some of these methods and key functionalities of the package are listed below.

* Compute partial rankings based on neighborhood-inclusion and other dominance relations in networks [@sb-sninc-15; @b-np-16]
* Analyze the partial rankings using rank intervals [@pt-miposlemrp-04] 
* Calculate rank probabilities to assess the likelihood of certain ranks (e.g. How likely is a node the most central one?) [@s-ciprrpn-18]
* Calculate mutual rank probabilities (e.g. How likely is a node more central than another one?)[@s-ciprrpn-18]
* Compute expected ranks of nodes (e.g. How central do we expect a node to be?)[@s-ciprrpn-18]

Note that the most of the tools can also be applied in other empirical settings where partial orders 
arise.

# Background

In an undirected graph $G=(V,E)$ with vertex set $V$ (with cardinality $n = \lvert V\rvert$) and edge set $E$ (with cardinality $m = \lvert E\rvert$), the neighborhood 
of a node $u \in V$ is defined as
$$N(u)=\lbrace w : \lbrace u,w \rbrace \in E \rbrace$$
and its closed neighborhood as $N[v]=N(v) \cup \lbrace v \rbrace$. If the 
neighborhood of the node $u$ is a subset of the closed neighborhood of the node 
$v$, $N(u)\subseteq N[v]$, than it is called a neighborhood inclusion. This concept 
defines a binary relation among nodes in a network and consequently $u$ is 
dominated by $v$ if $N(u)\subseteq N[v]$. Neighborhood-inclusion thus induces a 
partial ranking on the vertices of a network.

@sb-rcsn-16 showed that if $c:V \to \mathbb{R}$ is a centrality index, then 
$$N(u)\subseteq N[v] \implies c(u) \leq c(v)$$
that is, neighborhood inclusion is preserved by indices and the ranking can be viewed as linear extensions of the partial ranking induced by neighborhood-inclusion. Analyzing this partial ranking thus means that all possible centrality rankings can be analyzed at once. More technical details can be found in the dedicated literature [@sb-sninc-15; @b-np-16;@svb-ccicurg-17;@b-cpsn-20].

# Example usage

This example briefly explains some of the functionality of the package and the 
difference to an index driven approach. For more detailed applications see 
the package vignettes. 

We work with a small graph included in the package which was specifically crafted to highlight extreme
differences in centrality rankings.

```R
library(igraph)
library(netrankr)

data("dbces11")
```

![](figures/dbces.pdf)

Say we are interested in the most central node of the graph and simply
compute some standard centrality scores with the `igraph` package.

```R
cent_scores <- data.frame(
   degree = degree(g),
   betweenness = round(betweenness(g), 4),
   closeness = round(closeness(g), 4),
   eigenvector = round(eigen_centrality(g)$vector, 4),
   subgraph = round(subgraph_centrality(g), 4))

# What are the most central nodes for each index?
apply(cent_scores,2,which.max)
#>      degree betweenness   closeness eigenvector    subgraph 
#>          11           8           6           7          10
```


Each index assigns the highest value to a different
vertex and it is not clear which one is the correct choice. 

A more general assessment using `netrankr` starts by calculating the neighborhood inclusion preorder.

```R
P <- neighborhood_inclusion(g)
P
#>    1 2 3 4 5 6 7 8 9 10 11
#> 1  0 0 1 0 1 1 1 0 0  0  1
#> 2  0 0 0 1 0 0 0 1 0  0  0
#> 3  0 0 0 0 1 0 0 0 0  0  1
#> 4  0 0 0 0 0 0 0 0 0  0  0
#> 5  0 0 0 0 0 0 0 0 0  0  0
#> 6  0 0 0 0 0 0 0 0 0  0  0
#> 7  0 0 0 0 0 0 0 0 0  0  0
#> 8  0 0 0 0 0 0 0 0 0  0  0
#> 9  0 0 0 0 0 0 0 0 0  0  0
#> 10 0 0 0 0 0 0 0 0 0  0  0
#> 11 0 0 0 0 0 0 0 0 0  0  0
```

`P[u,v]=1` if $N(u)\subseteq N[v]$. Hence, $u$ is always ranked below $v$.
If `P[u,v]=0`, then there may be indices that rank $u$ above $v$ and vice versa.

We can examine the minimal and maximal possible
rank of each node for rankings that extend the preorder to a total order using rank intervals. 
The bigger the intervals are, the more freedom exists for indices to rank nodes differently.

```R
plot(rank_intervals(P), cent_scores = cent_scores,ties.method = "average")
```

![](figures/rk_intervals.pdf)

Note that the ranks of nodes are not uniformly distributed in the
intervals. The exact probabilities, can be obtained with
`exact_rank_prob()`.

```R
res <- exact_rank_prob(P)
```

`res$rank.prob` contains the probabilities for each node to occupy a certain
rank. For instance, the probability for each node to be the most central
one is as follows:

```R
round(res$rank.prob[,11], 2)
#>    1    2    3    4    5    6    7    8    9   10   11 
#> 0.00 0.00 0.00 0.14 0.16 0.11 0.11 0.14 0.09 0.09 0.16
```

The entry
`res$relative.rank[u,v]` indicates how likely it is that `v` is more central
than `u`.

```R
# How likely is it, that 6 is more central than 3?
round(res$relative.rank[3,6],2)
#> [1] 0.75
```

`res$expected.ranks` contains the expected centrality ranks for all nodes.

```R
round(res$expected.rank, 2)
#>    1    2    3    4    5    6    7    8    9   10   11 
#> 1.71 3.00 4.29 7.50 8.14 6.86 6.86 7.50 6.00 6.00 8.14
```

The higher the value, the more central a node is expected to be.

# References
