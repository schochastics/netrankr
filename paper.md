---
title: 'netrankr: An R package for total, partial, and probabilistic rankings in
  networks'
tags:
- R
- network analysis
- network centrality
- partial orders
date: "04 July 2022"
output: pdf_document
authors:
- name: David Schoch
  orcid: 0000-0003-2952-4812
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: GESIS - Leibniz Institute for the Social Sciences
  index: 1
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
constitutes a central position within a network.

`netrankr` is build around the idea that centrality is more than just the application of 
indices to determine structurally important nodes. Recent results suggest, that there exist a 
variety of partial rankings in networks which are preserved by many index based centrality rankings [@sb-sninc-15; @sb-rcsn-16; @svb-ccicurg-17]. 
These partial rankings can be leveraged to assess centrality on a more general level, without necessarily resorting to indices. 
Some of the key functionalities of the package are listed below.

* Compute partial rankings based on neighborhood-inclusion and other dominance relations in networks [@sb-sninc-15; @b-np-16]
* Analyze the partial rankings using rank intervals [@pt-miposlemrp-04] 
* Calculate rank probabilities to assess the likelihood of certain rank (e.g. How likely is a certain node the most central one?) [@s-ciprrpn-18]
* Calculate mutual rank probabilities (e.g. how likely is a node more central than another one?)[@s-ciprrpn-18]
* Compute expected ranks of nodes (How central do we expect a node to be?)[@s-ciprrpn-18]

Note that most of the tools can also be applied in other empirical settings where partial orders 
arise and need to be analyzed.

# Background

In an undirected graph $G=(V,E)$ with vertex set $V$ (with cardinality $n = \lvert V\rvert$) and edge set $E$ (with cardinality $m = \lvert E\rvert$), the neighborhood 
of a node $u \in V$ is defined as
$$N(u)=\lbrace w : \lbrace u,w \rbrace \in E \rbrace$$
and its closed neighborhood as $N[v]=N(v) \cup \lbrace v \rbrace$. If the 
neighborhood of a node $u$ is a subset of the closed neighborhood of a node 
$v$, $N(u)\subseteq N[v]$, we speak of neighborhood inclusion. This concept 
defines a binary relation among nodes in a network and we say that $u$ is 
dominated by $v$ if $N(u)\subseteq N[v]$. Neighborhood-inclusion thus induces a 
partial ranking on the vertices of a network.

@sb-rcsn-16 showed that if $c:V \to \mathbb{R}$ is a centrality index, then 
$$N(u)\subseteq N[v] \implies c(u) \leq c(v)$$
That is, neighborhood inclusion is preserved by centrality indices and the ranking induced by indices
can be viewed as linear extensions of the partial ranking induced by neighborhood-inclusion. Analyzing this partial
ranking thus means that all possible centrality rankings can be analyzed at once.
More technical details can be found in the dedicated literature [@sb-sninc-15; @b-np-16;@svb-ccicurg-17;@b-cpsn-20].

# References
