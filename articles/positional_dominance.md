# Positional dominance in networks

This vignette describes the concept of positional dominance, the
generalization of
[neighborhood-inclusion](https://schochastics.github.io/netrankr/articles/neighborhood_inclusion.md)
for arbitrary network and attribute data. Additionally, some use cases
with the `netrankr` package are given. The partial ranking induced by
positional dominance can be used to assess [partial
centrality](https://schochastics.github.io/netrankr/articles/partial_centrality.md)
or compute [probabilistic
centrality](https://schochastics.github.io/netrankr/articles/probabilistic_cent.md).

------------------------------------------------------------------------

## Theoretical Background

A network can be described as a *dyadic variable*
$`x\in \mathcal{W}^\mathcal{D}`$, where $`\mathcal{W}`$ is the value
range of the network (in the simple case of unweighted networks
$`\mathcal{W}=\{0,1\}`$) and
$`\mathcal{D}=\mathcal{N}\times\mathcal{A}`$ describes the dyadic domain
of actors $`\mathcal{N}`$ and affiliations $`\mathcal{A}`$. If
$`\mathcal{A}\neq\mathcal{N}`$, we obtain a *two-mode network* and if
$`\mathcal{A}=\mathcal{N}`$ a *one-mode network* consisting of relations
among actors.  
  
  

**Definition**

Let $`x\in \mathcal{W}^\mathcal{D}`$ be a network and
$`i,j \in \mathcal{N}`$. We say that $`i`$ is dominated by $`j`$*under
the total homogeneity assumption*, denoted by $`i \leq j`$ if
``` math
x_{it}\leq x_{jt} \quad \forall t \in \mathcal{N}.
```
If there exists a permutation $`\pi: \mathcal{N} \to \mathcal{N}`$ such
that
``` math
x_{it}\leq x_{j\pi(t)} \quad \forall t \in \mathcal{N},
```
we say that $`i`$ is dominated by $`j`$*under the total heterogeneity
assumption*, denoted by $`i ⪯ j`$.  
It holds that $`i\leq j \implies i ⪯ j`$ but not vice versa.  
  
  

More about the positional dominance and the positional approach to
network analysis can be found in

> Brandes, Ulrik. (2016). Network Positions. *Methodological
> Innovations*, **9**, 2059799116630650.
> ([link](https://dx.doi.org/10.1177/2059799116630650))

------------------------------------------------------------------------

## Positional Dominance in the `netrankr` Package

``` r

library(netrankr)
library(igraph)
library(magrittr)
set.seed(1886) #for reproducibility
```

The function `positional_dominance` can be used to check both, dominance
under homogeneity and heterogeneity. In accordance with the analytic
pipeline of centrality we use the `%>%` operator.

``` r

data("dbces11")
g <- dbces11

#neighborhood inclusion can be expressed with the analytic pipeline
D <- g %>% indirect_relations(type="adjacency") %>% positional_dominance()
```

More on the
[`indirect_relations()`](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
function can be found in
[this](https://schochastics.github.io/netrankr/articles/indirect_relations.md)
vignette.

The `map` parameter of `positional_dominance` allows to distinguish
between dominance under *total heterogeneity* (`map=FALSE`) and *total
homogeneity* (`map=TRUE`). In the later case, all relations can be
ordered non-decreasingly (or non-increasingly if the relation describes
costs, such as distances) and afterwards checked front to back.
Dominance under total homogeneity yields a ranking, if the relation is
binary (e.g. adjacent or not).

``` r

D <- g %>% 
  indirect_relations(type="adjacency") %>% 
  positional_dominance(map=TRUE)

comparable_pairs(D)
```

    ## [1] 1

For cost variables like shortest path distances, the `benefit` parameter
is set to `FALSE`.

``` r

D1 <- g %>% 
  indirect_relations(type="dist_sp") %>% 
  positional_dominance(map=FALSE,benefit=FALSE)
```

From the definition given in the first section, it is clear that there
are always at least as many comparable pairs under the total homogeneity
assumption as under total heterogeneity.

``` r

D1 <- g %>% 
  indirect_relations(type="dist_sp") %>% 
  positional_dominance(map=FALSE,benefit=FALSE)

D2 <- g %>% 
  indirect_relations(type="dist_sp") %>% 
  positional_dominance(map=TRUE,benefit=FALSE)

c("heterogeneity"=comparable_pairs(D1),
  "homogeneity"=comparable_pairs(D2))
```

    ## heterogeneity   homogeneity 
    ##     0.1636364     0.8727273

Additionally, all dominance relations from the heterogeneity assumption
are preserved under total homogeneity. (Note: $`A\implies B`$ is
equivalent to $`\neg A \lor B`$)

``` r

all(D1!=1 | D2==1) 
```

    ## [1] TRUE
