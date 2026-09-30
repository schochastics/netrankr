# Impact on closeness when a node is removed

`swan_closeness` measures the change in the sum of the inverse of
distances between all node pairs when excluding that node.

## Usage

``` r
swan_closeness(g)
```

## Arguments

- g:

  An `igraph` object representing the graph to analyze.

## Value

A numeric vector containing the `swan_closeness` values for all
vertices.

## Details

`swan_closeness` measures the impact of a node's removal by computing
the change in the sum of inverse distances between all node pairs.

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

# Compute swan_closeness
f2 <- swan_closeness(gra)

# Compare with betweenness centrality
bet <- betweenness(gra)
reg <- lm(bet ~ f2)
summary(reg)
#> 
#> Call:
#> lm(formula = bet ~ f2)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -1.6235 -0.3692  0.1222  0.4552  1.2889 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  -0.2541     0.4486  -0.566    0.585    
#> f2           -6.0531     0.3058 -19.791 9.96e-09 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.836 on 9 degrees of freedom
#> Multiple R-squared:  0.9775, Adjusted R-squared:  0.975 
#> F-statistic: 391.7 on 1 and 9 DF,  p-value: 9.959e-09
#> 
```
