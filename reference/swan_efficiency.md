# Impact on farness when a node is removed

`swan_efficiency` measures the change in the sum of distances between
all node pairs when excluding a node from the network.

## Usage

``` r
swan_efficiency(g)
```

## Arguments

- g:

  An `igraph` object representing the graph to analyze.
  `swan_efficiency` is based on geographic accessibility, similar to
  indices used for assessing transportation network performance, such as
  closeness accessibility. It quantifies the impact of node removal by
  calculating the change in the sum of distances between all node pairs.

  The code is an adaptation from the NetSwan package that was archived
  on CRAN.

## Value

A numeric vector where each entry represents the `swan_efficiency` value
for the corresponding node.

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

# Compute efficiency impact of node removal
f2 <- swan_efficiency(gra)
bet <- betweenness(gra)
reg <- lm(bet ~ f2)
summary(reg)
#> 
#> Call:
#> lm(formula = bet ~ f2)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -1.0956 -0.5924  0.1723  0.4648  1.3389 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.98463    0.38203   2.577   0.0298 *  
#> f2           0.39052    0.01891  20.648 6.85e-09 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.8021 on 9 degrees of freedom
#> Multiple R-squared:  0.9793, Adjusted R-squared:  0.977 
#> F-statistic: 426.3 on 1 and 9 DF,  p-value: 6.854e-09
#> 
```
