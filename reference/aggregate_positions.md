# Quantification of (indirect) relations

Function to aggregate positions defined via indirect relations to
construct centrality scores.

## Usage

``` r
aggregate_positions(tau_x, type = "sum")
```

## Arguments

- tau_x:

  Numeric matrix containing indirect relations calculated with
  [indirect_relations](https://schochastics.github.io/netrankr/reference/indirect_relations.md).

- type:

  String indicating the type of aggregation to be used. See Details for
  options.

## Value

Scores for the index defined by the indirect relation `tau_x` and the
used aggregation type.

## Details

The predefined functions are mainly wrappers around base R functions.
type='sum', for instance, is equivalent to
[`rowSums()`](https://rdrr.io/r/base/colSums.html). A non-base functions
is type='invsum' which calculates the inverse of type='sum'. type='self'
is mostly useful for walk based relations, e.g. to count closed walks.
Other self explanatory options are type='mean', type='min', type='max'
and type='prod'.

## See also

[indirect_relations](https://schochastics.github.io/netrankr/reference/indirect_relations.md),
[transform_relations](https://schochastics.github.io/netrankr/reference/transform_relations.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
#> 
#> Attaching package: ‘igraph’
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union
library(magrittr)

data("dbces11")
# degree
dbces11 %>%
    indirect_relations(type = "adjacency") %>%
    aggregate_positions(type = "sum")
#> A B C D E F G H I J K 
#> 1 1 2 2 3 4 4 4 4 4 5 

# closeness centrality
dbces11 %>%
    indirect_relations(type = "dist_sp") %>%
    aggregate_positions(type = "invsum")
#>          A          B          C          D          E          F          G 
#> 0.03703704 0.02941176 0.04000000 0.04000000 0.05000000 0.05882353 0.05263158 
#>          H          I          J          K 
#> 0.05555556 0.05555556 0.05263158 0.05555556 

# betweenness centrality
dbces11 %>%
    indirect_relations(type = "depend_sp") %>%
    aggregate_positions(type = "sum")
#>         A         B         C         D         E         F         G         H 
#>  0.000000  0.000000  0.000000 18.000000  7.666667 19.666667  5.333333 32.666667 
#>         I         J         K 
#> 14.666667  2.666667 29.333333 

# eigenvector centrality
dbces11 %>%
    indirect_relations(type = "walks", FUN = walks_limit_prop) %>%
    aggregate_positions(type = "sum")
#>         A         B         C         D         E         F         G         H 
#> 0.2751607 0.0786437 0.4610603 0.2941027 0.6952060 1.1990379 1.2177246 1.0212076 
#>         I         J         K 
#> 1.1097768 1.2160775 1.0290143 

# subgraph centrality
dbces11 %>%
    indirect_relations(type = "walks", FUN = walks_exp) %>%
    aggregate_positions(type = "self")
#>        A        B        C        D        E        F        G        H 
#> 1.825100 1.595400 3.148571 2.423091 4.387127 7.807257 7.939410 6.672783 
#>        I        J        K 
#> 7.032672 8.242124 7.389559 
```
