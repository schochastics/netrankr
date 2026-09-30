# Hyperbolic (centrality) index

The hyperbolic index is an index that considers all closed walks of even
or odd length on induced neighborhoods of a vertex.

## Usage

``` r
hyperbolic_index(g, type = "odd")
```

## Arguments

- g:

  igraph object.

- type:

  string. 'even' if only even length walks should be considered. 'odd'
  (Default) if only odd length walks should be used.

## Value

A vector containing centrality scores.

## Details

The hyperbolic index is an illustrative index that should not be used
for any serious analysis. Its purpose is to show that with enough
mathematical trickery, any desired result can be obtained when
centrality indices are used.

## Author

David Schoch

## Examples

``` r

library(igraph)

data("dbces11")
hyperbolic_index(dbces11, type = "odd")
#>  [1] 0.000000e+00 0.000000e+00 1.276458e+00 4.218847e-15 9.186479e-01
#>  [6] 3.554977e+00 3.554977e+00 1.925897e+00 1.925897e+00 5.995581e+00
#> [11] 1.501126e+00
hyperbolic_index(dbces11, type = "even")
#>  [1]  3.086161  3.086161  6.848357  3.570911  6.227501 11.664512 11.664512
#>  [8]  8.585124  8.585124 16.291922  7.755459
```
