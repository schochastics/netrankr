# Count occurrences of pairs in rankings

Counts the number of concordant, discordant and (left/right) ties
between two rankings.

## Usage

``` r
compare_ranks(x, y)
```

## Arguments

- x:

  A numeric vector.

- y:

  A numeric vector with the same length as `x`.

## Value

A list containing

- concordant:

  number of concordant pairs: `x[i]` \> `x[j]` and `y[i]` \> `y[j]`

- discordant:

  number of discordant pairs: `x[i]` \> `x[j]` and `y[i]` \< `y[j]`

- ties:

  number of tied pairs: `x[i]` == `x[j]` and `y[i]` == `y[j]`

- left:

  number of left ties: `x[i]` == `x[j]` and `y[i]` != `y[j]`

- right:

  number of right ties: `x[i]` != `x[j]` and `y[i]` == `y[j]`

## Details

Explicitly calculating the number of occurring cases is more robust than
using correlation indices as given in the `cor` function. Especially
left and right ties can significantly alter correlations.

## Author

David Schoch

## Examples

``` r
library(igraph)
tg <- threshold_graph(100, 0.2)
compare_ranks(degree(tg), closeness(tg)) # only concordant pairs
#> $concordant
#> [1] 4708
#> 
#> $discordant
#> [1] 0
#> 
#> $ties
#> [1] 242
#> 
#> $left
#> [1] 0
#> 
#> $right
#> [1] 0
#> 
compare_ranks(degree(tg), betweenness(tg)) # no discordant pairs
#> $concordant
#> [1] 1865
#> 
#> $discordant
#> [1] 0
#> 
#> $ties
#> [1] 242
#> 
#> $left
#> [1] 0
#> 
#> $right
#> [1] 2843
#> 
## Rank Correlation
cor(degree(tg), closeness(tg), method = "kendall") # 1
#> [1] 1
cor(degree(tg), betweenness(tg), method = "kendall") # not 1, although no discordant pairs
#> [1] 0.629392
```
