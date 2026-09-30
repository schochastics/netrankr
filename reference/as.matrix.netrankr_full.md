# Extract probabilities from netrankr_full object

extract probabilities as matrices from the result of an object obtained
from
[exact_rank_prob](https://schochastics.github.io/netrankr/reference/exact_rank_prob.md)

## Usage

``` r
# S3 method for class 'netrankr_full'
as.matrix(x, type = "rank", ...)
```

## Arguments

- x:

  A netrankr_full object

- type:

  which probabilities to return. "rank" for rank probabilities,
  "relative" for relative rank probabilities and "expected" for expected
  rank probabilities and their variants

- ...:

  additional parameters for as.matrix

## Author

David Schoch
