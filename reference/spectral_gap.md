# Spectral gap of a graph

The spectral (or eigen) gap of a graph is the absolute difference
between the biggest and second biggest eigenvalue of the adjacency
matrix. To compare spectral gaps across networks, the fraction can be
used.

## Usage

``` r
spectral_gap(g, method = "frac")
```

## Arguments

- g:

  igraph object

- method:

  A string, either "frac" or "abs"

## Value

Numeric value

## Details

The spectral gap is bounded between 0 and 1 if `method="frac"`. The
closer the value to one, the bigger the gap.

## Author

David Schoch

## Examples

``` r
# The fractional spectral gap of a threshold graph is usually close to 1
g <- threshold_graph(50, 0.3)
spectral_gap(g, method = "frac")
#> [1] 0.7305571
```
