# Transform indirect relations

Mostly wrapper functions that can be used in conjunction with
[indirect_relations](https://schochastics.github.io/netrankr/reference/indirect_relations.md)
to fine tune indirect relations.

## Usage

``` r
dist_2pow(x)

dist_inv(x)

dist_dpow(x, alpha = 1)

dist_powd(x, alpha = 0.5)

walks_limit_prop(x)

walks_exp(x, alpha = 1)

walks_exp_even(x, alpha = 1)

walks_exp_odd(x, alpha = 1)

walks_attenuated(x, alpha = 1/max(x) * 0.99)

walks_uptok(x, alpha = 1, k = 3)
```

## Arguments

- x:

  Matrix of relations.

- alpha:

  Potential weighting factor.

- k:

  For walk counts up to a certain length.

## Value

Transformed relations as matrix

## Details

The predefined functions follow the naming scheme
`relation_transformation`. Predefined functions `walks_*` are thus best
used with type="walks" in
[indirect_relations](https://schochastics.github.io/netrankr/reference/indirect_relations.md).
Theoretically, however, any transformation can be used with any
relation. The results might, however, not be interpretable.

The following functions are implemented so far:

`dist_2pow` returns \\2^{-x}\\

`dist_inv` returns \\1/x\\

`dist_dpow` returns \\x^{-\alpha}\\ where \\\alpha\\ should be chosen
greater than 0.

`dist_powd` returns \\\alpha^x\\ where \\\alpha\\ should be chosen
between 0 and 1.

`walks_limit_prop` returns the limit proportion of walks between pairs
of nodes. Calculating rowSums of this relation will result in the
principle eigenvector of the network.

`walks_exp` returns \\\sum\_{k=0}^\infty \frac{A^k}{k!}\\

`walks_exp_even` returns \\\sum\_{k=0}^\infty \frac{A^{2k}}{(2k)!}\\

`walks_exp_odd` returns \\\sum\_{k=0}^\infty \frac{A^{2k+1}}{(2k+1)!}\\

`walks_attenuated` returns \\\sum\_{k=0}^\infty \alpha^k A^k\\

`walks_uptok` returns \\\sum\_{j=0}^k \alpha^j A^j\\

Walk based transformation are defined on the eigen decomposition of the
adjacency matrix using the fact that \$\$f(A)=Xf(\Lambda)X^T.\$\$ Care
has to be taken when using user defined functions.

## Author

David Schoch
