# chop a vector

A re-export of
[`vctrs::vec_chop`](https://vctrs.r-lib.org/reference/vec_chop.html) as
an S7 generic function to allow `S4Vectors`.

## Usage

``` r
vec_chop(x, ..., indices = NULL)
```

## Arguments

- x:

  A vector

- ...:

  These dots are for future extensions and must be empty.

- indices:

  For `vec_chop()`, a list of positive integer vectors to slice `x`
  with, or `NULL`. Can't be used if `sizes` is already specified. If
  both `indices` and `sizes` are `NULL`, `x` is split into its
  individual elements, equivalent to using an `indices` of
  `as.list(vec_seq_along(x))`.

  For
  [`list_unchop()`](https://jtlandis.github.io/plyxp/reference/list_unchop.md),
  a list of positive integer vectors specifying the locations to place
  elements of `x` in. Each element of `x` is recycled to the size of the
  corresponding index vector. The size of `indices` must match the size
  of `x`. If `NULL`, `x` is combined in the order it is provided in,
  which is equivalent to using
  [`vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html).

## Value

a S3 or S4 vector

## Examples

``` r
vec_chop(1L)
#> [[1]]
#> [1] 1
#> 
vec_chop(S4Vectors::Rle(c(rep(1, 3), rep(4, 5))), indices = list(c(2, 3, 4), c(1, 5:8)))
#> [[1]]
#> numeric-Rle of length 3 with 2 runs
#>   Lengths: 2 1
#>   Values : 1 4
#> 
#> [[2]]
#> numeric-Rle of length 5 with 2 runs
#>   Lengths: 1 4
#>   Values : 1 4
#> 
```
