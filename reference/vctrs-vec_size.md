# extract the size of a vector

A re-export of
[`vctrs::vec_size`](https://vctrs.r-lib.org/reference/vec_size.html) and
as an S7 generic function to allow `S4Vectors`.

## Usage

``` r
vec_size(x)
```

## Arguments

- x:

  A vector

## Value

size of the S3 or S4 vector

## Examples

``` r
vec_size(1:2)
#> [1] 2
vec_size(S4Vectors::Rle(rep(1:2, each = 5)))
#> [1] 10
```
