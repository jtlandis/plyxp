# Recycle a vector

A re-export of
[`vctrs::vec_recycle`](https://vctrs.r-lib.org/reference/vec_recycle.html)
as an S7 generic function to allow `S4Vectors`.

## Usage

``` r
vec_recycle(x, size, ..., x_arg = "", call = caller_env())
```

## Arguments

- x:

  A vector to recycle.

- size:

  Desired output size.

- ...:

  Depending on the function used:

  - For `vec_recycle_common()`, vectors to recycle.

  - For `vec_recycle()`, these dots should be empty.

- x_arg:

  Argument name for `x`. These are used in error messages to inform the
  user about which argument has an incompatible size.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

a S3 or S4 vector

## Examples

``` r
vec_recycle(1L, size = 5L)
#> [1] 1 1 1 1 1
vec_recycle(S4Vectors::Rle(1L), size = 5L)
#> integer-Rle of length 5 with 1 run
#>   Lengths: 5
#>   Values : 1
```
