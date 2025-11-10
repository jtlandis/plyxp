# replicate a vector

A re-export of
[`vctrs::vec_rep`](https://vctrs.r-lib.org/reference/vec-rep.html) and
[`vctrs::vec_rep_each`](https://vctrs.r-lib.org/reference/vec-rep.html)
as an S7 generic function to allow `S4Vectors`.

## Usage

``` r
vec_rep(
  x,
  times,
  ...,
  error_call = caller_env(),
  x_arg = "x",
  times_arg = "times"
)

vec_rep_each(
  x,
  times,
  ...,
  error_call = caller_env(),
  x_arg = "x",
  times_arg = "times"
)
```

## Arguments

- x:

  A vector.

- times:

  For `vec_rep()`, a single integer for the number of times to repeat
  the entire vector.

  For `vec_rep_each()`, an integer vector of the number of times to
  repeat each element of `x`. `times` will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `x`.

- ...:

  These dots are for future extensions and must be empty.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- x_arg, times_arg:

  Argument names for errors.

## Value

a new S3 or S4 vector replicated by specified times

## Examples

``` r
vec_rep(1:2, times = 5)
#>  [1] 1 2 1 2 1 2 1 2 1 2
vec_rep(S4Vectors::Rle(1:2), times = 5)
#> integer-Rle of length 10 with 10 runs
#>   Lengths: 1 1 1 1 1 1 1 1 1 1
#>   Values : 1 2 1 2 1 2 1 2 1 2

vec_rep_each(1:2, times = 5)
#>  [1] 1 1 1 1 1 2 2 2 2 2
vec_rep_each(S4Vectors::Rle(1:2), times = 5)
#> integer-Rle of length 10 with 2 runs
#>   Lengths: 5 5
#>   Values : 1 2
```
