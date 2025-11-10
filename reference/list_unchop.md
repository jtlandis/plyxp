# unchop a list of objects

A generic version of
[`vctrs::list_unchop`](https://vctrs.r-lib.org/reference/vec_chop.html)
meant to support S4 Vectors.

## Usage

``` r
list_unchop(x, ptype = NULL, ..., indices = NULL)
```

## Arguments

- x:

  a list

- ptype:

  the expected prototype of the output

- ...:

  unused arguments

- indices:

  optional list of integer vectors whose size is equal to that of x.
  This maps the the final index of each element in the output.

## Value

an object of type `ptype` or the common ptype of elements of x.
