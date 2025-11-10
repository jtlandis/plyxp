# Get observations of a vector

This extends
[`vctrs::vec_slice`](https://vctrs.r-lib.org/reference/vec_slice.html)
to
[`S4Vectors::Vector`](https://rdrr.io/pkg/S4Vectors/man/Vector-class.html)
class by masking `vec_slice` with
[`S7::new_generic`](https://rconsortium.github.io/S7/reference/new_generic.html).
Atomic vectors and other base S3 classes (list, data.frame, factor, Dat,
POSIXct) will dispatch to the
[`vctrs::vec_slice`](https://vctrs.r-lib.org/reference/vec_slice.html)
method as normal. Dispatch support on the
[`S4Vectors::Vector`](https://rdrr.io/pkg/S4Vectors/man/Vector-class.html)
and
[`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
classes provides a unified framework for working with base R vectors and
`S4Vectors`.

### [`S4Vectors::Vector`](https://rdrr.io/pkg/S4Vectors/man/Vector-class.html) Implementation

This method will naively call the `[` method for any S4 class that
inherits from the
[`S4Vectors::Vector`](https://rdrr.io/pkg/S4Vectors/man/Vector-class.html)
class. This may not be a very efficient way to slice up an S4 class, but
will work.

With this implementation, the `x@mcol` data is expected to be retained
after a call to `plyxp::vec_slice(x, i)`.

### [`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html) Implementation

The `DataFrame` implementation works similar to how
[`vctrs::vec_slice`](https://vctrs.r-lib.org/reference/vec_slice.html)
works on a `data.frame` object. What is being sliced is the rows of
`x@listData`. To maintain the size stability of the `DataFrame` object,
we change `@nrows` to the appropriate value, and perform a recursive
call if `@elementMetadata` is not `NULL`.

### Performance

Depending on the size and complexity of your S4 Vector object, you may
find the standard subset operation is extremely slow. For example,
consider a `SummarizedExperiment` whose rowData contains a
`CompressedGRangesList` object assigned to the name "exons" and whose
length is 250,000 and underlying `@unlistData` is length 1,600,000.
Performing a by `.features` grouping operation and attempting to
evaluate the `exons` within the row context would force the
`CompressedGRangesList` object to be chopped element-wise.

Unfortunately, there is a massive performance hit in attempting to
construct 250,000 `GRanges`. Unless you do not mind waiting over an hour
for each `dplyr` verb in which `exons` gets evaluated, doing so is not
recommended.

The `plyxp` package is planning to export a new generic named
`plyxp_s4_proxy_vec()`. This attempts to reconstruct certain standard
`S4Vectors::Vectors` as standard vectors or tibbles. The equivalent
`exons` object would require much more memory use, but at the advantage
of only taking several seconds to construct. When you are done, you can
attempt to restore the original S4 Vector with
`plyxp_restore_s4_proxy()`.

In development, `plyxp_s4_proxy_vec()` is faster to work with because
there are less checks on the object validity and all `@elementMetadata`
and `@metadata` are dropped from the objects.

## Usage

``` r
vec_slice(x, i, ...)
```

## Arguments

- x:

  A vector

- i:

  An integer, character or logical vector specifying the locations or
  names of the observations to get/set. Specify `TRUE` to index all
  elements (as in `x[]`), or `NULL`, `FALSE` or
  [`integer()`](https://rdrr.io/r/base/integer.html) to index none (as
  in `x[NULL]`).

- ...:

  These dots are for future extensions and must be empty.

## Value

a new S3 or S4 vector subsetted by `i`

## Examples

``` r
vec_slice(1:10, i = 5)
#> [1] 5
vec_slice(S4Vectors::Rle(rep(1:3, each = 3)), i = 5)
#> integer-Rle of length 1 with 1 run
#>   Lengths: 1
#>   Values : 2
```
