# Extra slice functions for a PlySummarizedExperiment

Convenience wrappers around
[`slice()`](https://jtlandis.github.io/plyxp/reference/slice.md) that
select rows and/or columns of a `PlySummarizedExperiment` object:

- `slice_min()` and `slice_max()` select the rows or columns with the
  smallest or largest values of a variable.

- `slice_head()` and `slice_tail()` select the first or last rows or
  columns.

- `slice_sample()` randomly samples rows and/or columns, optionally
  weighted by a variable.

Unlike their data.frame counterparts, every one of these verbs uses the
[`rows()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
and
[`cols()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
context functions within `...` to declare which dimension(s) of the
object are sliced. If an object is grouped, slicing happens within each
group of the respective context.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
slice_max(.data, ..., n = NULL, prop = NULL, with_ties = TRUE, na_rm = FALSE)

# S3 method for class 'PlySummarizedExperiment'
slice_min(.data, ..., n = NULL, prop = NULL, with_ties = TRUE, na_rm = FALSE)

# S3 method for class 'PlySummarizedExperiment'
slice_head(.data, ..., n = NULL, prop = NULL)

# S3 method for class 'PlySummarizedExperiment'
slice_tail(.data, ..., n = NULL, prop = NULL)

# S3 method for class 'PlySummarizedExperiment'
slice_sample(
  .data,
  ...,
  n = NULL,
  prop = NULL,
  weight_by = NULL,
  replace = FALSE
)
```

## Arguments

- .data:

  a `PlySummarizedExperiment` object.

- ...:

  calls to the
  [`rows()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
  and/or
  [`cols()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
  context functions. The
  [`assays()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  context is not available for slicing. The meaning of the context
  contents depends on the verb:

  - `slice_min()`/`slice_max()`: an expression giving the variable to
    order by within that context, e.g. `rows(length)`.

  - `slice_head()`/`slice_tail()`: must be **empty**, e.g.
    [`rows()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
    or
    [`cols()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md).
    They only indicate which context(s) to slice.

  - `slice_sample()`: either empty, indicating a uniform sample of that
    context, or an expression, which is used as the `weight_by` value
    for that context.

- n, prop:

  Provide either `n`, the number of rows/columns, or `prop`, the
  proportion of rows/columns to select (per group). If neither is
  supplied, `n = 1` is used. Supplying both is an error.

  A negative value of `n` or `prop` is subtracted from the group size,
  i.e. `n = -2` keeps all but 2 rows/columns. Positive values are
  rounded down, negative values rounded up, and the result is truncated
  to the group size (unless `slice_sample(replace = TRUE)`).

  Always pass `n`/`prop` by name. Some calls error if they are left
  unnamed or omitted; see the section "Naming `n` and `prop`" below.

- with_ties:

  Should ties be kept together? The default, `TRUE`, may return more
  rows/columns than requested. Use `FALSE` to return exactly `n`
  rows/columns, with ties broken by position.

- na_rm:

  Should missing values be removed? The default, `FALSE`, ranks `NA`
  values last, so they are only kept when there are not enough
  non-missing values to satisfy `n`/`prop`. If `TRUE`, missing values
  are never selected.

- weight_by:

  Sampling weights wrapped in a context function, such as `rows(foo)` or
  `cols(bar)`. Each value must be non-negative. This is an alternative
  to giving a non-empty context in `...`; only one weight may be
  specified per context.

- replace:

  Should sampling be performed with (`TRUE`) or without (`FALSE`, the
  default) replacement? When `TRUE`, `n`/`prop` may exceed the group
  size.

## Value

a `PlySummarizedExperiment` object.

## Details

### Naming `n` and `prop`

Before dispatching to the methods, the dplyr generics check that `n` was
not passed by position. If neither `n` nor `prop` is supplied, they
error when exactly one unnamed argument is left in `...`. For
`slice_min()`/`slice_max()`, the first context fills the generic's
`order_by` argument. As a result, either the
[`rows()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
or the
[`cols()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
context can be used on its own, but they can only be used together when
`n` or `prop` is named. In other words, the following does **not** work:

    slice_min(xp, rows(foo), cols(bar))

but either of the following does:

    slice_min(xp, rows(foo), cols(bar), n = 1)

    xp |>
      slice_min(rows(foo)) |>
      slice_min(cols(bar))

Likewise, `slice_head()`, `slice_tail()` and `slice_sample()` need `n`
or `prop` to be named when only a single context is supplied, e.g.
`slice_head(xp, rows(), n = 1)` rather than `slice_head(xp, rows())`.

### `slice_head()`/`slice_tail()`

For a data.frame, these generics do not use the `...` argument. For
`PlySummarizedExperiment` objects, `...` specifies the
[`rows()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
and/or
[`cols()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
contexts to slice, and these expressions must be empty:

    slice_head(xp, rows(), n = 1)
    slice_tail(xp, cols(), n = 1)
    slice_head(xp, rows(), cols(), n = 2)

### `slice_sample()`

As with `slice_head()`/`slice_tail()`, empty context calls indicate
which contexts to sample. A non-empty context is used as the `weight_by`
argument for that context:

    # uniform sample of both
    slice_sample(xp, rows(), cols(), n = 2)
    # weight rows by `foo`, and cols by `bar`
    slice_sample(xp, rows(foo), cols(bar), n = 2)
    # weight rows by `foo`, uniform cols
    slice_sample(xp, rows(foo), cols(), n = 2)
    # weight rows by `foo`, using the `weight_by` argument
    slice_sample(xp, weight_by = rows(foo), n = 2)

## See also

[`slice()`](https://jtlandis.github.io/plyxp/reference/slice.md),
[`dplyr::slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_head()`](https://dplyr.tidyverse.org/reference/slice.html),
[`dplyr::slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)

## Examples

``` r
# slice_min() / slice_max() ---------------------------------------
slice_max(se_simple, rows(length), n = 2)
#> # A SummarizedExperiment-tibble Abstraction: 2 × 4
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
#> 2 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
#> 3 row_3     col_2    |      6      1.79 | g3        60 +         | s2    
#> 4 row_4     col_2    |     17      2.83 | g4        39 -         | s2    
#> 5 row_3     col_3    |     15      2.71 | g3        60 +         | s3    
#> 6 row_4     col_3    |     10      2.30 | g4        39 -         | s3    
#> 7 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#> 8 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#> # ℹ 1 more variable: condition <chr>
slice_min(se_simple, cols(sample))
#> # A SummarizedExperiment-tibble Abstraction: 5 × 1
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
#> 2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
#> 3 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
#> 4 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
#> 5 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
#> # ℹ 1 more variable: condition <chr>
# name `n` when using both contexts
slice_min(se_simple, rows(length), cols(sample), n = 1)
#> # A SummarizedExperiment-tibble Abstraction: 1 × 1
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
#> # ℹ 1 more variable: condition <chr>

gse <- group_by(se_simple, rows(direction))
slice_max(gse, rows(length), n = 1)
#> # A SummarizedExperiment-tibble Abstraction: 2 × 4
#> # Groups: rows(direction)
#>   .features .samples | counts logcounts | direction gene  length | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>     <chr>  <int> | <chr> 
#> 1 row_4     col_1    |     11      2.40 | -         g4        39 | s1    
#> 2 row_3     col_1    |     16      2.77 | +         g3        60 | s1    
#> 3 row_4     col_2    |     17      2.83 | -         g4        39 | s2    
#> 4 row_3     col_2    |      6      1.79 | +         g3        60 | s2    
#> 5 row_4     col_3    |     10      2.30 | -         g4        39 | s3    
#> 6 row_3     col_3    |     15      2.71 | +         g3        60 | s3    
#> 7 row_4     col_4    |      3      1.10 | -         g4        39 | s4    
#> 8 row_3     col_4    |     20      3.00 | +         g3        60 | s4    
#> # ℹ 1 more variable: condition <chr>

# slice_head() / slice_tail() -------------------------------------
slice_head(se_simple, rows(), n = 1)
#> # A SummarizedExperiment-tibble Abstraction: 1 × 4
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
#> 2 row_1     col_2    |      8      2.08 | g1         1 -         | s2    
#> 3 row_1     col_3    |      7      1.95 | g1         1 -         | s3    
#> 4 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
#> # ℹ 1 more variable: condition <chr>
slice_tail(se_simple, cols(), n = 2)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 2
#>    .features .samples | counts logcounts | gene  length direction | sample
#>    <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#>  1 row_1     col_3    |      7      1.95 | g1         1 -         | s3    
#>  2 row_2     col_3    |      1      0    | g2        24 +         | s3    
#>  3 row_3     col_3    |     15      2.71 | g3        60 +         | s3    
#>  4 row_4     col_3    |     10      2.30 | g4        39 -         | s3    
#>  5 row_5     col_3    |     12      2.48 | g5        37 +         | s3    
#>  6 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
#>  7 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
#>  8 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#>  9 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#> 10 row_5     col_4    |      5      1.61 | g5        37 +         | s4    
#> # ℹ 1 more variable: condition <chr>
# negative `n` drops rows: all but the last 2
slice_head(se_simple, rows(), n = -2)
#> # A SummarizedExperiment-tibble Abstraction: 3 × 4
#>     .features .samples | counts logcounts | gene  length direction | sample
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#>   1 row_1     col_1    |     14     2.64  | g1         1 -         | s1    
#>   2 row_2     col_1    |     19     2.94  | g2        24 +         | s1    
#>   3 row_3     col_1    |     16     2.77  | g3        60 +         | s1    
#>   4 row_1     col_2    |      8     2.08  | g1         1 -         | s2    
#>   5 row_2     col_2    |      2     0.693 | g2        24 +         | s2    
#>   …   …         …             …       …      …         … …            …    
#> n-4 row_2     col_3    |      1     0     | g2        24 +         | s3    
#> n-3 row_3     col_3    |     15     2.71  | g3        60 +         | s3    
#> n-2 row_1     col_4    |      9     2.20  | g1         1 -         | s4    
#> n-1 row_2     col_4    |      4     1.39  | g2        24 +         | s4    
#> n   row_3     col_4    |     20     3.00  | g3        60 +         | s4    
#> # ℹ n = 12
#> # ℹ 1 more variable: condition <chr>
slice_head(se_simple, rows(), cols(), n = 2)
#> # A SummarizedExperiment-tibble Abstraction: 2 × 2
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_1     col_1    |     14     2.64  | g1         1 -         | s1    
#> 2 row_2     col_1    |     19     2.94  | g2        24 +         | s1    
#> 3 row_1     col_2    |      8     2.08  | g1         1 -         | s2    
#> 4 row_2     col_2    |      2     0.693 | g2        24 +         | s2    
#> # ℹ 1 more variable: condition <chr>

# slice_sample() --------------------------------------------------
set.seed(1)
slice_sample(se_simple, rows(), cols(), n = 2)
#> # A SummarizedExperiment-tibble Abstraction: 2 × 2
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
#> 2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
#> 3 row_1     col_3    |      7      1.95 | g1         1 -         | s3    
#> 4 row_2     col_3    |      1      0    | g2        24 +         | s3    
#> # ℹ 1 more variable: condition <chr>
slice_sample(se_simple, rows(length), cols(), n = 2)
#> # A SummarizedExperiment-tibble Abstraction: 2 × 2
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
#> 2 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
#> 3 row_5     col_3    |     12      2.48 | g5        37 +         | s3    
#> 4 row_4     col_3    |     10      2.30 | g4        39 -         | s3    
#> # ℹ 1 more variable: condition <chr>
slice_sample(se_simple, weight_by = rows(length), n = 2)
#> # A SummarizedExperiment-tibble Abstraction: 2 × 4
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
#> 2 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
#> 3 row_3     col_2    |      6      1.79 | g3        60 +         | s2    
#> 4 row_4     col_2    |     17      2.83 | g4        39 -         | s2    
#> 5 row_3     col_3    |     15      2.71 | g3        60 +         | s3    
#> 6 row_4     col_3    |     10      2.30 | g4        39 -         | s3    
#> 7 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#> 8 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#> # ℹ 1 more variable: condition <chr>
```
