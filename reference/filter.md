# filter PlySummarizedExperiment

The `filter()` function is used to subset an object, returning the
observations that satisfy your conditions. An observation must return
TRUE for all conditions within a context to be retained. Note, to
guarantee a valid `PlySummarizedExperiment` is returned, filtering in
the `assays` evaluation context is disabled.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
filter(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

- ...:

  conditions to filter on. These must be wrapped in
  [`cols()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
  and or
  [`rows()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)

- .preserve:

  Relevant when the .data input is grouped. If .preserve = FALSE (the
  default), the grouping structure is recalculated based on the
  resulting data, i.e. the number of groups may change.

## Value

an object inheriting `PlySummarizedExperiment` class

## Examples

``` r
# example code
filter(
  se_simple,
  rows(length > 30),
  cols(condition == "drug")
)
#> # A SummarizedExperiment-tibble Abstraction: 3 × 2
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_3     col_3    |     15      2.71 | g3        60 +         | s3    
#> 2 row_4     col_3    |     10      2.30 | g4        39 -         | s3    
#> 3 row_5     col_3    |     12      2.48 | g5        37 +         | s3    
#> 4 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#> 5 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#> 6 row_5     col_4    |      5      1.61 | g5        37 +         | s4    
#> # ℹ 1 more variable: condition <chr>

filter(
  se_simple,
  rows(rowSums(.assays_asis$counts) > 40),
  cols(colSums(.assays_asis$counts) < 50)
)
#> # A SummarizedExperiment-tibble Abstraction: 3 × 3
#>   .features .samples | counts logcounts | gene  length direction | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#> 1 row_3     col_2    |      6      1.79 | g3        60 +         | s2    
#> 2 row_4     col_2    |     17      2.83 | g4        39 -         | s2    
#> 3 row_5     col_2    |     13      2.56 | g5        37 +         | s2    
#> 4 row_3     col_3    |     15      2.71 | g3        60 +         | s3    
#> 5 row_4     col_3    |     10      2.30 | g4        39 -         | s3    
#> 6 row_5     col_3    |     12      2.48 | g5        37 +         | s3    
#> 7 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#> 8 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#> 9 row_5     col_4    |      5      1.61 | g5        37 +         | s4    
#> # ℹ 1 more variable: condition <chr>

# assay context is disabled
filter(
  se_simple,
  counts > 12
) |> try()
#> Error in filter(se_simple, counts > 12) : 
#> Caused by error in `filter_se_impl()`:
#> ! Cannot filter in `assays` context
#> ✖ review expression: ..1
#> ℹ consider wrapping expressions in rows(...) or cols(...)

# convert to `data.frame` first
as.data.frame(se_simple) |>
  filter(counts > 12)
#>   .features .samples counts logcounts gene length direction sample condition
#> 1     row_1    col_1     14  2.639057   g1      1         -     s1     cntrl
#> 2     row_2    col_1     19  2.944439   g2     24         +     s1     cntrl
#> 3     row_3    col_1     16  2.772589   g3     60         +     s1     cntrl
#> 4     row_5    col_1     18  2.890372   g5     37         +     s1     cntrl
#> 5     row_4    col_2     17  2.833213   g4     39         -     s2     cntrl
#> 6     row_5    col_2     13  2.564949   g5     37         +     s2     cntrl
#> 7     row_3    col_3     15  2.708050   g3     60         +     s3      drug
#> 8     row_3    col_4     20  2.995732   g3     60         +     s4      drug
```
