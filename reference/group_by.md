# apply groups to PlySummarizedExperiment

create grouping variables about the rowData and colData of a
`PlySummarizedExperiment` object. Unlike the `data.frame` method the
resulting output class is left unchanged. Thus `dplyr` generics for
`PlySummarizedExperiment` must check grouping information manually.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
group_by(.data, ..., .add = FALSE)

# S3 method for class 'PlySummarizedExperiment'
ungroup(x, ...)
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

  ### S4 Compatibility

  At the moment, grouping on S4 Vectors is not yet supported. This is
  due to `plyxp` using `[vec_group_loc][vctrs::vec_group_loc]` to form
  grouping information. `plyxp` will eventually develop a method to
  handle S4 Vectors.

- ...:

  [contextual
  expressions](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
  specifying which columns to ungroup. Omitting `...` ungroups the
  entire object.

- .add:

  When `FALSE`, the default, `group_by()` will override existing groups.

- x:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

## Value

`PlySummarizedExperiment` object

## Functions

- `ungroup(PlySummarizedExperiment)`: Ungroup a PlySummarizedExperiment
  object

## Examples

``` r

group_by(se_simple, rows(direction), cols(condition))
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#> # Groups: rows(direction), cols(condition)
#>     .features .samples | counts logcounts | direction gene  length | condition
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>     <chr>  <int> | <chr>    
#>   1 row_1     col_1    |     14      2.64 | -         g1         1 | cntrl    
#>   2 row_2     col_1    |     19      2.94 | +         g2        24 | cntrl    
#>   3 row_3     col_1    |     16      2.77 | +         g3        60 | cntrl    
#>   4 row_4     col_1    |     11      2.40 | -         g4        39 | cntrl    
#>   5 row_5     col_1    |     18      2.89 | +         g5        37 | cntrl    
#>   …   …         …             …        …    …          …         …     …      
#> n-4 row_1     col_4    |      9      2.20 | -         g1         1 | drug     
#> n-3 row_2     col_4    |      4      1.39 | +         g2        24 | drug     
#> n-2 row_3     col_4    |     20      3.00 | +         g3        60 | drug     
#> n-1 row_4     col_4    |      3      1.10 | -         g4        39 | drug     
#> n   row_5     col_4    |      5      1.61 | +         g5        37 | drug     
#> # ℹ n = 20
#> # ℹ 1 more variable: sample <chr>
```
