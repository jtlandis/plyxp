# Summarize PlySummarizedExperiment

Summarize PlySummarizedExperiment

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
summarize(.data, ..., .retain = c("auto", "ungrouped", "none"))

# S3 method for class 'PlySummarizedExperiment'
summarise(.data, ..., .retain = c("auto", "ungrouped", "none"))
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

- ...:

  expressions to summarize the object

- .retain:

  This argument controls how
  [`rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  or
  [`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  is retained after summarizing. When "auto" (the default), `.retain`
  behavior depends on the groupings of `.data`. When exactly one
  dimension is grouped, "auto" behaves like "ungrouped-dim", and "none"
  otherwise. When "ungrouped-dim", the ungrouped dimension's data are
  retained in the resulting `SummarizedExperiment` object and scalar
  outputs are recycled to the length of the ungrouped dimension. When
  "none", all outputs are expected to be scalar and only computed values
  are retained in
  [`rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  and
  [`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)

## Value

an object inheriting PlySummarizedExperiment class

## Examples

``` r

# outputs in assay context may be either
# length 1, or the length of the ungrouped
# dimension while .retain = "auto"/"ungrouped-dim"
se_simple |>
  group_by(rows(direction)) |>
  summarise(
    col_sums = colSums(counts),
    sample = sample(1:20, 1L)
  )
#> # A SummarizedExperiment-tibble Abstraction: 2 × 4
#> # Groups: rows(direction)
#>   .features .samples | col_sums sample | direction | sample condition
#>       <int> <chr>    |    <dbl>  <int> | <chr>     | <chr>  <chr>    
#> 1         1 col_1    |       25     11 | -         | s1     cntrl    
#> 2         2 col_1    |       53     11 | +         | s1     cntrl    
#> 3         1 col_2    |       25     11 | -         | s2     cntrl    
#> 4         2 col_2    |       21     11 | +         | s2     cntrl    
#> 5         1 col_3    |       17     11 | -         | s3     drug     
#> 6         2 col_3    |       28     11 | +         | s3     drug     
#> 7         1 col_4    |       12     11 | -         | s4     drug     
#> 8         2 col_4    |       29     11 | +         | s4     drug     

# .retain = "none" will drop ungrouped dimensions and
# outputs of assay context should be length 1.
se_simple |>
  group_by(rows(direction)) |>
  summarise(
    col_sums = list(colSums(counts)),
    .retain = "none"
  )
#> # A SummarizedExperiment-tibble Abstraction: 2 × 1
#> # Groups: rows(direction)
#>   .features .samples | col_sums  | direction |
#>       <int>    <int> | <list>    | <chr>     |
#> 1         1        1 | <dbl [4]> | -         |
#> 2         2        1 | <dbl [4]> | +         |

# using an `across()` function will help
# nest ungrouped dimensions
se_simple |>
  group_by(rows(direction)) |>
  summarise(
    col_sums = list(colSums(counts)),
    cols(across(everything(), list)),
    .retain = "none"
  )
#> # A SummarizedExperiment-tibble Abstraction: 2 × 1
#> # Groups: rows(direction)
#>   .features .samples | col_sums  | direction | sample    condition
#>       <int>    <int> | <list>    | <chr>     | <list>    <list>   
#> 1         1        1 | <dbl [4]> | -         | <chr [4]> <chr [4]>
#> 2         2        1 | <dbl [4]> | +         | <chr [4]> <chr [4]>
```
