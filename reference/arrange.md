# arrange rows or columns of PlySummarizedExperiment

`arrange()` orders either the rows or columns of a
`PlySummarizedExperiment` object. Note, to guarentee a valid
`PlySummarizedExperiment` is returned, arranging in the `assays`
evaluation context is disabled.

Unlike other dplyr verbs, `arrange()` largely ignores grouping. The
`PlySummarizedExperiment` method also provides the same functionality
via the `.by_group` argument.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
arrange(.data, ..., .by_group = FALSE)
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .by_group:

  If `TRUE`, will sort first by grouping variable. Applies to grouped
  data frames only.

## Value

an object inheriting `PlySummarizedExperiment` class

## Examples

``` r

# arrange within rows/cols contexts separately
arrange(
  se_simple,
  rows(direction),
  cols(dplyr::desc(condition))
)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | gene  length direction | sample
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#>   1 row_2     col_3    |      1     0     | g2        24 +         | s3    
#>   2 row_3     col_3    |     15     2.71  | g3        60 +         | s3    
#>   3 row_5     col_3    |     12     2.48  | g5        37 +         | s3    
#>   4 row_1     col_3    |      7     1.95  | g1         1 -         | s3    
#>   5 row_4     col_3    |     10     2.30  | g4        39 -         | s3    
#>   …   …         …             …       …      …         … …            …    
#> n-4 row_2     col_2    |      2     0.693 | g2        24 +         | s2    
#> n-3 row_3     col_2    |      6     1.79  | g3        60 +         | s2    
#> n-2 row_5     col_2    |     13     2.56  | g5        37 +         | s2    
#> n-1 row_1     col_2    |      8     2.08  | g1         1 -         | s2    
#> n   row_4     col_2    |     17     2.83  | g4        39 -         | s2    
#> # ℹ n = 20
#> # ℹ 1 more variable: condition <chr>

# access assay data to compute arrangement
arrange(
  se_simple,
  rows(rowSums(.assays_asis$counts)),
  cols(colSums(.assays_asis$counts))
)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | gene  length direction | sample
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#>   1 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
#>   2 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
#>   3 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#>   4 row_5     col_4    |      5      1.61 | g5        37 +         | s4    
#>   5 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#>   …   …         …             …        …     …         … …            …    
#> n-4 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
#> n-3 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
#> n-2 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
#> n-1 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
#> n   row_3     col_1    |     16      2.77 | g3        60 +         | s1    
#> # ℹ n = 20
#> # ℹ 1 more variable: condition <chr>

# assay context is disabled
arrange(se_simple, counts) |> try()
#> Error in arrange(se_simple, counts) : 
#> Caused by error in `arrange_se_impl()`:
#> ! Cannot arrange in `assays` context
#> ✖ review expression: ..1
#> ℹ consider wrapping expressions in rows(...) or cols(...)

# convert to `data.frame` first
as.data.frame(se_simple) |>
  arrange(counts)
#>    .features .samples counts logcounts gene length direction sample condition
#> 1      row_2    col_3      1 0.0000000   g2     24         +     s3      drug
#> 2      row_2    col_2      2 0.6931472   g2     24         +     s2     cntrl
#> 3      row_4    col_4      3 1.0986123   g4     39         -     s4      drug
#> 4      row_2    col_4      4 1.3862944   g2     24         +     s4      drug
#> 5      row_5    col_4      5 1.6094379   g5     37         +     s4      drug
#> 6      row_3    col_2      6 1.7917595   g3     60         +     s2     cntrl
#> 7      row_1    col_3      7 1.9459101   g1      1         -     s3      drug
#> 8      row_1    col_2      8 2.0794415   g1      1         -     s2     cntrl
#> 9      row_1    col_4      9 2.1972246   g1      1         -     s4      drug
#> 10     row_4    col_3     10 2.3025851   g4     39         -     s3      drug
#> 11     row_4    col_1     11 2.3978953   g4     39         -     s1     cntrl
#> 12     row_5    col_3     12 2.4849066   g5     37         +     s3      drug
#> 13     row_5    col_2     13 2.5649494   g5     37         +     s2     cntrl
#> 14     row_1    col_1     14 2.6390573   g1      1         -     s1     cntrl
#> 15     row_3    col_3     15 2.7080502   g3     60         +     s3      drug
#> 16     row_3    col_1     16 2.7725887   g3     60         +     s1     cntrl
#> 17     row_4    col_2     17 2.8332133   g4     39         -     s2     cntrl
#> 18     row_5    col_1     18 2.8903718   g5     37         +     s1     cntrl
#> 19     row_2    col_1     19 2.9444390   g2     24         +     s1     cntrl
#> 20     row_3    col_4     20 2.9957323   g3     60         +     s4      drug
```
