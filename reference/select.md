# select assays, rowData, and colData names

Select one or more values from each context. By default omitting an
expression for a context is the same as selecting NOTHING from that
context.

The
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
implementation within `plyxp` is almost similar to `dplyr` except when
used within the `across()` function. When used from `accross()`, the
data provided to
[eval_select](https://tidyselect.r-lib.org/reference/eval_select.html)
is a zero length slice of the data. This was an intentional choice to
prevent the evaluation of potentionally expensive chopping operations
for S4Vectors. This means that predicate function from
[`where()`](https://tidyselect.r-lib.org/reference/where.html) will NOT
be able to query the original data.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
select(.data, ...)
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  one or more selection expressions. Supports wrapping expressions
  within the
  \<[`plyxp-contexts`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)\>.

## Value

an object inheriting PlySummarizedExperiment class

## Examples

``` r


# only keep assays, other contexts are dropped
select(se_simple, everything())
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | |
#>     <chr>     <chr>    |  <int>     <dbl> | |
#>   1 row_1     col_1    |     14      2.64 | |
#>   2 row_2     col_1    |     19      2.94 | |
#>   3 row_3     col_1    |     16      2.77 | |
#>   4 row_4     col_1    |     11      2.40 | |
#>   5 row_5     col_1    |     18      2.89 | |
#>   …   …         …             …        …     
#> n-4 row_1     col_4    |      9      2.20 | |
#> n-3 row_2     col_4    |      4      1.39 | |
#> n-2 row_3     col_4    |     20      3.00 | |
#> n-1 row_4     col_4    |      3      1.10 | |
#> n   row_5     col_4    |      5      1.61 | |
#> # ℹ n = 20

# only keep rowData, other contexts are dropped
select(se_simple, rows(everything()))
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | | gene  length direction |
#>     <chr>     <chr>    | | <chr>  <int> <chr>     |
#>   1 row_1     col_1    | | g1         1 -         |
#>   2 row_2     col_1    | | g2        24 +         |
#>   3 row_3     col_1    | | g3        60 +         |
#>   4 row_4     col_1    | | g4        39 -         |
#>   5 row_5     col_1    | | g5        37 +         |
#>   …   …         …           …         … …          
#> n-4 row_1     col_4    | | g1         1 -         |
#> n-3 row_2     col_4    | | g2        24 +         |
#> n-2 row_3     col_4    | | g3        60 +         |
#> n-1 row_4     col_4    | | g4        39 -         |
#> n   row_5     col_4    | | g5        37 +         |
#> # ℹ n = 20

select(se_simple, rows(where(is.numeric)))
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | | length |
#>     <chr>     <chr>    | |  <int> |
#>   1 row_1     col_1    | |      1 |
#>   2 row_2     col_1    | |     24 |
#>   3 row_3     col_1    | |     60 |
#>   4 row_4     col_1    | |     39 |
#>   5 row_5     col_1    | |     37 |
#>   …   …         …               …  
#> n-4 row_1     col_4    | |      1 |
#> n-3 row_2     col_4    | |     24 |
#> n-2 row_3     col_4    | |     60 |
#> n-1 row_4     col_4    | |     39 |
#> n   row_5     col_4    | |     37 |
#> # ℹ n = 20

# Note on `where()` clause, all data is available within select
select(se_simple, rows(where(~ any(grepl("-", .x)))))
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | | direction |
#>     <chr>     <chr>    | | <chr>     |
#>   1 row_1     col_1    | | -         |
#>   2 row_2     col_1    | | +         |
#>   3 row_3     col_1    | | +         |
#>   4 row_4     col_1    | | -         |
#>   5 row_5     col_1    | | +         |
#>   …   …         …          …          
#> n-4 row_1     col_4    | | -         |
#> n-3 row_2     col_4    | | +         |
#> n-2 row_3     col_4    | | +         |
#> n-1 row_4     col_4    | | -         |
#> n   row_5     col_4    | | +         |
#> # ℹ n = 20

# within an `across()`, only a zero-length slice avialble, so the
# `where()` predicate cannot access the data
mutate(
  se_simple,
  rows(
    across(
      where(~ any(grepl("-", .x))),
      ~ sprintf("%s foo", .x)
    )
  )
)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | gene  length direction | sample
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#>   1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
#>   2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
#>   3 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
#>   4 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
#>   5 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
#>   …   …         …             …        …     …         … …            …    
#> n-4 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
#> n-3 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
#> n-2 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#> n-1 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#> n   row_5     col_4    |      5      1.61 | g5        37 +         | s4    
#> # ℹ n = 20
#> # ℹ 1 more variable: condition <chr>
# here is an acceptable usage of the `where()` predicate
mutate(
  se_simple,
  rows(
    across(
      where(is.character),
      ~ sprintf("%s foo", .x)
    )
  )
)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | gene   length direction | sample
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>   <int> <chr>     | <chr> 
#>   1 row_1 foo col_1    |     14      2.64 | g1 foo      1 - foo     | s1    
#>   2 row_2 foo col_1    |     19      2.94 | g2 foo     24 + foo     | s1    
#>   3 row_3 foo col_1    |     16      2.77 | g3 foo     60 + foo     | s1    
#>   4 row_4 foo col_1    |     11      2.40 | g4 foo     39 - foo     | s1    
#>   5 row_5 foo col_1    |     18      2.89 | g5 foo     37 + foo     | s1    
#>   …   …    …    …             …        …     …  …       … …  …         …    
#> n-4 row_1 foo col_4    |      9      2.20 | g1 foo      1 - foo     | s4    
#> n-3 row_2 foo col_4    |      4      1.39 | g2 foo     24 + foo     | s4    
#> n-2 row_3 foo col_4    |     20      3.00 | g3 foo     60 + foo     | s4    
#> n-1 row_4 foo col_4    |      3      1.10 | g4 foo     39 - foo     | s4    
#> n   row_5 foo col_4    |      5      1.61 | g5 foo     37 + foo     | s4    
#> # ℹ n = 20
#> # ℹ 1 more variable: condition <chr>
```
