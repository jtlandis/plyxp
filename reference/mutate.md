# Mutate a PlySummarizedExperiment object

Mutate a PlySummarizedExperiment object under an data mask. Unlike a few
other `dplyr` implementations, all contextual evaluations of `mutate()`
for `SummarizedExperiment` are valid.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
mutate(.data, ...)
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

- ...:

  expressions to evaluate

## Value

an object inheriting PlySummarizedExperiment class

## Examples

``` r

mutate(se_simple,
  counts_1 = counts + 1,
  logp_counts = log(counts_1),
  # access assays context with ".assays" pronoun,
  # note that assays are sliced into a list to
  # fit dimensions of cols context
  cols(sum = purrr::map_dbl(.assays$counts, sum)),
  # access assays context "asis" with the same pronoun
  # but with a "_asis" suffix.
  rows(sum = rowSums(.assays_asis$counts))
)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts counts_1 logp_counts | gene  length
#>     <chr>     <chr>    |  <int>     <dbl>    <dbl>       <dbl> | <chr>  <int>
#>   1 row_1     col_1    |     14      2.64       15        2.71 | g1         1
#>   2 row_2     col_1    |     19      2.94       20        3.00 | g2        24
#>   3 row_3     col_1    |     16      2.77       17        2.83 | g3        60
#>   4 row_4     col_1    |     11      2.40       12        2.48 | g4        39
#>   5 row_5     col_1    |     18      2.89       19        2.94 | g5        37
#>   …   …         …             …        …         …          …     …         …
#> n-4 row_1     col_4    |      9      2.20       10        2.30 | g1         1
#> n-3 row_2     col_4    |      4      1.39        5        1.61 | g2        24
#> n-2 row_3     col_4    |     20      3.00       21        3.04 | g3        60
#> n-1 row_4     col_4    |      3      1.10        4        1.39 | g4        39
#> n   row_5     col_4    |      5      1.61        6        1.79 | g5        37
#> # ℹ n = 20
#> # ℹ 6 more variables: direction <chr>, sum <dbl>, `` <>, sample <chr>,
#> #   condition <chr>, sum <dbl>
```
