# contextual plyxp pronouns

`plyxp` utilizes its own version of
[`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html)
pronouns. These may be used to gain access to other evaluation contexts
for a managed set of data-masks.

Similar to
[`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html),
`plyxp::.assays` and other exported pronouns are exported to pass R CMD
Checks. When using a `plyxp` within your package, import the associated
pronoun from `plyxp` but only use the fully unqualified name, `.assays`,
`.assays_asis`, etc.

## Usage

``` r
.assays

.assays_asis

.rows

.rows_asis

.cols

.cols_asis
```

## Format

An object of class `NULL` of length 0.

An object of class `NULL` of length 0.

An object of class `NULL` of length 0.

An object of class `NULL` of length 0.

An object of class `NULL` of length 0.

An object of class `NULL` of length 0.

## Value

access to specific values behind the rlang pronoun

## Examples

``` r
mutate(
  se_simple,
  # access via pronoun
  rows(sum = rowSums(.assays_asis$counts)),
  cols(sum = vapply(.assays$counts, sum, numeric(1)))
)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | gene  length direction   sum |
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     <dbl> |
#>   1 row_1     col_1    |     14      2.64 | g1         1 -            38 |
#>   2 row_2     col_1    |     19      2.94 | g2        24 +            26 |
#>   3 row_3     col_1    |     16      2.77 | g3        60 +            57 |
#>   4 row_4     col_1    |     11      2.40 | g4        39 -            41 |
#>   5 row_5     col_1    |     18      2.89 | g5        37 +            48 |
#>   …   …         …             …        …     …         … …             …  
#> n-4 row_1     col_4    |      9      2.20 | g1         1 -            38 |
#> n-3 row_2     col_4    |      4      1.39 | g2        24 +            26 |
#> n-2 row_3     col_4    |     20      3.00 | g3        60 +            57 |
#> n-1 row_4     col_4    |      3      1.10 | g4        39 -            41 |
#> n   row_5     col_4    |      5      1.61 | g5        37 +            48 |
#> # ℹ n = 20
#> # ℹ 3 more variables: sample <chr>, condition <chr>, sum <dbl>
```
