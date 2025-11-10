# Objects exported from other packages

These objects are imported from other packages. Follow the links below
to see their documentation.

- S4Vectors:

  [`metadata`](https://rdrr.io/pkg/S4Vectors/man/Annotated-class.html),
  [`metadata<-`](https://rdrr.io/pkg/S4Vectors/man/Annotated-class.html)

- SummarizedExperiment:

  [`assay`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`assay<-`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`assays`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`assays<-`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`colData`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`colData<-`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`rowData`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`rowData<-`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
  [`SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)

## Value

exported functions available from `plyxp`

## See also

[`arrange()`](https://jtlandis.github.io/plyxp/reference/arrange.md)
[`mutate()`](https://jtlandis.github.io/plyxp/reference/mutate.md)
[`filter()`](https://jtlandis.github.io/plyxp/reference/filter.md)
[`summarize()`](https://jtlandis.github.io/plyxp/reference/summarize.md)
[`select()`](https://jtlandis.github.io/plyxp/reference/select.md)
[`pull()`](https://jtlandis.github.io/plyxp/reference/pull.md)
[`group_by()`](https://jtlandis.github.io/plyxp/reference/group_by.md)
[`group_data()`](https://jtlandis.github.io/plyxp/reference/group_data.md)
[`group_vars()`](https://jtlandis.github.io/plyxp/reference/group_vars.md)
[`ungroup()`](https://jtlandis.github.io/plyxp/reference/group_data.md)
[`group_split()`](https://jtlandis.github.io/plyxp/reference/group_split.md)

[PlySummarizedExperiment-methods](https://jtlandis.github.io/plyxp/reference/PlySummarizedExperiment-methods.md)

## Examples

``` r
arrange(se_simple, rows(direction)) |>
  mutate(logp_counts = log1p(counts)) |>
  filter(cols(condition == "drug"))
#> # A SummarizedExperiment-tibble Abstraction: 5 × 2
#>    .features .samples | counts logcounts logp_counts | gene  length direction |
#>    <chr>     <chr>    |  <int>     <dbl>       <dbl> | <chr>  <int> <chr>     |
#>  1 row_2     col_3    |      1      0          0.693 | g2        24 +         |
#>  2 row_3     col_3    |     15      2.71       2.77  | g3        60 +         |
#>  3 row_5     col_3    |     12      2.48       2.56  | g5        37 +         |
#>  4 row_1     col_3    |      7      1.95       2.08  | g1         1 -         |
#>  5 row_4     col_3    |     10      2.30       2.40  | g4        39 -         |
#>  6 row_2     col_4    |      4      1.39       1.61  | g2        24 +         |
#>  7 row_3     col_4    |     20      3.00       3.04  | g3        60 +         |
#>  8 row_5     col_4    |      5      1.61       1.79  | g5        37 +         |
#>  9 row_1     col_4    |      9      2.20       2.30  | g1         1 -         |
#> 10 row_4     col_4    |      3      1.10       1.39  | g4        39 -         |
#> # ℹ 2 more variables: sample <chr>, condition <chr>

assays(se_simple)
#> List of length 2
#> names(2): counts logcounts
rowData(se_simple)
#> DataFrame with 5 rows and 3 columns
#>              gene    length   direction
#>       <character> <integer> <character>
#> row_1          g1         1           -
#> row_2          g2        24           +
#> row_3          g3        60           +
#> row_4          g4        39           -
#> row_5          g5        37           +
colData(se_simple)
#> DataFrame with 4 rows and 2 columns
#>            sample   condition
#>       <character> <character>
#> col_1          s1       cntrl
#> col_2          s2       cntrl
#> col_3          s3        drug
#> col_4          s4        drug
```
