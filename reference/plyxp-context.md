# plyxp contexts

Contextual user-facing helper function for dplyr verbs with
SummarizedExperiment objects. These functions are intended to be used as
the top level call to any dplyr verbs `...` argument, similar to that of
`across()`/`if_any()`/`if_all()`.

Specifies that the following expressions should be evaluated within the
colData context.

Specifies that the following expressions should be evaluated within the
rowData context.

Specify a single expression to evaluate in another context

Specify a single expression to evaluate in another context

Specify a single expression to evaluate in another context

## Usage

``` r
cols(...)

rows(...)

col_ctx(x, asis = FALSE)

row_ctx(x, asis = FALSE)

assay_ctx(x, asis = FALSE)
```

## Arguments

- x, ...:

  expressions to evaluate within its associated context

- asis:

  asis = FALSE (the default) will indicate using active bindings that
  attempt to coerce the underlying data into a format that is
  appropriate for the current context. Indicating TRUE will instead bind
  the underlying data as is.

## Value

function called for its side-effects

## Examples

``` r

# cols
mutate(se_simple,
  cols(is_drug = condition == "drug"),
  # bind a different context
  effect = col_ctx(counts + (is_drug * rbinom(n(), 20, .3)))
)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts effect | gene  length direction |
#>     <chr>     <chr>    |  <int>     <dbl>  <int> | <chr>  <int> <chr>     |
#>   1 row_1     col_1    |     14      2.64     14 | g1         1 -         |
#>   2 row_2     col_1    |     19      2.94     19 | g2        24 +         |
#>   3 row_3     col_1    |     16      2.77     16 | g3        60 +         |
#>   4 row_4     col_1    |     11      2.40     11 | g4        39 -         |
#>   5 row_5     col_1    |     18      2.89     18 | g5        37 +         |
#>   …   …         …             …        …       …    …         … …          
#> n-4 row_1     col_4    |      9      2.20     13 | g1         1 -         |
#> n-3 row_2     col_4    |      4      1.39     11 | g2        24 +         |
#> n-2 row_3     col_4    |     20      3.00     26 | g3        60 +         |
#> n-1 row_4     col_4    |      3      1.10     10 | g4        39 -         |
#> n   row_5     col_4    |      5      1.61     12 | g5        37 +         |
#> # ℹ n = 20
#> # ℹ 3 more variables: sample <chr>, condition <chr>, is_drug <lgl>
```
