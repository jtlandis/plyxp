# extract data from object

similar to `dplyr::pull.data.frame` except allows to extract objects
from different contexts.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
pull(.data, var = -1, name = NULL, ...)
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

- var:

  A variable as specified by
  [dplyr::pull](https://dplyr.tidyverse.org/reference/pull.html)

- name:

  ignored argument. Due to the range of data types a
  `PlySummarizedExperiment` this argument is not supported

- ...:

  unused argument

## Value

an element from either the assays, rowData, or colData of a
`SummarizedExperiment` object

## Examples

``` r

# last element of default context (assays)
pull(se_simple, var = -1)
#>          col_1     col_2    col_3    col_4
#> row_1 2.639057 2.0794415 1.945910 2.197225
#> row_2 2.944439 0.6931472 0.000000 1.386294
#> row_3 2.772589 1.7917595 2.708050 2.995732
#> row_4 2.397895 2.8332133 2.302585 1.098612
#> row_5 2.890372 2.5649494 2.484907 1.609438
# first element of rows context
pull(se_simple, var = rows(1))
#> [1] "g1" "g2" "g3" "g4" "g5"
# element from col context by literal variable name
pull(se_simple, var = cols(condition))
#> [1] "cntrl" "cntrl" "drug"  "drug" 

# use `pull()` to return contextual info
mutate(se_simple, rows(counts = .assays$counts)) |>
  # get last stored element
  pull(rows(-1))
#> [[1]]
#> col_1 col_2 col_3 col_4 
#>    14     8     7     9 
#> 
#> [[2]]
#> col_1 col_2 col_3 col_4 
#>    19     2     1     4 
#> 
#> [[3]]
#> col_1 col_2 col_3 col_4 
#>    16     6    15    20 
#> 
#> [[4]]
#> col_1 col_2 col_3 col_4 
#>    11    17    10     3 
#> 
#> [[5]]
#> col_1 col_2 col_3 col_4 
#>    18    13    12     5 
#> 
```
