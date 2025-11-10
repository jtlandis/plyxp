# SummarizedExperiment Shell Object

A container object for the SummarizedExperiment class.

This S4 class is implemented to bring unique `dplyr` syntax to the
`SummarizedExperiment` object without clashing with the
`tidySummarizedExperiment` package. As such, this is a simple wrapper
that contains one slot, which holds a `SummarizedExperiment` object.

## Usage

``` r
new_plyxp(se)

PlySummarizedExperiment(se)
```

## Arguments

- se:

  SummarizedExperiment object

## Value

PlySummarizedExperiment object

## Slots

- `se`:

  contains the underlying `SummarizedExperiment` class.

## Examples

``` r
se <- SummarizedExperiment(
  assays = list(counts = matrix(1:6, nrow = 3)),
  colData = S4Vectors::DataFrame(condition = c("A", "B"))
)
new_plyxp(se = se)
#> # A SummarizedExperiment-tibble Abstraction: 3 × 2
#>   .features .samples | counts | | condition
#>       <int>    <int> |  <int> | | <chr>    
#> 1         1        1 |      1 | | A        
#> 2         2        1 |      2 | | A        
#> 3         3        1 |      3 | | A        
#> 4         1        2 |      4 | | B        
#> 5         2        2 |      5 | | B        
#> 6         3        2 |      6 | | B        
# or
PlySummarizedExperiment(se = se)
#> # A SummarizedExperiment-tibble Abstraction: 3 × 2
#>   .features .samples | counts | | condition
#>       <int>    <int> |  <int> | | <chr>    
#> 1         1        1 |      1 | | A        
#> 2         2        1 |      2 | | A        
#> 3         3        1 |      3 | | A        
#> 4         1        2 |      4 | | B        
#> 5         2        2 |      5 | | B        
#> 6         3        2 |      6 | | B        
```
