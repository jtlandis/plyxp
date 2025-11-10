# Split a PlySummarizedExperiment based on groups

Splits a grouped PlySummarizedExperiment based on groups. Note the
elements of the return value are ungrouped PlySummarizedExperiment
objects.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
group_split(.tbl, ..., .keep = TRUE)
```

## Arguments

- .tbl:

  a PlySummarizedExperiment object

- ...:

  ignored if the `.tbl` is grouped, otherwise it is passed to
  [group_by](https://jtlandis.github.io/plyxp/reference/group_by.md).

- .keep:

  logical indicating of grouping variables should be kept

## Value

A list of PlySummarizedExperiment objects

## Examples

``` r

gse <- group_by(se_simple, rows(direction), cols(condition))
gse |> group_split()
#> [[1]]
#> # A SummarizedExperiment-tibble Abstraction: 2 × 2
#>   .features .samples | counts logcounts | direction gene  length | condition
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>     <chr>  <int> | <chr>    
#> 1 row_1     col_1    |     14      2.64 | -         g1         1 | cntrl    
#> 2 row_4     col_1    |     11      2.40 | -         g4        39 | cntrl    
#> 3 row_1     col_2    |      8      2.08 | -         g1         1 | cntrl    
#> 4 row_4     col_2    |     17      2.83 | -         g4        39 | cntrl    
#> # ℹ 1 more variable: sample <chr>
#> 
#> [[2]]
#> # A SummarizedExperiment-tibble Abstraction: 3 × 2
#>   .features .samples | counts logcounts | direction gene  length | condition
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>     <chr>  <int> | <chr>    
#> 1 row_2     col_1    |     19     2.94  | +         g2        24 | cntrl    
#> 2 row_3     col_1    |     16     2.77  | +         g3        60 | cntrl    
#> 3 row_5     col_1    |     18     2.89  | +         g5        37 | cntrl    
#> 4 row_2     col_2    |      2     0.693 | +         g2        24 | cntrl    
#> 5 row_3     col_2    |      6     1.79  | +         g3        60 | cntrl    
#> 6 row_5     col_2    |     13     2.56  | +         g5        37 | cntrl    
#> # ℹ 1 more variable: sample <chr>
#> 
#> [[3]]
#> # A SummarizedExperiment-tibble Abstraction: 2 × 2
#>   .features .samples | counts logcounts | direction gene  length | condition
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>     <chr>  <int> | <chr>    
#> 1 row_1     col_3    |      7      1.95 | -         g1         1 | drug     
#> 2 row_4     col_3    |     10      2.30 | -         g4        39 | drug     
#> 3 row_1     col_4    |      9      2.20 | -         g1         1 | drug     
#> 4 row_4     col_4    |      3      1.10 | -         g4        39 | drug     
#> # ℹ 1 more variable: sample <chr>
#> 
#> [[4]]
#> # A SummarizedExperiment-tibble Abstraction: 3 × 2
#>   .features .samples | counts logcounts | direction gene  length | condition
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>     <chr>  <int> | <chr>    
#> 1 row_2     col_3    |      1      0    | +         g2        24 | drug     
#> 2 row_3     col_3    |     15      2.71 | +         g3        60 | drug     
#> 3 row_5     col_3    |     12      2.48 | +         g5        37 | drug     
#> 4 row_2     col_4    |      4      1.39 | +         g2        24 | drug     
#> 5 row_3     col_4    |     20      3.00 | +         g3        60 | drug     
#> 6 row_5     col_4    |      5      1.61 | +         g5        37 | drug     
#> # ℹ 1 more variable: sample <chr>
#> 
gse |> group_split(.keep = FALSE)
#> [[1]]
#> # A SummarizedExperiment-tibble Abstraction: 2 × 2
#>   .features .samples | counts logcounts | gene  length | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> | <chr> 
#> 1 row_1     col_1    |     14      2.64 | g1         1 | s1    
#> 2 row_4     col_1    |     11      2.40 | g4        39 | s1    
#> 3 row_1     col_2    |      8      2.08 | g1         1 | s2    
#> 4 row_4     col_2    |     17      2.83 | g4        39 | s2    
#> 
#> [[2]]
#> # A SummarizedExperiment-tibble Abstraction: 3 × 2
#>   .features .samples | counts logcounts | gene  length | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> | <chr> 
#> 1 row_2     col_1    |     19     2.94  | g2        24 | s1    
#> 2 row_3     col_1    |     16     2.77  | g3        60 | s1    
#> 3 row_5     col_1    |     18     2.89  | g5        37 | s1    
#> 4 row_2     col_2    |      2     0.693 | g2        24 | s2    
#> 5 row_3     col_2    |      6     1.79  | g3        60 | s2    
#> 6 row_5     col_2    |     13     2.56  | g5        37 | s2    
#> 
#> [[3]]
#> # A SummarizedExperiment-tibble Abstraction: 2 × 2
#>   .features .samples | counts logcounts | gene  length | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> | <chr> 
#> 1 row_1     col_3    |      7      1.95 | g1         1 | s3    
#> 2 row_4     col_3    |     10      2.30 | g4        39 | s3    
#> 3 row_1     col_4    |      9      2.20 | g1         1 | s4    
#> 4 row_4     col_4    |      3      1.10 | g4        39 | s4    
#> 
#> [[4]]
#> # A SummarizedExperiment-tibble Abstraction: 3 × 2
#>   .features .samples | counts logcounts | gene  length | sample
#>   <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> | <chr> 
#> 1 row_2     col_3    |      1      0    | g2        24 | s3    
#> 2 row_3     col_3    |     15      2.71 | g3        60 | s3    
#> 3 row_5     col_3    |     12      2.48 | g5        37 | s3    
#> 4 row_2     col_4    |      4      1.39 | g2        24 | s4    
#> 5 row_3     col_4    |     20      3.00 | g3        60 | s4    
#> 6 row_5     col_4    |      5      1.61 | g5        37 | s4    
#> 
```
