# get PlySummarizedExperiment grouping Variables

like in
[`dplyr::group_vars()`](https://dplyr.tidyverse.org/reference/group_data.html)
will get character strings for groupings with the exection of the return
value being a list for each grouped context

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
group_vars(x)
```

## Arguments

- x:

  PlySummarizedExperiment

## Value

NULL or list containing names of grouping columns

## Examples

``` r

out <- group_by(se_simple, rows(direction))
group_vars(out)
#> $row_groups
#> [1] "direction"
#> 
#> $col_groups
#> character(0)
#> 
```
