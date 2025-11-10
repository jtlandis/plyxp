# get grouping data

retrieve grouping information from a `SummarizedExperiment` object. This
is stored within the
[`metadata()`](https://rdrr.io/pkg/S4Vectors/man/Annotated-class.html)
of the object.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
group_data(.data)
```

## Arguments

- .data:

  An object Inheriting from `PlySummarizedExperiment`, the wrapper class
  for `SummarizedExperiment` objects

## Value

list of groupings for an SummarizedExperiment

## Examples

``` r
group_by(se_simple, rows(direction), cols(condition)) |> group_data()
#> $row_groups
#> # A tibble: 2 × 3
#>   direction .indices  .indices_group_id
#>   <chr>     <list>                <int>
#> 1 -         <int [2]>                 1
#> 2 +         <int [3]>                 2
#> 
#> $col_groups
#> # A tibble: 2 × 3
#>   condition .indices  .indices_group_id
#>   <chr>     <list>                <int>
#> 1 cntrl     <int [2]>                 1
#> 2 drug      <int [2]>                 2
#> 
#> attr(,"class")
#> [1] "plyxp_groups"
#> attr(,"type")
#> [1] "rowcol"
```
