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
#> DataFrame with 2 rows and 3 columns
#>     direction .indices .indices_group_id
#>   <character>   <list>         <integer>
#> 1           -      1,4                 1
#> 2           +    2,3,5                 2
#> 
#> $col_groups
#> DataFrame with 2 rows and 3 columns
#>     condition .indices .indices_group_id
#>   <character>   <list>         <integer>
#> 1       cntrl      1,2                 1
#> 2        drug      3,4                 2
#> 
#> attr(,"class")
#> [1] "plyxp_groups"
```
