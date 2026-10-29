# Slice a PlySummarizedExperiment

`slice()` selects rows and/or columns by position.

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
slice(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a PlySummarizedExperiment object

- ...:

  expressions that resolve to integer values to slice, .data by. Note
  that only the
  [`rows()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
  and
  [`cols()`](https://jtlandis.github.io/plyxp/reference/plyxp-context.md)
  contexts are available here.

- .preserve:

  Logical value. When FALSE, the default, group combinations are
  recomputed based on the resulting data. When TRUE, group combinations
  are retained, despite any groups being empty.

## Examples

``` r
gse <- group_by(se_simple, rows(direction), cols(condition))
sgse <- slice(gse, rows(which(direction == "-")))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'slice': Rle of type 'S4' is not supported
group_data(gse)
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
group_data(sgse)
#> Error: object 'sgse' not found
sgse2 <- slice(gse, rows(which(direction == "-")), .preserve = TRUE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'slice': Rle of type 'S4' is not supported
group_data(sgse2)
#> Error: object 'sgse2' not found
```
