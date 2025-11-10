# Modify SummarizedExperiment Object

Modify the underlying SummarizedExperiment object with a function.

## Usage

``` r
plyxp(.data, .f, ..., .caller = caller_env())

plyxp_on(.data, .f, ..., .on, .caller = caller_env())
```

## Arguments

- .data:

  a PlySummarizedExperiment object

- .f:

  within `plyxp()`: a function that returns a SummarizedExperiment
  object. within `plyxp_on()`: `.f` should return a value compatible
  with `.on(se)<-`

- ...:

  additional arguments passed to `.f`

- .caller:

  environment in which plyxp should signal an error if one occurs.

- .on:

  a symbol matching an accessor and setter function for the
  `SummarizedExperiment` Class.

## Value

a PlySummarizedExperiment object

## Functions

- `plyxp_on()`: pass a function to the result of an accessor of the
  `SummarizedExperiment` Class This function is a wrapper for the
  expression:


       plyxp::plyxp(.data, function(se, ...) {
         .f <- rlang::as_function(.f)
         obj <- .on(se)
         obj <- .f(se, ...)
         .on(se) <- obj
         se
       }, ...)

  where `.on` is the symbol for the accessor function into a
  `SummarizedExperiment` Class. Note: the setter variant must exist in
  the environment that `plyxp_on()` is called. All other arguments are
  diffused as quosures and will be evaluated in the environment they
  were quoted.

## Examples

``` r
plyxp(se_simple, function(x) x)
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | gene  length direction | sample
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
#>   1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
#>   2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
#>   3 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
#>   4 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
#>   5 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
#>   …   …         …             …        …     …         … …            …    
#> n-4 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
#> n-3 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
#> n-2 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
#> n-1 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
#> n   row_5     col_4    |      5      1.61 | g5        37 +         | s4    
#> # ℹ n = 20
#> # ℹ 1 more variable: condition <chr>
plyxp_on(se_simple,
  .f = lapply, # function to call on `.on` args,
  .on = rowData, # data `.f` will be used on
  paste, "foo"
) # arguments for `.f`
#> # A SummarizedExperiment-tibble Abstraction: 5 × 4
#>     .features .samples | counts logcounts | gene   length direction | sample
#>     <chr>     <chr>    |  <int>     <dbl> | <chr>  <chr>  <chr>     | <chr> 
#>   1 row_1     col_1    |     14      2.64 | g1 foo 1 foo  - foo     | s1    
#>   2 row_2     col_1    |     19      2.94 | g2 foo 24 foo + foo     | s1    
#>   3 row_3     col_1    |     16      2.77 | g3 foo 60 foo + foo     | s1    
#>   4 row_4     col_1    |     11      2.40 | g4 foo 39 foo - foo     | s1    
#>   5 row_5     col_1    |     18      2.89 | g5 foo 37 foo + foo     | s1    
#>   …   …         …             …        …     …  …   …  …  …  …         …    
#> n-4 row_1     col_4    |      9      2.20 | g1 foo 1 foo  - foo     | s4    
#> n-3 row_2     col_4    |      4      1.39 | g2 foo 24 foo + foo     | s4    
#> n-2 row_3     col_4    |     20      3.00 | g3 foo 60 foo + foo     | s4    
#> n-1 row_4     col_4    |      3      1.10 | g4 foo 39 foo - foo     | s4    
#> n   row_5     col_4    |      5      1.61 | g5 foo 37 foo + foo     | s4    
#> # ℹ n = 20
#> # ℹ 1 more variable: condition <chr>
```
