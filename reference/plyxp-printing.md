# Printing within tibble with S4 objects

`plyxp` uses
[pillar](https://pillar.r-lib.org/reference/pillar-package.html) for its
printing. If you want to change how your S4 object is printed within
`plyxp`'s print method, consider writing a method for this function.

To print S4 objects in a tibble, `plyxp` hacks a custom integer vector
built from [`vctrs`](https://vctrs.r-lib.org/reference/new_vctr.html)
where the S4 object lives in an attribute named "phantomData". You can
create your own S4 phantom vector with `vec_phantom()`. This function is
not used outside of printing for `plyxp`

The default method for formatting a `vec_phantom()` is to call
[`showAsCell()`](https://rdrr.io/pkg/S4Vectors/man/show-utils.html).

## Usage

``` r
vec_phantom(x)

plyxp_pillar_format(x, ...)

show_tidy(x, ...)

use_show_tidy()

use_show_default()
```

## Arguments

- x:

  The S4 object

- ...:

  other arguments passed from
  [`pillar_shaft`](https://pillar.r-lib.org/reference/pillar_shaft.html)

## Value

`plyxp_pillar_format` -\> formatted version of your S4 vector
`vec_phantom` -\> integer vector with arbitrary object in `phatomData`
attribute.

## tidy printing

By default, `plyxp` will not affect the show method for
`SummarizedExperiment` objects. The `PlySummarizedExperiment` object
will always use the tibble abstraction method. If you want to use tibble
abstraction, you may use `use_show_tidy()` to enable or
`use_show_default()` \#' to disable this feature. These functions are
called for their side effects, \#' modifying the global option
"show_SummarizedExperiment_as_tibble_abstraction".

To show an object as the tibble abstraction regardless of the set
option, use the S3 generic `show_tidy(...)`.

## Examples

``` r

if (require("IRanges")) {
  ilist <- IRanges::IntegerList(list(c(1L, 2L, 3L), c(5L, 6L)))
  phantom <- vec_phantom(ilist)
  pillar::pillar_shaft(phantom)

  plyxp_pillar_format.CompressedIntegerList <- function(x) {
    sprintf("Int: [%i]", lengths(x))
  }
  print(pillar::pillar_shaft(phantom))
  rm(plyxp_pillar_format.CompressedIntegerList)
}
#> Loading required package: IRanges
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:plyxp’:
#> 
#>     colnames, rownames
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> Loading required package: S4Vectors
#> Loading required package: stats4
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
#> 
#> Attaching package: ‘IRanges’
#> The following object is masked from ‘package:plyxp’:
#> 
#>     slice
#> <pillar_ornament>
#> 1,2,3
#> 5,6  

# default printing for PlySummarizedExperiment object
se_simple
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
# default printing for SummarizedExperiment object
se <- se(se_simple)
se
#> class: SummarizedExperiment 
#> dim: 5 4 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames(5): row_1 row_2 row_3 row_4 row_5
#> rowData names(3): gene length direction
#> colnames(4): col_1 col_2 col_3 col_4
#> colData names(2): sample condition
# use `plyxp` tibble abstraction
use_show_tidy()
se
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
# restore default print
use_show_default()
se
#> class: SummarizedExperiment 
#> dim: 5 4 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames(5): row_1 row_2 row_3 row_4 row_5
#> rowData names(3): gene length direction
#> colnames(4): col_1 col_2 col_3 col_4
#> colData names(2): sample condition
# explicitly using tibble abstraction
show_tidy(se)
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
```
