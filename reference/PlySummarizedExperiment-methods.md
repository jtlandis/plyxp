# PlySummarizedExperiment Methods

Methods from SummarizedExperiment package re-implemented for
PlySummarizedExperiment.

## Usage

``` r
se(x)

# S4 method for class 'PlySummarizedExperiment'
se(x)

se(x) <- value

# S4 method for class 'PlySummarizedExperiment'
se(x) <- value

# S4 method for class 'SummarizedExperiment'
se(x)

# S4 method for class 'SummarizedExperiment'
se(x) <- value

# S4 method for class 'PlySummarizedExperiment'
assays(x, withDimnames = TRUE, ...)

# S4 method for class 'PlySummarizedExperiment,list'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'PlySummarizedExperiment,SimpleList'
assays(x, withDimnames = TRUE, ...) <- value

# S4 method for class 'PlySummarizedExperiment,missing'
assay(x, i, withDimnames = TRUE, ...)

# S4 method for class 'PlySummarizedExperiment,numeric'
assay(x, i, withDimnames = TRUE, ...)

# S4 method for class 'PlySummarizedExperiment,character'
assay(x, i, withDimnames = TRUE, ...)

# S4 method for class 'PlySummarizedExperiment,missing'
assay(x, i, withDimnames = TRUE, ...) <- value

# S4 method for class 'PlySummarizedExperiment,numeric'
assay(x, i, withDimnames = TRUE, ...) <- value

# S4 method for class 'PlySummarizedExperiment,character'
assay(x, i, withDimnames = TRUE, ...) <- value

# S4 method for class 'PlySummarizedExperiment'
rowData(x, use.names = TRUE, ...)

# S4 method for class 'PlySummarizedExperiment'
rowData(x, ...) <- value

# S4 method for class 'PlySummarizedExperiment'
colData(x, ...)

# S4 method for class 'PlySummarizedExperiment,DataFrame'
colData(x, ...) <- value

# S4 method for class 'PlySummarizedExperiment,NULL'
colData(x, ...) <- value

# S4 method for class 'PlySummarizedExperiment'
metadata(x, ...)

# S4 method for class 'PlySummarizedExperiment'
metadata(x, ...) <- value

# S4 method for class 'PlySummarizedExperiment'
rownames(x)

# S4 method for class 'PlySummarizedExperiment'
colnames(x)

# S4 method for class 'PlySummarizedExperiment'
nrow(x)

# S4 method for class 'PlySummarizedExperiment'
ncol(x)

# S4 method for class 'PlySummarizedExperiment'
dimnames(x)

# S4 method for class 'PlySummarizedExperiment,list'
dimnames(x) <- value

# S4 method for class 'PlySummarizedExperiment,NULL'
dimnames(x) <- value
```

## Arguments

- x:

  PlySummarizedExperiment object

- value:

  replacement value

- withDimnames:

  logical

- ...:

  additional arguments

- i:

  character or numeric index

- use.names:

  logical

## Value

Replacement functions return a PlySummarizedExperiment object. Other
functions will return the same object as the method from
SummarizedExperiment.

## Functions

- `se(PlySummarizedExperiment)`: get the se slot of the
  PlySummarizedExperiment object

- `se(x) <- value`: set the se slot of the PlySummarizedExperiment
  object

- `se(PlySummarizedExperiment) <- value`: set the se slot of the
  PlySummarizedExperiment object

- `se(SummarizedExperiment)`: get the SummarizedExperiment object

- `se(SummarizedExperiment) <- value`: get the SummarizedExperiment
  object

- `assays(PlySummarizedExperiment)`: get the assays o the
  PlySummarizedExperiment object

- `assays(x = PlySummarizedExperiment) <- value`: set the assays of the
  PlySummarizedExperiment object

- `assays(x = PlySummarizedExperiment) <- value`: set the assays of the
  PlySummarizedExperiment object

- `assay(x = PlySummarizedExperiment, i = missing)`: get the first assay
  of the PlySummarizedExperiment object

- `assay(x = PlySummarizedExperiment, i = numeric)`: get assay from a
  PlySummarizedExperiment object

- `assay(x = PlySummarizedExperiment, i = character)`: get assay from a
  PlySummarizedExperiment object

- `assay(x = PlySummarizedExperiment, i = missing) <- value`: set assay
  in a PlySummarizedExperiment object

- `assay(x = PlySummarizedExperiment, i = numeric) <- value`: set assay
  in a PlySummarizedExperiment object

- `assay(x = PlySummarizedExperiment, i = character) <- value`: set
  assay in a PlySummarizedExperiment object

- `rowData(PlySummarizedExperiment)`: get rowData in a
  PlySummarizedExperiment object

- `rowData(PlySummarizedExperiment) <- value`: set rowData in a
  PlySummarizedExperiment object

- `colData(PlySummarizedExperiment)`: get colData in a
  PlySummarizedExperiment object

- `colData(x = PlySummarizedExperiment) <- value`: set colData in a
  PlySummarizedExperiment object

## Examples

``` r
assays(se_simple)
#> List of length 2
#> names(2): counts logcounts
rowData(se_simple)
#> DataFrame with 5 rows and 3 columns
#>              gene    length   direction
#>       <character> <integer> <character>
#> row_1          g1         1           -
#> row_2          g2        24           +
#> row_3          g3        60           +
#> row_4          g4        39           -
#> row_5          g5        37           +
colData(se_simple)
#> DataFrame with 4 rows and 2 columns
#>            sample   condition
#>       <character> <character>
#> col_1          s1       cntrl
#> col_2          s2       cntrl
#> col_3          s3        drug
#> col_4          s4        drug
```
