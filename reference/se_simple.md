# Plyxp Simple Example Summarized Experiment

A small data SummarizedExperiment Object of 20 observations, 5 rows and
4 columns.

## Usage

``` r
se_simple
```

## Format

### `se_simple`

- `assays`:

  counts

  :   sampled data points between 1:20

  logcounts

  :   log transform of `counts`

- `rowData`/`.features`:

  gene

  :   fake gene name

  length

  :   fake gene length

  direction

  :   fake strand

- `colData`/`.samples`:

  sample

  :   fake sample name

  condition

  :   control or drug treatment

## Value

a `SummarizedExperiment` object

## Examples

``` r
SummarizedExperiment::assays(se_simple)
#> List of length 2
#> names(2): counts logcounts
SummarizedExperiment::rowData(se_simple)
#> DataFrame with 5 rows and 3 columns
#>              gene    length   direction
#>       <character> <integer> <character>
#> row_1          g1         1           -
#> row_2          g2        24           +
#> row_3          g3        60           +
#> row_4          g4        39           -
#> row_5          g5        37           +
SummarizedExperiment::colData(se_simple)
#> DataFrame with 4 rows and 2 columns
#>            sample   condition
#>       <character> <character>
#> col_1          s1       cntrl
#> col_2          s2       cntrl
#> col_3          s3        drug
#> col_4          s4        drug
```
