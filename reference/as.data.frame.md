# create data.frame

create data.frame

## Usage

``` r
# S3 method for class 'PlySummarizedExperiment'
as.data.frame(x, ...)
```

## Arguments

- x:

  `SummarizedExperiment` object

- ...:

  unused arguments

## Value

a data.frame object

## Examples

``` r
as.data.frame(se_simple)
#>    .features .samples counts logcounts gene length direction sample condition
#> 1      row_1    col_1     14 2.6390573   g1      1         -     s1     cntrl
#> 2      row_2    col_1     19 2.9444390   g2     24         +     s1     cntrl
#> 3      row_3    col_1     16 2.7725887   g3     60         +     s1     cntrl
#> 4      row_4    col_1     11 2.3978953   g4     39         -     s1     cntrl
#> 5      row_5    col_1     18 2.8903718   g5     37         +     s1     cntrl
#> 6      row_1    col_2      8 2.0794415   g1      1         -     s2     cntrl
#> 7      row_2    col_2      2 0.6931472   g2     24         +     s2     cntrl
#> 8      row_3    col_2      6 1.7917595   g3     60         +     s2     cntrl
#> 9      row_4    col_2     17 2.8332133   g4     39         -     s2     cntrl
#> 10     row_5    col_2     13 2.5649494   g5     37         +     s2     cntrl
#> 11     row_1    col_3      7 1.9459101   g1      1         -     s3      drug
#> 12     row_2    col_3      1 0.0000000   g2     24         +     s3      drug
#> 13     row_3    col_3     15 2.7080502   g3     60         +     s3      drug
#> 14     row_4    col_3     10 2.3025851   g4     39         -     s3      drug
#> 15     row_5    col_3     12 2.4849066   g5     37         +     s3      drug
#> 16     row_1    col_4      9 2.1972246   g1      1         -     s4      drug
#> 17     row_2    col_4      4 1.3862944   g2     24         +     s4      drug
#> 18     row_3    col_4     20 2.9957323   g3     60         +     s4      drug
#> 19     row_4    col_4      3 1.0986123   g4     39         -     s4      drug
#> 20     row_5    col_4      5 1.6094379   g5     37         +     s4      drug
```
