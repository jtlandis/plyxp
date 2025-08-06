# Default printing is `SummarizedExperiment`

    Code
      se(se_simple)
    Output
      class: SummarizedExperiment 
      dim: 5 4 
      metadata(0):
      assays(2): counts logcounts
      rownames(5): row_1 row_2 row_3 row_4 row_5
      rowData names(3): gene length direction
      colnames(4): col_1 col_2 col_3 col_4
      colData names(2): sample condition

# enabling `plyxp` printing

    Code
      use_show_tidy()
      se(se_simple)
    Output
      # A SummarizedExperiment-tibble Abstraction: 5 x 4
          .features .samples | counts logcounts | gene  length direction | sample
          <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
        1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
        2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
        3 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
        4 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
        5 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
        ~   ~         ~             ~        ~     ~         ~ ~            ~    
      n-4 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
      n-3 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
      n-2 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
      n-1 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
      n   row_5     col_4    |      5      1.61 | g5        37 +         | s4    
      # i n = 20
      # i 1 more variable: condition <chr>

# re-enabling default printing

    Code
      use_show_default()
      se(se_simple)
    Output
      class: SummarizedExperiment 
      dim: 5 4 
      metadata(0):
      assays(2): counts logcounts
      rownames(5): row_1 row_2 row_3 row_4 row_5
      rowData names(3): gene length direction
      colnames(4): col_1 col_2 col_3 col_4
      colData names(2): sample condition

# force `plyxp` printing

    Code
      show_tidy(se(se_simple))
    Output
      # A SummarizedExperiment-tibble Abstraction: 5 x 4
          .features .samples | counts logcounts | gene  length direction | sample
          <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
        1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
        2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
        3 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
        4 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
        5 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
        ~   ~         ~             ~        ~     ~         ~ ~            ~    
      n-4 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
      n-3 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
      n-2 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
      n-1 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
      n   row_5     col_4    |      5      1.61 | g5        37 +         | s4    
      # i n = 20
      # i 1 more variable: condition <chr>

# printing `PlySummarizedExperiment` as is

    Code
      se_simple
    Output
      # A SummarizedExperiment-tibble Abstraction: 5 x 4
          .features .samples | counts logcounts | gene  length direction | sample
          <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
        1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
        2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
        3 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
        4 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
        5 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
        ~   ~         ~             ~        ~     ~         ~ ~            ~    
      n-4 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
      n-3 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
      n-2 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
      n-1 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
      n   row_5     col_4    |      5      1.61 | g5        37 +         | s4    
      # i n = 20
      # i 1 more variable: condition <chr>

# printing `PlySummarizedExperiment` with `show_tidy()`

    Code
      show_tidy(se_simple)
    Output
      # A SummarizedExperiment-tibble Abstraction: 5 x 4
          .features .samples | counts logcounts | gene  length direction | sample
          <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
        1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
        2 row_2     col_1    |     19      2.94 | g2        24 +         | s1    
        3 row_3     col_1    |     16      2.77 | g3        60 +         | s1    
        4 row_4     col_1    |     11      2.40 | g4        39 -         | s1    
        5 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
        ~   ~         ~             ~        ~     ~         ~ ~            ~    
      n-4 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
      n-3 row_2     col_4    |      4      1.39 | g2        24 +         | s4    
      n-2 row_3     col_4    |     20      3.00 | g3        60 +         | s4    
      n-1 row_4     col_4    |      3      1.10 | g4        39 -         | s4    
      n   row_5     col_4    |      5      1.61 | g5        37 +         | s4    
      # i n = 20
      # i 1 more variable: condition <chr>

