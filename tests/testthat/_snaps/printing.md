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

# printing grouping information shows

    Code
      show_tidy(group_by(se_simple, rows(direction), cols(condition)))
    Output
      # A SummarizedExperiment-tibble Abstraction: 5 x 4
      # Groups: rows(direction), cols(condition)
          .features .samples | counts logcounts | direction gene  length | condition
          <chr>     <chr>    |  <int>     <dbl> | <chr>     <chr>  <int> | <chr>    
        1 row_1     col_1    |     14      2.64 | -         g1         1 | cntrl    
        2 row_2     col_1    |     19      2.94 | +         g2        24 | cntrl    
        3 row_3     col_1    |     16      2.77 | +         g3        60 | cntrl    
        4 row_4     col_1    |     11      2.40 | -         g4        39 | cntrl    
        5 row_5     col_1    |     18      2.89 | +         g5        37 | cntrl    
        ~   ~         ~             ~        ~    ~          ~         ~     ~      
      n-4 row_1     col_4    |      9      2.20 | -         g1         1 | drug     
      n-3 row_2     col_4    |      4      1.39 | +         g2        24 | drug     
      n-2 row_3     col_4    |     20      3.00 | +         g3        60 | drug     
      n-1 row_4     col_4    |      3      1.10 | -         g4        39 | drug     
      n   row_5     col_4    |      5      1.61 | +         g5        37 | drug     
      # i n = 20
      # i 1 more variable: sample <chr>

# printing small table

    Code
      show_tidy(se_simple[c("row_1", "row_5"), c(TRUE, FALSE, FALSE, TRUE)])
    Output
      # A SummarizedExperiment-tibble Abstraction: 2 x 2
        .features .samples | counts logcounts | gene  length direction | sample
        <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
      1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
      2 row_5     col_1    |     18      2.89 | g5        37 +         | s1    
      3 row_1     col_4    |      9      2.20 | g1         1 -         | s4    
      4 row_5     col_4    |      5      1.61 | g5        37 +         | s4    
      # i 1 more variable: condition <chr>

# printing with S4 vectors work

    Code
      mutate(se_simple, rows(df = S4Vectors::DataFrame(gene = gene, length = length,
        direction = S4Vectors::Rle(direction)), gene = NULL, length = NULL,
      direction = NULL), )
    Output
      # A SummarizedExperiment-tibble Abstraction: 5 x 4
          .features .samples | counts logcounts | df           | sample condition
          <chr>     <chr>    |  <int>     <dbl> | <DFrame[,3]> | <chr>  <chr>    
        1 row_1     col_1    |     14      2.64 | g1:1:-       | s1     cntrl    
        2 row_2     col_1    |     19      2.94 | g2:24:+      | s1     cntrl    
        3 row_3     col_1    |     16      2.77 | g3:60:+      | s1     cntrl    
        4 row_4     col_1    |     11      2.40 | g4:39:-      | s1     cntrl    
        5 row_5     col_1    |     18      2.89 | g5:37:+      | s1     cntrl    
        ~   ~         ~             ~        ~       ~            ~       ~      
      n-4 row_1     col_4    |      9      2.20 | g1:1:-       | s4     drug     
      n-3 row_2     col_4    |      4      1.39 | g2:24:+      | s4     drug     
      n-2 row_3     col_4    |     20      3.00 | g3:60:+      | s4     drug     
      n-1 row_4     col_4    |      3      1.10 | g4:39:-      | s4     drug     
      n   row_5     col_4    |      5      1.61 | g5:37:+      | s4     drug     
      # i n = 20

# printing n values

    Code
      show_tidy(se_simple, n = 2)
    Output
      # A SummarizedExperiment-tibble Abstraction: 5 x 4
        .features .samples | counts logcounts | gene  length direction | sample
        <chr>     <chr>    |  <int>     <dbl> | <chr>  <int> <chr>     | <chr> 
      1 row_1     col_1    |     14      2.64 | g1         1 -         | s1    
      ~   ~         ~             ~        ~     ~         ~ ~            ~    
      n row_5     col_4    |      5      1.61 | g5        37 +         | s4    
      # i n = 20
      # i 1 more variable: condition <chr>

# show_tidy() will default print

    Code
      show_tidy(S4Vectors::Rle(rep(1:2, each = 5)))
    Output
      integer-Rle of length 10 with 2 runs
        Lengths: 5 5
        Values : 1 2

# show() works

    Code
      show(se_simple)
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

