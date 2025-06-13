

test_that("summarize works - no groups", {
  
  res <- expect_no_error(
    summarize(
      se_simple,
      sum = sum(counts),
      rows(sum = sum(length)),
      cols(sum = sum(1:n()))
    )
  )
  
  #outputs should be 1 x 1
  expect_identical(dim(res), c(1L, 1L))
})

test_that("summarize works - groups: rows", {
  
  gse <- group_by(se_simple, rows(direction))
  
  res <- expect_no_error(
    summarize(
      gse,
      sum = sum(counts),
      rows(sum = sum(.assays_asis$counts)),
      cols(sum = colSums(.assays_asis$counts))
    )
  )
  
  expect_identical(dim(res), c(2L, 4L))
})

test_that("summarize works - groups: cols", {
  
  gse <- group_by(se_simple, cols(condition))
  
  res <- expect_no_error(
    summarize(
      gse,
      sum = sum(counts),
      rows(sum = rowSums(.assays_asis$counts)),
      cols(sum = sum(.assays_asis$counts))
    )
  )
  
  expect_identical(dim(res), c(5L, 2L))
})

test_that("summarize works - groups: rows,cols", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  
  res <- expect_no_error(
    summarize(
      gse,
      sum = sum(counts),
      rows(sum = sum(.assays_asis$counts)),
      cols(sum = sum(.assays_asis$counts))
    )
  )
  
  expect_identical(dim(res), c(2L, 2L))
  
})
