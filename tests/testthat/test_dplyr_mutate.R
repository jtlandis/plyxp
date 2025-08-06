test_that("mutate works - no groups", {
  res <- expect_no_error(
    mutate(se_simple,
      foo = 1:n(),
      rows(foo = 1:n()),
      cols(foo = 1:n())
    )
  )@se

  expect_identical(
    assay(res, "foo") |> unname(),
    matrix(1:20, nrow = 5, ncol = 4)
  )
  expect_identical(rowData(res)[["foo"]], 1:5)
  expect_identical(colData(res)[["foo"]], 1:4)
})

test_that("mutate works - with groups", {
  gse <- group_by(se_simple, rows(direction), cols(condition))

  res <- expect_no_error(
    mutate(gse,
      foo = cur_group_id(),
      rows(foo = cur_group_id()),
      cols(foo = cur_group_id())
    )@se
  )
  expect_identical(
    assay(res, "foo") |> unname(),
    matrix(c(
      rep(c(1L, 2L, 2L, 1L, 2L), 2),
      rep(c(3L, 4L, 4L, 3L, 4L), 2)
    ), nrow = 5, ncol = 4)
  )
  expect_identical(rowData(res)[["foo"]], c(1L, 2L, 2L, 1L, 2L))
  expect_identical(colData(res)[["foo"]], c(1L, 1L, 2L, 2L))
})
