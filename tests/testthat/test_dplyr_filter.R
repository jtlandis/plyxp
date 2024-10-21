test_that("filter works - no groups", {
  expect_error(
    filter(se_simple, counts >= 20),
    "Cannot filter in `assays` context"
  )

  res <- expect_no_error(
    filter(
      se_simple,
      rows(length > 30),
      cols(sample %in% c("s2", "s3"))
    )
  )

  expect_identical(dim(res), c(3L, 2L))
})


test_that("filter works - with groups", {
  gse <- group_by(se_simple, rows(direction), cols(condition))

  res <- expect_no_error(
    filter(
      gse,
      rows(length > 30),
      cols(sample %in% c("s2", "s3"))
    )
  )

  # still works regardless
  expect_identical(dim(res), c(3L, 2L))
})

test_that("endomorphism", {
  res <- se_simple |>
    filter(
      rows(length > 30),
      cols(sample %in% c("s2", "s3"))
    )

  endo <- local({
    row_sub <- rowData(se_simple@se)[["length"]] > 30
    col_sub <- colData(se_simple@se)[["sample"]] %in% c("s2", "s3")
    se_simple[row_sub, col_sub]
  })

  expect_identical(res, endo)

  # make a test for groups?
})
