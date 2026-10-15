test_that("as.data.frame unwinds data", {
  expect_no_error(
    df <- as.data.frame(se_simple)
  )
  expect_identical(
    dplyr::distinct(df, .features, gene, length, direction) |>
      nrow(),
    nrow(se_simple)
  )
  expect_identical(
    dplyr::distinct(df, .samples, sample, condition) |>
      nrow(),
    ncol(se_simple)
  )
})

test_that("sentinal functions error", {
  expect_error(
    rows(),
    "`rows\\(\\)` is a sentinal function"
  )
  expect_error(
    cols(),
    "`cols\\(\\)` is a sentinal function"
  )
})
