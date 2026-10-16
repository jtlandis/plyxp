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


test_that("ctx_env works", {
  expect_null(
    peek_ctx("foo")
  )
  old <- expect_null(
    poke_ctx("foo", "bar")
  )
  expect_identical(
    poke_ctx("foo", NULL),
    "bar"
  )
  expect_null(
    peek_ctx("foo")
  )
  expect_no_error(
    rlang::env_unbind(ctx_env, "foo")
  )
})
