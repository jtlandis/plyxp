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

test_that("can slice PlySummarizedExperiment various ways", {
  expect_no_error(
    se_simple[, 1:2]
  )
  expect_no_error(
    se_simple["row_1", "col_2"]
  )
  expect_no_error(
    se_simple[c(TRUE, FALSE), logical()]
  )

  expect_error(
    se_simple[, c(TRUE, FALSE, TRUE, FALSE, TRUE)]
  )
})

test_that("into_index returns integers", {
  size <- 4L
  names <- sprintf("row%i", 1:4)

  expect_identical(
    into_index(1:3, size, names),
    1L:3L
  )
  expect_identical(
    into_index(names, size, names),
    1L:4L
  )

  expect_identical(
    into_index(c(TRUE, FALSE), size, names),
    c(1L, 3L)
  )

  expect_identical(
    into_index(c("row4", "row2"), size, names),
    c(4L, 2L)
  )

  expect_identical(
    into_index(logical(), size, names),
    integer()
  )

  expect_error(
    into_index(c(TRUE, FALSE, TRUE, FALSE, TRUE), size, names),
    "logical cannot be longer than 4"
  )

  expect_error(
    into_index(identity, size, names),
    "cannot convert type 'closure' into integer index"
  )
})


test_that("add_bind works for lazy binding", {
  env <- new_environment(
    list(
      .iris = iris,
      .nrow = 10
    ),
    baseenv()
  )
  binding_func <- add_bind(
    .expr = quote({
      print(sprintf("evaluating for %s", !!name))
      lapply(seq_len(.nrow), function(i, x) x[i], x = .iris[[!!name]])
    }),
    .env_expr = env,
    type = "lazy"
  )
  expect_no_error(
    binding_func("Sepal.Width")
  )
  expect_snapshot(
    {
      print("first")
      env$Sepal.Width
      print("second")
      env$Sepal.Width
    }
  )
})


test_that("add_bind works for standard binding", {
  env <- new_environment(
    list(
      .iris = iris,
      .nrow = 10
    ),
    baseenv()
  )
  binding_func <- add_bind(
    .expr = quote({
      print(sprintf("evaluating for %s", !!name))
      lapply(seq_len(.nrow), function(i, x) x[i], x = .iris[[!!name]])
    }),
    .env_expr = env,
    type = "standard"
  )
  expect_snapshot(
    binding_func("Sepal.Width")
  )
  expect_snapshot(
    {
      print("first")
      env$Sepal.Width
      print("second")
      env$Sepal.Width
    }
  )
})


test_that("add_bind works for active binding", {
  env <- new_environment(
    list(
      .iris = iris,
      .nrow = 10
    ),
    baseenv()
  )
  binding_func <- add_bind(
    .expr = quote({
      print(sprintf("evaluating for %s", !!name))
      lapply(seq_len(.nrow), function(i, x) x[i], x = .iris[[!!name]])
    }),
    .env_expr = env,
    type = "active"
  )
  expect_no_error(
    binding_func("Sepal.Width")
  )
  expect_snapshot(
    {
      print("first")
      env$Sepal.Width
      print("second")
      env$Sepal.Width
    }
  )
})
