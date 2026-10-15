test_that("`plyxp_quos()` aborts", {
  expect_error(plyxp_quos(), "`.ctx` must be specified!")
  expect_error(
    plyxp_quos(.ctx = "rows", .trans = 1),
    "`.trans` must be a list!"
  )
})

test_that("`plyxp_quos()` removes empty quos", {
  expect_identical(
    length(quos <- plyxp_quos(, cols(), .ctx = c("rows", "cols"))),
    0L
  )
  expect_identical(
    length(plyxp_quos(foo, cols(bar, baz), .ctx = c("rows", "cols"))),
    3L
  )
  expect_true(
    !is.null(
      names(
        enforce_named(exprs(foo, bar))
      )
    )
  )
  expect_true(
    !is.null(
      names(
        enforce_named(list(quote(foo), quote(bar)))
      )
    )
  )
})

test_that(".trans only accepts functions/calls", {
  expect_true(
    plyxp_quos(foo, .ctx = "rows", .trans = list(rows = identity)) |>
      _[[1]] |>
      attr("plyxp:::trans") |>
      is.function()
  )
  expect_true(
    plyxp_quos(
      foo,
      .ctx = "rows",
      .trans = list(rows = quote(\(.x) base::identity(.x)))
    ) |>
      _[[1]] |>
      attr("plyxp:::trans") |>
      is.call()
  )
  improper <- plyxp_quos(
    foo,
    .ctx = "rows",
    .trans = list(rows = quote(base::identity(.x)))
  ) |>
    _[[1]] |>
    attr("plyxp:::trans")
  expect_false(
    improper |> is.call()
  )
  expect_true(
    improper |> is.null()
  )
  expect_true(
    expect_true(
      plyxp_quos(foo, .ctx = "rows", .trans = list(rows = 1L)) |>
        _[[1]] |>
        attr("plyxp:::trans") |>
        is.null()
    )
  )
})


test_that("plyxp quo helpers work", {
  pm <- new_plyxp_manager(se_simple)
  q <- plyxp_quos(
    counts_1 = counts + 1,
    .ctx = "assays"
  )
  expect_no_error(
    plyxp_evaluate(pm, q)
  )
  expect_identical(
    plyxp_quo_ctx(q[[1L]]),
    "assays"
  )
  plyxp_quo_ctx(q[[1L]]) <- "rows"
  expect_identical(
    plyxp_quo_ctx(q[[1L]]),
    "rows"
  )
  expect_error(
    plyxp_evaluate(pm, q),
    "object 'counts' not found"
  )
  q <- plyxp_quos(
    val = 1:5,
    .ctx = "assays"
  )
  expect_identical(
    plyxp_evaluate(pm, q)$result("val")$assays,
    1:5
  )
  expect_null(
    plyxp_quo_trans(q[[1L]])
  )

  plyxp_quo_trans(q[[1L]]) <- quote(\(x) {
    enforce_2d_size(rep(x, 4), .nrow = 5L, .ncol = 4L)
  })
  expect_identical(
    plyxp_evaluate(pm, q)$result("val")$assays,
    matrix(rep(1:5, 4), 5, 4)
  )
  expect_identical(
    length(plyxp_quo_data(q[[1L]])),
    2L
  )
  plyxp_quo_data(q[[1L]]) <- list(foo = "bar")
  expect_identical(
    plyxp_quo_data(q[[1L]]) |> length(),
    3L
  )
  expect_identical(
    plyxp_quo_data(q[[1L]])$foo,
    "bar"
  )
  plyxp_quo_data(q[[1L]]) <- NULL
  expect_null(
    plyxp_quo_data(q[[1L]])
  )
})
