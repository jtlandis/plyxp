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
