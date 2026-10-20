test_that("dplyr slice aborts with invalid input", {
  expect_error(
    slice(se_simple, 1L),
    "Cannot slice in `assays` context"
  )
  expect_error(
    slice(se_simple, rows(foo = 1L)),
    "Expressions should not be named in"
  )
})

test_that("slice works in rows", {
  expect_no_error(
    slice(se_simple, rows(2:3))
  )
})

test_that("slice works in cols", {
  expect_no_error(
    slice(se_simple, cols(c(3, 1)))
  )
})


test_that("slice works in rows & cols", {
  expect_no_error(
    slice(se_simple, rows(c(3, 1)), cols(c(3, 1)))
  )
})

test_that("slice refuses non-integer or non-null", {
  expect_error(
    slice(se_simple, cols("1")),
    "`slice` should resolve numeric slices"
  )
})
