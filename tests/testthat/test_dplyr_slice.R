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
