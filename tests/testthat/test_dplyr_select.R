
test_that("select works", {
  res <- expect_no_error(
    select(
      se_simple,
      counts,
      rows(direction),
      cols(condition)
    )
  )
  
  expect_identical(assays(res) |> names(), "counts")
  expect_identical(rowData(res) |> names(), "direction")
  expect_identical(colData(res) |> names(), "condition")
})