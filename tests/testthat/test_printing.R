test_that("Default printing is `SummarizedExperiment`", {
  expect_snapshot(se(se_simple))
})

test_that("enabling `plyxp` printing", {
  expect_snapshot({
    use_show_tidy()
    se(se_simple)
  })
})

test_that("re-enabling default printing", {
  expect_snapshot({
    use_show_default()
    se(se_simple)
  })
})

test_that("force `plyxp` printing", {
  expect_snapshot(show_tidy(se(se_simple)))
})

test_that("printing `PlySummarizedExperiment` as is", {
  expect_snapshot(se_simple)
})

test_that("printing `PlySummarizedExperiment` with `show_tidy()`", {
  expect_snapshot({
    show_tidy(se_simple)
  })
})

test_that("printing grouping information shows", {
  expect_snapshot({
    group_by(se_simple, rows(direction), cols(condition)) |>
      show_tidy()
  })
})

test_that("printing small table", {
  expect_snapshot({
    show_tidy(se_simple[c("row_1", "row_5"), c(TRUE, FALSE, FALSE, TRUE)])
  })
})


test_that("printing with S4 vectors work", {
  expect_snapshot(
    mutate(
      se_simple,
      rows(
        df = S4Vectors::DataFrame(
          gene = gene,
          length = length,
          direction = S4Vectors::Rle(direction)
        ),
        gene = NULL,
        length = NULL,
        direction = NULL
      ),
    )
  )
})

test_that("printing n values", {
  expect_snapshot({
    show_tidy(se_simple, n = 2)
  })
})

test_that("show_tidy() will default print", {
  expect_snapshot({
    show_tidy(S4Vectors::Rle(rep(1:2, each = 5)))
  })
})


test_that("show() works", {
  expect_snapshot(
    show(se_simple)
  )
})
