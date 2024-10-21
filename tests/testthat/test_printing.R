test_that("Default printing is `SummarizedExperiment`", {
  expect_snapshot(se_simple@se)
})

test_that("enabling `plyxp` printing", {
  expect_snapshot({
    use_show_tidy()
    se_simple@se
  })
})

test_that("re-enabling default printing", {
  expect_snapshot({
    use_show_default()
    se_simple@se
  })
})

test_that("force `plyxp` printing", {
  expect_snapshot(show_tidy(se_simple@se))
})

test_that("printing `PlySummarizedExperiment` as is", {
  expect_snapshot(se_simple)
})

test_that("printing `PlySummarizedExperiment` with `show_tidy()`", {
  expect_snapshot({
    show_tidy(se_simple)
  })
})
