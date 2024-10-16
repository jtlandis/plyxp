

bm <- new_plyxp_manager(se_simple)
gbm <- group_by(se_simple, rows(direction), cols(condition)) |>
  new_plyxp_manager()

test_that("`new_plyxp_manager` returns an R6 `plyxp_manager`", {
  expect_s3_class(bm, "R6")
  expect_s3_class(bm, "plyxp_manager")
  expect_s3_class(gbm, "R6")
  expect_s3_class(gbm, "plyxp_manager")
})

test_that("default context is `assays` for SummarizedExperiment", {
  expect_identical(bm$ctx, "assays")
  expect_identical(gbm$ctx, "assays")
})


test_that("context may be changed between `assays`, `rows` and `cols`", {
  bm$ctx <- "rows"
  expect_identical(bm$ctx, "rows")
  bm$ctx <- "cols"
  expect_identical(bm$ctx, "cols")
  bm$ctx <- "assays"
  # shouldnt be able to change it to something else
  expect_error(bm$ctx <- "foo", "should be one of")
  # bm$ctx should still be "assays" after the above failure
  expect_identical(bm$ctx, "assays")
})

test_that("correct number of groups for ungrouped and grouped `se_simple`", {
  expect_identical(bm$n_groups, 1L)
  expect_identical(gbm$n_groups, 4L) # rows(+, -), cols(condition, treatment)
  gbm$ctx <- "rows"
  expect_identical(gbm$n_groups, 2L)
  gbm$ctx <- "cols"
  expect_identical(gbm$n_groups, 2L)
  gbm$ctx <- "assays"
})

