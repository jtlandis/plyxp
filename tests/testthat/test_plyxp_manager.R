bm <- new_plyxp_manager(se(se_simple))
gbm <- group_by(se_simple, rows(direction), cols(condition)) |>
  se() |>
  plyxp:::new_plyxp_manager()

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


test_that("manager", {
  manager <- new_plyxp_manager(se(se_simple))
  q <- plyxp_quos(
    counts_1 = counts + 1,
    cols(is_drug = condition == "drug"),
    rows(will_error = stop("bad expr")),
    .ctx = c("assays", "rows", "cols")
  )
  expect_error(
    plyxp_evaluate(manager, q),
    "bad expr"
  )
  gm <- new_plyxp_manager(
    group_by(se_simple, rows(direction), cols(condition))
  )
  expect_error(
    plyxp_evaluate(gm, q),
    "bad expr"
  )
})


test_that("assays split in groups", {
  m <- group_by(se_simple, rows(direction), cols(condition)) |>
    new_plyxp_manager()
  q <- plyxp_quos(counts, .ctx = "assays")
  expect_identical(
    m$ctx_mask$eval(q[[1L]]),
    matrix(
      c(14L, 11L, 8L, 17L),
      2,
      2,
      dimnames = list(c("row_1", "row_4"), c("col_1", "col_2"))
    )
  )
})


test_that("masks bindings are read-only", {
  m <- group_by(se_simple, rows(direction), cols(condition)) |>
    new_plyxp_manager()

  expect_error(
    m$ctx_mask <- list(),
    "`\\$ctx_mask` is read only"
  )

  expect_error(
    m$n_groups <- list(),
    "`\\$n_groups` is read only"
  )

  expect_error(
    m$masks <- list(),
    "`\\$masks` is read only"
  )

  expect_error(
    m$extended <- list(),
    "`\\$extended` is read only"
  )
})

test_that("get proper sequence across contexts groups", {
  m <- group_by(se_simple, rows(direction), cols(condition)) |>
    new_plyxp_manager()

  expect_identical(
    m$along_ctx(),
    seq_len(4L)
  )

  m$ctx <- "rows"

  expect_identical(
    m$along_ctx(),
    seq_len(2L)
  )

  m$ctx <- "cols"

  expect_identical(
    m$along_ctx(),
    seq_len(2L)
  )
})
