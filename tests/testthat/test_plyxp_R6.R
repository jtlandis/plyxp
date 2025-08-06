# gbm <- group_by(se_simple, rows(direction), cols(condition)) |>
#   new_plyxp_manager()
#
# assays_mask <- bm$masks[["assays"]]
# rows_mask <- bm$masks[["cols"]]
# cols_mask <- bm$masks[["rows"]]

test_that("evaluation works", {
  bm <- plyxp:::new_plyxp_manager(se(se_simple))
  expect_error(bm$eval(rlang::quo("foo")), "requires quosure from")
  quo <- plyxp:::plyxp_quos(
    foo = rep(1L, n()),
    rows(foo = rep(1L, n())),
    cols(foo = rep(1L, n())),
    .ctx_default = "assays",
    .ctx_opt = c("rows", "cols")
  )
  bm$eval(quo[[1]])
  bm$ctx <- "rows"
  bm$eval(quo[[2]])
  bm$ctx <- "cols"
  bm$eval(quo[[3]])
  res <- bm$results()
  expect_identical(sum(res$assays$foo), 20L)
  expect_identical(sum(res$rows$foo), 5L)
  expect_identical(sum(res$cols$foo), 4L)
})
