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
    .ctx = c("assays", "rows", "cols")
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

test_that("R6 environments are accessible", {
  bm <- plyxp:::new_plyxp_manager(se(se_simple))

  assay_envs <- expect_no_error(bm$ctx_mask$environments)
  chops <- expect_no_error(
    assay_envs@env_data_chop
  )
  expect_identical(
    chops$counts,
    list(assays(se(se_simple))$counts)
  )

  expect_no_error(
    assay_envs@env_data_chop$foo <- list("bar")
  )

  expect_identical(
    chops$foo,
    list("bar")
  )
})

test_that("AtNames for plyxp_envs", {
  bm <- plyxp:::new_plyxp_manager(se(se_simple))

  assay_envs <- bm$ctx_mask$environments

  expect_identical(
    .AtNames.plyxp_envs(assay_envs, "env"),
    c(
      "env_mask_bind",
      "env_data_chop",
      "env_data_lazy",
      "env_foreign_data",
      "env_current_group_info"
    )
  )

  expect_identical(
    .AtNames.plyxp_envs(assay_envs, ""),
    c(
      "env_mask_bind",
      "env_data_chop",
      "env_data_lazy",
      "env_foreign_data",
      "env_current_group_info"
    )
  )
})
