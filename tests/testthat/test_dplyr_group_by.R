test_that("no groups exist", {
  expect_null(group_data(se_simple))
})

test_that("groups can be constructed", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  g <- group_data(gse)
  expect_s3_class(g, "plyxp_groups")
  expect_true("direction" %in% names(g$row_groups))
  expect_true("condition" %in% names(g$col_groups))
})

test_that("groups can be removed", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  expect_s3_class(group_data(gse), "plyxp_groups")
  gse2 <- ungroup(gse)
  expect_null(group_data(gse2))

  gse3 <- ungroup(gse, rows(direction))
  expect_s3_class(group_data(gse3), "plyxp_groups")
  expect_true("condition" %in% names(group_data(gse3)$col_groups))
  expect_null(group_data(gse3)$row_groups)

  gse4 <- ungroup(gse, cols(condition))
  expect_s3_class(group_data(gse4), "plyxp_groups")
  expect_true("direction" %in% names(group_data(gse4)$row_groups))
  expect_null(group_data(gse4)$col_groups)

  # again with multiple groups
  gse <- group_by(se_simple, rows(direction, gene), cols(condition, sample))
  expect_s3_class(group_data(gse), "plyxp_groups")
  gse2 <- ungroup(gse)
  expect_null(group_data(gse2))

  gse3 <- ungroup(gse, rows(direction))
  expect_s3_class(group_data(gse3), "plyxp_groups")
  expect_true("condition" %in% names(group_data(gse3)$col_groups))
  expect_true("gene" %in% names(group_data(gse3)$row_groups))
  expect_false("direction" %in% names(group_data(gse3)$row_groups))

  gse4 <- ungroup(gse, cols(condition))
  expect_s3_class(group_data(gse4), "plyxp_groups")
  expect_true("direction" %in% names(group_data(gse4)$row_groups))
  expect_true("sample" %in% names(group_data(gse4)$col_groups))
  expect_false("condition" %in% names(group_data(gse4)$col_groups))
})
