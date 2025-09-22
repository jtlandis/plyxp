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

test_that("groups change on data augment", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  gdata <- group_data(gse)
  expect_equal(nrow(gdata$row_groups), 2)
  gdata_ <- group_data(gse[1, ])
  expect_equal(nrow(gdata_$row_groups), 1)
  expect_identical(gdata$col_groups, gdata_$col_groups)
})

test_that("groups change on filter augment", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  gdata <- group_data(gse)
  expect_equal(nrow(gdata$row_groups), 2)
  gdata_ <- group_data(filter(gse, rows(direction == "-")))
  expect_equal(nrow(gdata_$row_groups), 1)
  expect_identical(gdata$col_groups, gdata_$col_groups)
  gdata_ <- group_data(filter(gse, rows(direction == "-"), .preserve = TRUE))
  expect_equal(nrow(gdata_$row_groups), 2)
  expect_identical(gdata_$row_groups$.indices, list(1:2, integer()))
  expect_identical(gdata$col_groups, gdata_$col_groups)
})

test_that("groups change on arrange augment", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  gdata <- group_data(gse)
  expect_equal(nrow(gdata$row_groups), 2)
  gdata_ <- group_data(arrange(gse, rows(dplyr::desc(.features))))
  expect_equal(nrow(gdata_$row_groups), 2)
  expect_identical(gdata$col_groups, gdata_$col_groups)
  expect_identical(gdata_$row_groups$.indices, list(c(1L, 3L, 4L), c(2L, 5L)))
  gdata_ <- group_data(arrange(gse, rows(dplyr::desc(.features)),
    .by_group = TRUE
  ))
  expect_equal(nrow(gdata_$row_groups), 2)
  expect_identical(gdata_$row_groups$.indices, list(1:3, 4:5))
  expect_identical(gdata$col_groups, gdata_$col_groups)
})


test_that("groups change on slice augment", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  gdata <- group_data(gse)
  expect_equal(nrow(gdata$row_groups), 2)
  gdata_ <- group_data(slice(gse, rows(which(direction == "-"))))
  expect_equal(nrow(gdata_$row_groups), 1)
  expect_identical(gdata$col_groups, gdata_$col_groups)
  gdata_ <- group_data(slice(gse, rows(which(direction == "-")),
    .preserve = TRUE
  ))
  expect_equal(nrow(gdata_$row_groups), 2)
  expect_identical(gdata_$row_groups$.indices, list(1:2, integer()))
  expect_identical(gdata$col_groups, gdata_$col_groups)
})

test_that("scalars provide appropriate indices", {
  se <- se_simple |> group_by(rows(foo = 6L))

  expect_identical(rowData(se)$foo, rep(6L, 5L))
  expect_identical(group_data(se)$row_groups$.indices, list(1:5))
})
