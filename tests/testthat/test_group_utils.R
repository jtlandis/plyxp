test_that("group_details works", {
  # Ungrouped
  details <- group_details(se_simple)
  expect_false(attr(details, "grouped_rows"))
  expect_false(attr(details, "grouped_cols"))
  expect_equal(nrow(details$row_groups), 1)
  expect_equal(details$row_groups$.indices, list(1:5))
  expect_equal(nrow(details$col_groups), 1)
  expect_equal(details$col_groups$.indices, list(1:4))
  expect_equal(attr(details, "obj_dim"), c(5, 4))

  # Grouped
  gse <- group_by(se_simple, rows(direction), cols(condition))
  details_g <- group_details(gse)
  expect_null(attr(details_g, "grouped_rows"))
  expect_null(attr(details_g, "grouped_cols"))
  expect_equal(nrow(details_g$row_groups), 2)
  expect_equal(nrow(details_g$col_groups), 2)
})

test_that("expand_groups2 works", {
  gse <- group_by(se_simple, rows(direction), cols(condition))
  details_g <- group_details(gse)

  res <- expand_groups2(details_g$row_groups, details_g$col_groups)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4) # 2 row groups * 2 col groups

  expected_names <- c(
    ".rows::direction",
    ".rows::.indices",
    ".rows::.indices_group_id",
    ".cols::condition",
    ".cols::.indices",
    ".cols::.indices_group_id",
    ".nrows",
    ".ncols",
    ".group_id"
  )
  expect_true(all(expected_names %in% names(res)))

  expect_equal(res$.nrows, c(2, 3, 2, 3))
  expect_equal(res$.ncols, c(2, 2, 2, 2))
  expect_equal(res$.group_id, 1:4)

  expect_equal(attr(res, "plyxp:::unique_row_ind"), c(1, 2))
  expect_equal(attr(res, "plyxp:::unique_col_ind"), c(1, 3))
})

test_that("get_group_indices works", {
  # Ungrouped returns NULL
  details <- group_details(se_simple)
  expanded <- expand_groups2(details$row_groups, details$col_groups)
  expect_null(get_group_indices(details, expanded, type = "assays"))
  expect_null(get_group_indices(details, expanded, type = "rowData"))
  expect_null(get_group_indices(details, expanded, type = "colData"))

  # Grouped
  gse <- group_by(se_simple, rows(direction), cols(condition))
  details_g <- group_details(gse)
  expanded_g <- expand_groups2(details_g$row_groups, details_g$col_groups)

  res_assays <- get_group_indices(details_g, expanded_g, type = "assays")
  expect_type(res_assays, "list")
  expect_equal(length(res_assays), 4)

  # Check attributes for chopped indices
  expect_equal(attr(res_assays, "plyxp:::unique_row_ind"), c(1, 2))
  expect_equal(attr(res_assays, "plyxp:::unique_col_ind"), c(1, 3))
  expect_equal(length(attr(res_assays, "plyxp:::row_chop_ind")), 4)
  expect_equal(length(attr(res_assays, "plyxp:::col_chop_ind")), 4)

  # Check one index specifically
  expect_equal(res_assays[[1]], c(1, 4, 6, 9))

  # Grouped by row only - check rowData and colData
  gse_row <- group_by(se_simple, rows(direction))
  details_row <- group_details(gse_row)
  expanded_row <- expand_groups2(details_row$row_groups, details_row$col_groups)

  res_rowData <- get_group_indices(details_row, expanded_row, type = "rowData")
  expect_equal(length(res_rowData), 2)
  expect_equal(res_rowData[[1]], c(1, 4))

  res_colData <- get_group_indices(details_row, expanded_row, type = "colData")
  expect_null(res_colData)
})

test_that("sequence of calls in new_plyxp_manager.SummarizedExperiment works", {
  # Grouped SE
  gse <- group_by(se_simple, rows(direction), cols(condition))

  groups <- group_details(gse)
  expect_type(groups, "list")
  expect_s3_class(groups$row_groups, "data.frame")
  expect_s3_class(groups$col_groups, "data.frame")

  expanded <- expand_groups2(groups$row_groups, groups$col_groups)
  expect_s3_class(expanded, "data.frame")
  expect_equal(nrow(expanded), 4)

  assays_idx <- get_group_indices(groups, expanded, "assay")
  expect_type(assays_idx, "list")
  expect_equal(length(assays_idx), 4)

  row_idx <- get_group_indices(groups, expanded, "rowData")
  expect_type(row_idx, "list")
  expect_equal(length(row_idx), 2)

  col_idx <- get_group_indices(groups, expanded, "colData")
  expect_type(col_idx, "list")
  expect_equal(length(col_idx), 2)
})
