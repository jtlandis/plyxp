test_that("no groups exist", {
  expect_null(group_data(se_simple))
})

test_that("group_by aborts on invalid input", {
  expect_error(
    group_by(se_simple, 1L),
    "Cannot group_by in `assays` context"
  )
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
  gdata_ <- group_data(arrange(
    gse,
    rows(dplyr::desc(.features)),
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
  gdata_ <- group_data(slice(
    gse,
    rows(which(direction == "-")),
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

test_that(".add works in contexts", {
  expect_identical(
    group_data(
      gse <- group_by(se_simple, rows(direction), .add = TRUE)
    )$row_groups |>
      nrow(),
    2L
  )
  gse2 <- gse |>
    group_by(rows(length), .add = TRUE)
  gd2 <- gse2 |>
    group_data() |>
    _$row_groups

  expect_identical(
    nrow(gd2),
    5L
  )
  expect_identical(
    names(gd2[1:2]),
    c("direction", "length")
  )

  expect_identical(
    group_data(
      gse <- group_by(se_simple, cols(condition), .add = TRUE)
    )$col_groups |>
      nrow(),
    2L
  )
  gse2 <- gse |>
    group_by(cols(sample), .add = TRUE)
  gd2 <- gse2 |>
    group_data() |>
    _$col_groups

  expect_identical(
    nrow(gd2),
    4L
  )
  expect_identical(
    names(gd2[1:2]),
    c("condition", "sample")
  )
})

test_that("can group_by .features or .samples", {
  expect_identical(
    group_data(
      group_by(se_simple, rows(.features))
    )$row_groups |>
      nrow(),
    nrow(se_simple)
  )
  expect_identical(
    group_data(
      group_by(se_simple, cols(.samples))
    )$col_groups |>
      nrow(),
    ncol(se_simple)
  )
})

test_that("ungrouped object is identical", {
  expect_identical(
    se_simple,
    ungroup(se_simple)
  )
})

test_that("can selectively ungroup data", {
  gse <- group_by(se_simple, rows(direction, length), cols(condition))
  expect_identical(
    ungroup(gse) |> group_data(),
    NULL
  )

  gd <- ungroup(gse, cols()) |> group_data()
  expect_identical(
    gd$col_groups,
    NULL
  )
  expect_identical(
    gd$row_groups,
    group_data(gse)$row_groups
  )

  gd <- ungroup(gse, rows()) |> group_data()
  expect_identical(
    gd$row_groups,
    NULL
  )
  expect_identical(
    gd$col_groups,
    group_data(gse)$col_groups
  )

  gd <- ungroup(gse, rows(length)) |> group_data()
  expect_identical(
    nrow(gd$row_groups),
    2L
  )
  expect_identical(
    gd$col_groups,
    group_data(gse)$col_groups
  )

  gxp <- group_by(se_simple, rows(direction, gene))
  expect_identical(
    groups(gxp)$row_groups,
    list(quote(direction), quote(gene))
  )
  expect_identical(
    ungroup(gxp, rows(gene)) |> groups() |> _$row_groups,
    list(quote(direction))
  )

  gxp <- group_by(se_simple, cols(condition, sample))
  expect_identical(
    groups(gxp)$col_groups,
    list(quote(condition), quote(sample))
  )
  expect_identical(
    ungroup(gxp, cols(sample)) |> groups() |> _$col_groups,
    list(quote(condition))
  )
})

test_that("cannot ungroup assays", {
  gxp <- group_by(se_simple, rows(direction))
  expect_error(
    ungroup(gxp, counts),
    "Cannot ungroup in `assays` context"
  )
})

test_that("cannot ungroup non-grouped contexts", {
  gxp <- group_by(se_simple, rows(direction))
  expect_error(
    ungroup(gxp, cols()),
    "no col groups to ungroup"
  )
  expect_error(
    ungroup(gxp, cols(condition)),
    "no col groups to ungroup"
  )
  gxp <- group_by(se_simple, cols(condition))
  expect_error(
    ungroup(gxp, rows()),
    "no row groups to ungroup"
  )
  expect_error(
    ungroup(gxp, rows(direction)),
    "no row groups to ungroup"
  )
})

test_that("can query groups properly", {
  expect_null(
    plyxp_curr_groups(se_simple)
  )
  expect_identical(
    group_by(se_simple, rows(direction)) |> plyxp_curr_groups(),
    list(quote(rows(direction)))
  )
  expect_identical(
    group_by(se_simple, cols(condition)) |> plyxp_curr_groups(),
    list(quote(cols(condition)))
  )
  expect_identical(
    group_by(se_simple, rows(direction), cols(condition)) |>
      plyxp_curr_groups(),
    list(quote(rows(direction)), quote(cols(condition)))
  )

  expect_identical(
    group_by(se_simple, cols(condition), rows(direction)) |>
      plyxp_curr_groups(),
    list(quote(rows(direction)), quote(cols(condition)))
  )
})

test_that("group_split() returns itself", {
  expect_identical(
    group_split(se_simple),
    list(se_simple)
  )
})

test_that("group_split() slices objects as expected - rows & cols", {
  gxp <- group_by(se_simple, rows(direction), cols(condition))
  expect_warning(
    gsplit <- group_split(gxp, rows(sample)),
    "... is ignored when `.tbl` is grouped"
  )
  expect_identical(
    group_split(gxp),
    gsplit
  )
  expect_identical(
    group_split(se_simple, rows(direction), cols(condition)),
    gsplit
  )
  expect_identical(
    length(gsplit),
    4L
  )
  expect_identical(
    purrr::map2_lgl(
      gsplit,
      group_data(gxp)$row_groups$direction |>
        rep(2L),
      ~ all(pull(.x, rows(direction)) == .y)
    ),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    purrr::map2_lgl(
      gsplit,
      group_data(gxp)$col_groups$condition |>
        rep(each = 2L),
      ~ all(pull(.x, cols(condition)) == .y)
    ),
    c(TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("group_split() slices objects as expected - rows ", {
  gxp <- group_by(se_simple, rows(direction))
  expect_warning(
    gsplit <- group_split(gxp, rows(sample)),
    "... is ignored when `.tbl` is grouped"
  )
  expect_identical(
    group_split(gxp),
    gsplit
  )
  expect_identical(
    group_split(se_simple, rows(direction)),
    gsplit
  )
  expect_identical(length(gsplit), 2L)
  expect_identical(
    purrr::map2_lgl(
      gsplit,
      group_data(gxp)$row_groups$direction,
      ~ all(pull(.x, rows(direction)) == .y)
    ),
    c(TRUE, TRUE)
  )
})

test_that("group_split() slices objects as expected - cols ", {
  gxp <- group_by(se_simple, cols(condition))
  expect_warning(
    gsplit <- group_split(gxp, rows(sample)),
    "... is ignored when `.tbl` is grouped"
  )
  expect_identical(
    group_split(gxp),
    gsplit
  )
  expect_identical(
    group_split(se_simple, cols(condition)),
    gsplit
  )
  expect_identical(length(gsplit), 2L)
  expect_identical(
    purrr::map2_lgl(
      gsplit,
      group_data(gxp)$col_groups$condition,
      ~ all(pull(.x, cols(condition)) == .y)
    ),
    c(TRUE, TRUE)
  )
})

test_that("ungroup(..., .keep = FALSE) removes fields", {
  gxp <- group_by(se_simple, cols(condition))
  expect_identical(
    group_split(gxp, .keep = FALSE) |>
      purrr::map_lgl(
        ~ !"condition" %in% colnames(.x)
      ),
    c(TRUE, TRUE)
  )
  gxp <- group_by(se_simple, rows(direction))
  expect_identical(
    group_split(gxp, .keep = FALSE) |>
      purrr::map_lgl(
        ~ !"direction" %in% rownames(.x)
      ),
    c(TRUE, TRUE)
  )
  gxp <- group_by(se_simple, cols(condition), rows(direction))
  expect_identical(
    group_split(gxp, .keep = FALSE) |>
      purrr::map_lgl(
        ~ !"condition" %in% colnames(.x)
      ),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    group_split(gxp, .keep = FALSE) |>
      purrr::map_lgl(
        ~ !"direction" %in% rownames(.x)
      ),
    c(TRUE, TRUE, TRUE, TRUE)
  )
})
