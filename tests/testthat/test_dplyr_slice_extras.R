# Tests for the slice_*() family beyond slice() itself:
# slice_min(), and later slice_max(), slice_head(), slice_tail(), ...
#
# se_simple reference data:
#   rows:  row_1 row_2 row_3 row_4 row_5
#   length:    1    24    60    39    37
#   direction: -     +     +     -     +
#   cols:  col_1 col_2 col_3 col_4
#   condition: cntrl cntrl drug drug

# a row variable with ties and a missing value, and a numeric col variable
se_ties <- se_simple |>
  mutate(
    rows(score = c(2, 1, 1, 3, NA)),
    rows(score_na_first = c(NA, 3, 1, 2, 1)),
    cols(score = c(3, 1, 4, 2))
  )

# slice_min ---------------------------------------------------------------

test_that("slice_min/max errors in assays context", {
  expect_error(
    slice_min(se_simple, counts),
    "Cannot slice_min in `assays` context"
  )
  expect_error(
    slice_max(se_simple, counts),
    "Cannot slice_max in `assays` context"
  )
})

test_that("slice_min/max errors on named expressions", {
  expect_error(
    slice_min(se_simple, rows(x = length)),
    "should not be named"
  )
  expect_error(
    slice_max(se_simple, rows(x = length)),
    "should not be named"
  )
})

test_that("slice_min/max errors when both n and prop are supplied", {
  expect_error(
    slice_min(se_simple, rows(length), n = 2, prop = 0.5),
    "Cannot use both `n` and `prop`"
  )
  expect_error(
    slice_max(se_simple, rows(length), n = 2, prop = 0.5),
    "Cannot use both `n` and `prop`"
  )
})

test_that("slice_min/max works with n - rows", {
  res <- expect_no_error(slice_min(se_simple, rows(length), n = 3))
  expect_identical(dim(res), c(3L, 4L))
  expect_identical(rownames(res), c("row_1", "row_2", "row_5"))

  # default n = 1
  res <- slice_min(se_simple, rows(length))
  expect_identical(rownames(res), "row_1")

  res <- expect_no_error(slice_max(se_simple, rows(length), n = 3))
  expect_identical(dim(res), c(3L, 4L))
  expect_identical(rownames(res), c("row_3", "row_4", "row_5"))

  # default n = 1
  res <- slice_max(se_simple, rows(length))
  expect_identical(rownames(res), "row_3")
})

test_that("slice_min/max caps n at the number of rows", {
  res <- slice_min(se_simple, rows(length), n = 10)
  expect_identical(dim(res), c(5L, 4L))

  res <- slice_max(se_simple, rows(length), n = 10)
  expect_identical(dim(res), c(5L, 4L))
})

test_that("slice_min/ax works with prop - rows", {
  # floor(0.4 * 5) = 2
  res <- expect_no_error(slice_min(se_simple, rows(length), prop = 0.4))
  expect_identical(rownames(res), c("row_1", "row_2"))

  res <- expect_no_error(slice_max(se_simple, rows(length), prop = 0.4))
  expect_identical(rownames(res), c("row_3", "row_4"))
})

test_that("slice_min rounds prop down like dplyr", {
  # floor(0.5 * 5) = 2
  res <- slice_min(se_simple, rows(length), prop = 0.5)
  expect_identical(nrow(res), 2L)
})

test_that("slice_min with_ties", {
  # score: 2 1 1 3 NA -> rows 2 and 3 tie for the minimum
  res <- slice_min(se_ties, rows(score), n = 1)
  expect_setequal(rownames(res), c("row_2", "row_3"))

  res <- slice_min(se_ties, rows(score), n = 1, with_ties = FALSE)
  expect_identical(rownames(res), "row_2")

  # ties at the boundary of n are kept
  res <- slice_min(se_ties, rows(score), n = 2)
  expect_setequal(rownames(res), c("row_2", "row_3"))
  res <- slice_min(se_ties, rows(score), n = 3)
  expect_setequal(rownames(res), c("row_1", "row_2", "row_3"))
})

test_that("slice_min na_rm", {
  # dplyr semantics: with na_rm = FALSE, missing values are ordered last
  # and are returned only when needed to fill n
  res <- slice_min(se_ties, rows(score), n = 5)
  expect_setequal(rownames(res), paste0("row_", 1:5))

  res <- slice_min(se_ties, rows(score), n = 5, na_rm = TRUE)
  expect_setequal(rownames(res), paste0("row_", 1:4))

  # NA must not shift which rows are chosen
  res <- slice_min(se_ties, rows(score), n = 3, na_rm = TRUE)
  expect_setequal(rownames(res), c("row_1", "row_2", "row_3"))
  res <- slice_min(se_ties, rows(score), n = 4, with_ties = FALSE, na_rm = TRUE)
  expect_setequal(rownames(res), paste0("row_", 1:4))

  # score_na_first: NA 3 1 2 1 -> minimum is rows 3 and 5
  res <- slice_min(se_ties, rows(score_na_first), n = 1, na_rm = TRUE)
  expect_setequal(rownames(res), c("row_3", "row_5"))
  res <- slice_min(se_ties, rows(score_na_first), n = 1)
  expect_setequal(rownames(res), c("row_3", "row_5"))
})

test_that("slice_min works in cols context", {
  # col score: 3 1 4 2
  res <- expect_no_error(slice_min(se_ties, cols(score), n = 2))
  expect_identical(dim(res), c(5L, 2L))
  expect_setequal(colnames(res), c("col_2", "col_4"))
})

test_that("slice_min works in rows and cols contexts together", {
  res <- expect_no_error(
    slice_min(se_ties, rows(length), cols(score), n = 2)
  )
  expect_identical(dim(res), c(2L, 2L))
  expect_setequal(rownames(res), c("row_1", "row_2"))
  expect_setequal(colnames(res), c("col_2", "col_4"))
})

test_that("slice_min works per group - rows", {
  gse <- group_by(se_simple, rows(direction))

  # "-": row_1 (1), row_4 (39); "+": row_2 (24), row_3 (60), row_5 (37)
  res <- expect_no_error(slice_min(gse, rows(length), n = 1))
  expect_setequal(rownames(res), c("row_1", "row_2"))

  res <- slice_min(gse, rows(length), n = 2)
  expect_setequal(rownames(res), c("row_1", "row_4", "row_2", "row_5"))

  # n larger than a group is capped per group
  res <- slice_min(gse, rows(length), n = 3)
  expect_identical(nrow(res), 5L)
})

test_that("slice_min prop is applied per group", {
  gse <- group_by(se_simple, rows(direction))
  # "-": floor(0.5 * 2) = 1 -> row_1; "+": floor(0.5 * 3) = 1 -> row_2
  res <- slice_min(gse, rows(length), prop = 0.5)
  expect_setequal(rownames(res), c("row_1", "row_2"))
})

test_that("slice_min works per group - cols", {
  gse <- group_by(se_ties, cols(condition))
  # cntrl: col_1 (3), col_2 (1); drug: col_3 (4), col_4 (2)
  res <- expect_no_error(slice_min(gse, cols(score), n = 1))
  expect_setequal(colnames(res), c("col_2", "col_4"))
})

test_that("slice_min keeps grouping", {
  gse <- group_by(se_simple, rows(direction))
  res <- slice_min(gse, rows(length), n = 1)
  expect_s3_class(group_data(res), "plyxp_groups")
})

test_that("slice_min endomorphism", {
  res <- slice_min(se_simple, rows(length), n = 2)
  endo <- se_simple[c("row_1", "row_2"), ]
  expect_identical(res, endo)
})

test_that("slice_min returns results sorted by value", {
  # length: 1 24 60 39 37
  res <- slice_min(se_simple, rows(length), n = 5)
  expect_identical(
    rownames(res),
    c("row_1", "row_2", "row_5", "row_4", "row_3")
  )
})

test_that("slice_min matches dplyr::slice_min on rowData", {
  rd <- as.data.frame(rowData(se(se_ties)))
  rd$.id <- rownames(rd)
  args <- list(
    list(n = 1),
    list(n = 2),
    list(n = 3),
    list(n = 10),
    list(prop = 0.4),
    list(prop = 0.99),
    list(n = 1, with_ties = FALSE),
    list(n = 4, with_ties = FALSE),
    list(n = 5, na_rm = TRUE),
    list(n = 2, na_rm = TRUE, with_ties = FALSE)
  )
  for (var in c("score", "score_na_first", "length")) {
    for (a in args) {
      expected <- do.call(dplyr::slice_min, c(list(rd, rd[[var]]), a))$.id
      actual <- rownames(
        do.call(
          slice_min,
          c(list(se_ties, rlang::expr(rows(!!rlang::sym(var)))), a)
        )
      )
      expect_identical(actual, expected, label = paste(var, deparse(a)))
    }
  }
})


test_that("slice_min returns results sorted by value", {
  # length: 1 24 60 39 37
  res <- slice_min(se_simple, rows(length), n = 5)
  expect_identical(
    rownames(res),
    c("row_1", "row_2", "row_5", "row_4", "row_3")
  )
})

test_that("slice_max matches dplyr::slice_min on rowData", {
  rd <- as.data.frame(rowData(se(se_ties)))
  rd$.id <- rownames(rd)
  args <- list(
    list(n = 1),
    list(n = 2),
    list(n = 3),
    list(n = 10),
    list(prop = 0.4),
    list(prop = 0.99),
    list(n = 1, with_ties = FALSE),
    list(n = 4, with_ties = FALSE),
    list(n = 5, na_rm = TRUE),
    list(n = 2, na_rm = TRUE, with_ties = FALSE)
  )
  for (var in c("score", "score_na_first", "length")) {
    for (a in args) {
      expected <- do.call(dplyr::slice_max, c(list(rd, rd[[var]]), a))$.id
      actual <- rownames(
        do.call(
          slice_max,
          c(list(se_ties, rlang::expr(rows(!!rlang::sym(var)))), a)
        )
      )
      expect_identical(actual, expected, label = paste(var, deparse(a)))
    }
  }
})

# slice_head / slice_tail -------------------------------------------------

test_that("slice_head/tail default to n = 1", {
  res <- expect_no_error(slice_head(se_simple, rows(), cols()))
  expect_identical(dimnames(res), list("row_1", "col_1"))

  res <- expect_no_error(slice_tail(se_simple, rows(), cols()))
  expect_identical(dimnames(res), list("row_5", "col_4"))

  expect_error(slice_head(se_simple, foo), "must be explicitly named")
})

test_that("slice_head/tail works with n - rows", {
  res <- slice_head(se_simple, rows(), n = 3)
  expect_identical(
    dimnames(res),
    list(sprintf("row_%i", 1:3), sprintf("col_%i", 1:4))
  )

  res <- slice_tail(se_simple, rows(), n = 3)
  expect_identical(
    dimnames(res),
    list(sprintf("row_%i", 3:5), sprintf("col_%i", 1:4))
  )
})

test_that("slice_head/tail works n - cols", {
  res <- slice_head(se_simple, cols(), n = 2)
  expect_identical(
    dimnames(res),
    list(sprintf("row_%i", 1:5), sprintf("col_%i", 1:2))
  )

  res <- slice_tail(se_simple, cols(), n = 2)
  expect_identical(
    dimnames(res),
    list(sprintf("row_%i", 1:5), sprintf("col_%i", 3:4))
  )
})

test_that("slice_head/tail works in rows and cols contexts together", {
  res <- slice_head(se_simple, rows(), cols(), n = 2)
  expect_identical(
    dimnames(res),
    list(sprintf("row_%i", 1:2), sprintf("col_%i", 1:2))
  )

  res <- slice_tail(se_simple, rows(), cols(), n = 2)
  expect_identical(
    dimnames(res),
    list(sprintf("row_%i", 4:5), sprintf("col_%i", 3:4))
  )
})

test_that("slice_head/tail works with prop", {
  res <- slice_head(se_simple, rows(), prop = 0.4)
  expect_identical(rownames(res), c("row_1", "row_2"))
  res <- slice_tail(se_simple, rows(), prop = 0.4)
  expect_identical(rownames(res), c("row_4", "row_5"))

  res <- slice_head(se_simple, cols(), prop = 0.5)
  expect_identical(colnames(res), c("col_1", "col_2"))
  res <- slice_tail(se_simple, cols(), prop = 0.5)
  expect_identical(colnames(res), c("col_3", "col_4"))
})

test_that("slice_head/tail caps n at the context size and allows n = 0", {
  res <- slice_head(se_simple, rows(), n = 10)
  expect_identical(dim(res), c(5L, 4L))
  res <- slice_tail(se_simple, cols(), prop = 2)
  expect_identical(dim(res), c(5L, 4L))

  res <- slice_head(se_simple, rows(), n = 0)
  expect_identical(dim(res), c(0L, 4L))
  res <- slice_tail(se_simple, cols(), n = 0)
  expect_identical(dim(res), c(5L, 0L))
})

test_that("slice_head/tail accept negative n and prop like dplyr", {
  # n = -2 drops 2 from the opposite end
  res <- slice_head(se_simple, rows(), n = -2)
  expect_identical(rownames(res), c("row_1", "row_2", "row_3"))
  res <- slice_tail(se_simple, rows(), n = -2)
  expect_identical(rownames(res), c("row_3", "row_4", "row_5"))

  # ceiling(5 - 0.4 * 5) = 3
  res <- slice_head(se_simple, rows(), prop = -0.4)
  expect_identical(rownames(res), c("row_1", "row_2", "row_3"))

  # dropping more than exists returns nothing
  res <- slice_tail(se_simple, rows(), n = -10)
  expect_identical(nrow(res), 0L)
})

test_that("slice_head/tail works per group", {
  # "-": row_1, row_4; "+": row_2, row_3, row_5
  gse <- group_by(se_simple, rows(direction))
  res <- slice_head(gse, rows(), n = 1)
  expect_setequal(rownames(res), c("row_1", "row_2"))
  res <- slice_tail(gse, rows(), n = 1)
  expect_setequal(rownames(res), c("row_4", "row_5"))

  # n larger than a group is capped per group
  res <- slice_head(gse, rows(), n = 2)
  expect_setequal(rownames(res), c("row_1", "row_4", "row_2", "row_3"))

  # cntrl: col_1, col_2; drug: col_3, col_4
  gse <- group_by(se_simple, cols(condition))
  res <- slice_head(gse, cols(), n = 1)
  expect_setequal(colnames(res), c("col_1", "col_3"))
  res <- slice_tail(gse, cols(), n = 1)
  expect_setequal(colnames(res), c("col_2", "col_4"))

  # groups are kept
  expect_s3_class(group_data(res), "plyxp_groups")
})

test_that("slice_head/tail endomorphism", {
  expect_identical(
    slice_head(se_simple, rows(), cols(), n = 2),
    se_simple[1:2, 1:2]
  )
  expect_identical(
    slice_tail(se_simple, rows(), cols(), n = 2),
    se_simple[4:5, 3:4]
  )
})

test_that("slice_head/tail errors", {
  for (f in list(slice_head, slice_tail)) {
    expect_error(f(se_simple, rows(), n = 2, prop = 0.5), "Cannot use both")
    # no context supplied
    expect_error(f(se_simple), "must be empty quosures")
    # contexts must be empty
    expect_error(f(se_simple, rows(length), n = 1), "must be empty quosures")
    expect_error(f(se_simple, cols(), rows(length)), "must be empty quosures")
    # assays context
    expect_error(f(se_simple, counts, n = 1), "in `assays` context")
  }
})

test_that("slice_head/tail matches dplyr on rowData", {
  rd <- as.data.frame(rowData(se(se_simple)))
  rd$.id <- rownames(rd)
  args <- list(
    list(n = 1),
    list(n = 3),
    list(n = 10),
    list(n = 0),
    list(n = -1),
    list(n = -10),
    list(prop = 0.4),
    list(prop = 0.99),
    list(prop = -0.5)
  )
  for (f in c("slice_head", "slice_tail")) {
    for (a in args) {
      expected <- do.call(getExportedValue("dplyr", f), c(list(rd), a))$.id
      actual <- rownames(do.call(f, c(list(se_simple, quote(rows())), a)))
      expect_identical(actual, expected, label = paste(f, deparse(a)))
    }
  }
})

# slice_sample ------------------------------------------------------------

test_that("slice_sample default n = 1", {
  set.seed(1)
  res <- expect_no_error(slice_sample(se_simple, rows(), cols()))
  expect_identical(dim(res), c(1L, 1L))

  # weight_by alone implies the context
  res <- expect_no_error(slice_sample(se_simple, weight_by = rows(length)))
  expect_identical(dim(res), c(1L, 4L))

  # dplyr's generic rejects a single unnamed argument without `n`/`prop`
  expect_error(slice_sample(se_simple, rows()), "must be explicitly named")
})

test_that("slice_sample samples rows like sample.int", {
  set.seed(42)
  expected <- rownames(se_simple)[sample.int(5, 3)]
  set.seed(42)
  res <- slice_sample(se_simple, rows(), n = 3)
  expect_identical(dim(res), c(3L, 4L))
  expect_identical(rownames(res), expected)
})

test_that("slice_sample works in cols and in both contexts", {
  set.seed(1)
  res <- slice_sample(se_simple, cols(), n = 2)
  expect_identical(dim(res), c(5L, 2L))
  expect_true(all(colnames(res) %in% colnames(se_simple)))
  expect_false(anyDuplicated(colnames(res)) > 0)

  res <- slice_sample(se_simple, rows(), cols(), n = 2)
  expect_identical(dim(res), c(2L, 2L))
})

test_that("slice_sample works with prop", {
  set.seed(1)
  # floor(0.4 * 5) = 2; floor(0.5 * 4) = 2
  res <- slice_sample(se_simple, rows(), prop = 0.4)
  expect_identical(nrow(res), 2L)
  res <- slice_sample(se_simple, cols(), prop = 0.5)
  expect_identical(ncol(res), 2L)
})

test_that("slice_sample without replacement caps n at the context size", {
  set.seed(1)
  res <- slice_sample(se_simple, rows(), n = 10)
  expect_identical(nrow(res), 5L)
  expect_setequal(rownames(res), rownames(se_simple))
})

test_that("slice_sample with replacement can exceed the context size", {
  set.seed(1)
  res <- slice_sample(se_simple, rows(), n = 10, replace = TRUE)
  expect_identical(dim(res), c(10L, 4L))
  expect_true(all(rownames(res) %in% rownames(se_simple)))

  res <- slice_sample(se_simple, cols(), prop = 2, replace = TRUE)
  expect_identical(dim(res), c(5L, 8L))
})

test_that("slice_sample weights in dots and weight_by", {
  # only rows with a non-zero weight can be drawn
  plus <- c("row_2", "row_3", "row_5")
  set.seed(1)
  res <- slice_sample(se_simple, rows(as.numeric(direction == "+")), n = 3)
  expect_setequal(rownames(res), plus)

  res <- slice_sample(
    se_simple,
    n = 3,
    weight_by = rows(as.numeric(direction == "+"))
  )
  expect_setequal(rownames(res), plus)

  # an empty rows() merges with weight_by = rows(...)
  res <- slice_sample(
    se_simple,
    rows(),
    n = 3,
    weight_by = rows(as.numeric(direction == "+"))
  )
  expect_setequal(rownames(res), plus)

  # weight_by in one context, uniform in the other
  res <- slice_sample(
    se_simple,
    cols(),
    n = 2,
    weight_by = rows(as.numeric(direction == "+"))
  )
  expect_identical(dim(res), c(2L, 2L))
  expect_true(all(rownames(res) %in% plus))
})

test_that("slice_sample weights in dots and weight_by give identical draws", {
  set.seed(7)
  a <- slice_sample(se_simple, rows(length), n = 3)
  set.seed(7)
  b <- slice_sample(se_simple, n = 3, weight_by = rows(length))
  expect_identical(a, b)

  # and both match sample.int with prob
  set.seed(7)
  expected <- rownames(se_simple)[
    sample.int(5, 3, prob = c(1, 24, 60, 39, 37))
  ]
  expect_identical(rownames(a), expected)
})

test_that("slice_sample works per group", {
  set.seed(1)
  # "-": 2 rows, "+": 3 rows -> n = 2 takes everything from "-"
  gse <- group_by(se_simple, rows(direction))
  res <- slice_sample(gse, rows(), n = 2)
  expect_identical(nrow(res), 4L)
  expect_identical(
    as.vector(table(rowData(se(res))$direction)[c("-", "+")]),
    c(2L, 2L)
  )
  expect_s3_class(group_data(res), "plyxp_groups")

  # weights are evaluated per group
  # "-": only row_4 has length > 30; "+": row_3 or row_5
  res <- slice_sample(gse, rows(as.numeric(length > 30)), n = 1)
  expect_identical(nrow(res), 2L)
  expect_true("row_4" %in% rownames(res))
  expect_true(any(c("row_3", "row_5") %in% rownames(res)))

  # cntrl: col_1, col_2; drug: col_3, col_4
  gse <- group_by(se_simple, cols(condition))
  res <- slice_sample(gse, cols(), n = 1)
  expect_identical(ncol(res), 2L)
  expect_setequal(colData(se(res))$condition, c("cntrl", "drug"))
})

test_that("slice_sample errors", {
  # no context supplied
  expect_error(slice_sample(se_simple, n = 1))
  # assays context, in dots or in a bare weight_by
  expect_error(slice_sample(se_simple, counts, n = 1), "assays")
  expect_error(slice_sample(se_simple, weight_by = length, n = 1), "assays")
  # n and prop together
  expect_error(
    slice_sample(se_simple, rows(), n = 2, prop = 0.5),
    "Cannot use both"
  )
  # more than one weight per context
  expect_error(
    slice_sample(se_simple, rows(length, length), n = 1),
    "more than 1 weight per context"
  )
  expect_error(
    slice_sample(se_simple, rows(length), weight_by = rows(length), n = 1),
    "more than 1 weight per context"
  )
  # invalid weights
  expect_error(slice_sample(se_simple, rows(-length), n = 1))
  expect_error(slice_sample(se_simple, rows(c(1, NA, 1, 1, 1)), n = 1))
  expect_error(
    slice_sample(se_simple, rows(c(1, 2)), n = 1),
    "weight must be size"
  )
})
