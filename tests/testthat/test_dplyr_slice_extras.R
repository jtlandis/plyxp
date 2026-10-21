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

test_that("slice_min errors in assays context", {
  expect_error(
    slice_min(se_simple, counts),
    "Cannot slice_min in `assays` context"
  )
})

test_that("slice_min errors on named expressions", {
  expect_error(
    slice_min(se_simple, rows(x = length)),
    "should not be named"
  )
})

test_that("slice_min errors when both n and prop are supplied", {
  expect_error(
    slice_min(se_simple, rows(length), n = 2, prop = 0.5),
    "Cannot use both `n` and `prop`"
  )
})

test_that("slice_min works with n - rows", {
  res <- expect_no_error(slice_min(se_simple, rows(length), n = 3))
  expect_identical(dim(res), c(3L, 4L))
  expect_identical(rownames(res), c("row_1", "row_2", "row_5"))

  # default n = 1
  res <- slice_min(se_simple, rows(length))
  expect_identical(rownames(res), "row_1")
})

test_that("slice_min caps n at the number of rows", {
  res <- slice_min(se_simple, rows(length), n = 10)
  expect_identical(dim(res), c(5L, 4L))
})

test_that("slice_min works with prop - rows", {
  # floor(0.4 * 5) = 2
  res <- expect_no_error(slice_min(se_simple, rows(length), prop = 0.4))
  expect_identical(rownames(res), c("row_1", "row_2"))
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
  expect_identical(rownames(res), c("row_1", "row_2", "row_5", "row_4", "row_3"))
})

test_that("slice_min matches dplyr::slice_min on rowData", {
  rd <- as.data.frame(rowData(se(se_ties)))
  rd$.id <- rownames(rd)
  args <- list(
    list(n = 1), list(n = 2), list(n = 3), list(n = 10),
    list(prop = 0.4), list(prop = 0.99),
    list(n = 1, with_ties = FALSE), list(n = 4, with_ties = FALSE),
    list(n = 5, na_rm = TRUE), list(n = 2, na_rm = TRUE, with_ties = FALSE)
  )
  for (var in c("score", "score_na_first", "length")) {
    for (a in args) {
      expected <- do.call(dplyr::slice_min, c(list(rd, rd[[var]]), a))$.id
      actual <- rownames(
        do.call(slice_min, c(list(se_ties, rlang::expr(rows(!!rlang::sym(var)))), a))
      )
      expect_identical(actual, expected, label = paste(var, deparse(a)))
    }
  }
})
