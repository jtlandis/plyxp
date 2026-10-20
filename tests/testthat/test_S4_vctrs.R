test_that("vec_size works on S3 and S4 vectors", {
  expect_identical(
    vec_size(1:2),
    2L
  )
  expect_identical(
    vec_size(S4Vectors::Rle(rep(1:2, each = 5))),
    10L
  )
  expect_identical(
    vec_size(S4Vectors::DataFrame(y = letters[1:3], z = c(TRUE, FALSE, TRUE))),
    3L
  )
})


test_that("vec_chop works on S3 and S4 vectors", {
  expect_identical(
    vec_chop(1:5, indices = list(1:2, 3:5)),
    list(1:2, 3:5)
  )
  expect_identical(
    vec_chop(1:5, list(1:2, 3:5)),
    list(1:2, 3:5)
  )

  expect_identical(
    vec_chop(1:5),
    as.list(1:5)
  )

  expect_identical(
    vec_chop(S4Vectors::Rle(rep(1:2, each = 5)), indices = list(1:5, 6:10)),
    list(S4Vectors::Rle(rep(1L, 5)), S4Vectors::Rle(rep(2L, 5)))
  )

  expect_identical(
    vec_chop(S4Vectors::Rle(rep(1:2, each = 5)), list(1:5, 6:10)),
    list(S4Vectors::Rle(rep(1L, 5)), S4Vectors::Rle(rep(2L, 5)))
  )

  expect_identical(
    vec_chop(S4Vectors::Rle(rep(1:2, each = 5))),
    split(S4Vectors::Rle(rep(1:2, each = 5)), 1:10) |> unname()
  )
})

test_that("vec_chop errors on imporper args with S4 vectors", {
  expect_error(
    vec_chop(
      S4Vectors::Rle(rep(1:2, each = 5)),
      list(1:5, 6:10),
      indices = list(1:5, 6:10)
    ),
    "dots arguments found and `indices` is not NULL"
  )

  expect_error(
    vec_chop(
      S4Vectors::Rle(rep(1:2, each = 5)),
      list(1:5, 6:10),
      list(1:5, 6:10)
    ),
    "dots arguments found. Only 1 unnamed argument supporte for dots"
  )
})


test_that("vec_cast data.frame and DFrame are possible", {
  df <- data.frame(
    x = 1:3,
    y = letters[1:3],
    z = logical(3),
    row.names = sprintf("row%i", 1:3)
  )
  DF <- S4Vectors::DataFrame(
    y = letters[4:6],
    z = c(TRUE, FALSE, TRUE)
  )

  expect_no_error(
    vec_ptype2(DF, DF)
  )

  expect_no_error(
    vec_cast(DF, df)
  )

  DF2 <- methods::as(df, "DFrame")

  expect_no_error(
    vec_cast(DF, DF2)
  )

  df <- as.data.frame(DF)

  expect_no_error(
    vec_cast(df, DF2)
  )
})


test_that("group_id and group_loc works", {
  DF <- S4Vectors::DataFrame(
    x = rep(letters[1:5], each = 2),
    y = S4Vectors::Rle(rep(1:2, each = 5))
  )

  expect_identical(
    vec_group_id(DF$y) |> c(),
    rep(1L:2L, each = 5L)
  )

  expect_identical(
    vec_group_id(DF) |> c(),
    c(1L, 1L, 2L, 2L, 3L, 4L, 5L, 5L, 6L, 6L)
  )

  expect_no_error(
    vec_group_loc(DF$y)
  )

  res <- expect_no_error(
    vec_group_loc(DF)
  )

  expect_identical(
    colnames(res),
    c("key", "loc")
  )

  expect_s4_class(
    res$key,
    "DFrame"
  )
})

test_that("vec_rep errors with `times` arg", {
  rle <- S4Vectors::Rle(vec_rep_each(1:2, 5))

  expect_error(
    vec_rep(rle, c(2, 0)),
    "`times` must be a single number"
  )

  expect_error(
    vec_rep_each(rle, c(2, 0)),
    "`times` must be a single number"
  )

  expect_identical(
    vec_rep_each(rle, 2) |> vec_size(),
    20L
  )
})

test_that("vec_recycle errors with `size` arg", {
  rle <- S4Vectors::Rle(vec_rep_each(1:2, 5))

  expect_error(
    vec_recycle(rle, c(2, 0)),
    "argument `size` should be length 1L"
  )

  expect_error(
    vec_recycle(rle, "2"),
    "argument `size` should be integer-ish"
  )

  expect_error(
    vec_recycle(rle, 2),
    "Can't recycle inpute of size 10 to size 2"
  )

  expect_identical(
    vec_slice(rle, 1L) |> vec_recycle(5) |> vec_size(),
    5L
  )
  expect_identical(
    vec_recycle(rle, 10L),
    rle
  )
})


test_that("vec_cast fails for unknown classes", {
  foo <- S7::new_class("foo")
  bar <- S7::new_class("bar")

  expect_error(
    vec_cast(foo, bar)
  )

  expect_error(
    vec_cast(foo(), bar)
  )

  brle <- base::rle(c(1, 1, 1, 2, 2))
  s4rle <- S4Vectors::Rle(c(1, 1, 1, 2, 2))

  expect_error(
    vec_cast(brle, s4rle)
  )

  expect_null(
    obj_class(NULL)
  )
  expect_identical(
    class_name_(NULL),
    "NULL"
  )
  expect_error(
    class_name_(1L),
    "`cls` is not S4 or S7, expected character vector S3"
  )
})


test_that("slicing DFrame keeps rownames", {
  DF <- S4Vectors::DataFrame(
    x = 1:3,
    y = letters[1:3],
    z = logical(3),
    row.names = sprintf("row%i", 1:3)
  )

  DF <- expect_no_error(
    vec_slice(DF, 1:2)
  )
  expect_identical(
    rownames(DF),
    sprintf("row%i", 1:2)
  )
})
