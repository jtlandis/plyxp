test_that("mutate works - no groups", {
  res <- expect_no_error(
    mutate(se_simple, foo = 1:n(), rows(foo = 1:n()), cols(foo = 1:n()))
  )@se

  expect_identical(
    assay(res, "foo") |> unname(),
    matrix(1:20, nrow = 5, ncol = 4)
  )
  expect_identical(rowData(res)[["foo"]], 1:5)
  expect_identical(colData(res)[["foo"]], 1:4)
})

test_that("mutate works with scalars in assays ctx", {
  expect_no_error(res <- se_simple |> mutate(foo = 1L))
  expect_identical(unname(assay(res, "foo")), matrix(1L, 5L, 4L))
})

test_that("enforce_2d_size works", {
  # scalar
  res <- enforce_2d_size(1, 3, 2)
  expect_equal(dim(res), c(3, 2))
  expect_equal(as.vector(res), rep(1, 6))

  # 1D vector equal to nrow * ncol
  res <- enforce_2d_size(1:6, 3, 2)
  expect_equal(dim(res), c(3, 2))
  expect_equal(as.vector(res), 1:6)

  # 1D vector multiple of nrow * ncol
  res <- enforce_2d_size(1:12, 3, 2)
  expect_equal(dim(res), c(3, 2, 2))
  expect_equal(as.vector(res), 1:12)

  # wrong length 1D vector
  expect_error(
    enforce_2d_size(1:7, 3, 2),
    "Assay length \\(7\\) is not a equal to or a multiple of expected dimensions \\(3, 2\\)"
  )

  # 2D matrix exact size
  mat <- matrix(1:6, 3, 2)
  res <- enforce_2d_size(mat, 3, 2)
  expect_equal(res, mat)

  # 2D matrix wrong size
  mat_wrong <- matrix(1:6, 2, 3)
  expect_error(
    enforce_2d_size(mat_wrong, 3, 2),
    "Assay dimensions \\(2, 3\\) do not match expected dimensions \\(3, 2\\)"
  )

  # 1D array, some multiple
  arr <- array(1:12, c(3, 2, 2))
  res <- enforce_2d_size(1:12, 3, 2)
  expect_equal(res, arr)

  # 1D array, wrong size
  expect_error(
    enforce_2d_size(1:13, 3, 2),
    "Assay length \\(13\\) is not a equal to or a multiple of expected dimensions \\(3, 2\\)"
  )
})

test_that("mutate works - with groups", {
  gse <- group_by(se_simple, rows(direction), cols(condition))

  res <- expect_no_error(
    mutate(
      gse,
      foo = cur_group_id(),
      rows(foo = cur_group_id()),
      cols(foo = cur_group_id())
    )@se
  )
  expect_identical(
    assay(res, "foo") |> unname(),
    matrix(
      c(
        rep(c(1L, 2L, 2L, 1L, 2L), 2),
        rep(c(3L, 4L, 4L, 3L, 4L), 2)
      ),
      nrow = 5,
      ncol = 4
    )
  )
  expect_identical(rowData(res)[["foo"]], c(1L, 2L, 2L, 1L, 2L))
  expect_identical(colData(res)[["foo"]], c(1L, 1L, 2L, 2L))
})
