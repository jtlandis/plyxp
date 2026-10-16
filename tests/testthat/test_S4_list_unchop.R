test_that("can find common size", {
  expect_error(
    plyxp_size_common(
      1:3,
      1:6
    )
  )
  expect_identical(
    plyxp_size_common(
      1,
      2
    ),
    1L
  )

  expect_identical(
    plyxp_size_common(
      1:10,
      2
    ),
    10L
  )

  expect_error(
    plyxp_size_common(
      numeric(),
      1:10
    )
  )
})

test_that("can combind DF and data.frame", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  DF <- S4Vectors::DataFrame(y = letters[4:6], z = c(TRUE, FALSE, TRUE))
  ptype <- vec_ptype_common(df, DF)
  expect_identical(
    dim(ptype),
    c(0L, 3L)
  )
  expect_identical(
    dim(vec_c(df, DF)),
    c(6L, 3L)
  )
  expect_identical(
    dim(vec_c(DF, df)),
    c(6L, 3L)
  )
})


test_that("Cannot lossy cast data frames", {
  DF <- S4Vectors::DataFrame(y = letters[1:3], z = c(TRUE, FALSE, TRUE))
  DF2 <- S4Vectors::DataFrame(y = letters[4:6])
  expect_error(
    vec_cast(DF, DF2)
  )
})

test_that("Cannot init malformed DFrame", {
  DF <- S4Vectors::DataFrame(y = letters[1:3], z = c(TRUE, FALSE, TRUE))
  S4Vectors::mcols(DF) <- S4Vectors::DataFrame(a = 1:2)
  expect_identical(
    S4Vectors::mcols(vec_init(DF, 4L)),
    S4Vectors::mcols(DF)
  )
  DF@listData <- DF@listData[1L]
  expect_error(
    vec_init(DF, 4L),
    "cannot init DFrame. Improper mcols length"
  )
})
