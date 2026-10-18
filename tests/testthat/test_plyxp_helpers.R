test_that("plyxp_on() works", {
  wfoo <- plyxp_on(se_simple, .on = metadata, \(meta) {
    meta$foo <- "bar"
    meta
  })
  expect_true(
    !is.null(metadata(wfoo))
  )
  expect_true(
    !is.null(metadata(wfoo)$foo)
  )
  expect_identical(
    metadata(wfoo)$foo,
    "bar"
  )
})

test_that("plyxp_on() sends custom errors", {
  expect_error(
    plyxp_on(se_simple, .on = metadata, \(meta) {
      stop("forcing error")
    }),
    "forcing error"
  )

  custom_on <- function(.x) {
    stop("bad .on function")
    .x
  }

  # fails because custom_on<- not defined
  expect_error(
    plyxp_on(se_simple, .on = custom_on, identity),
    "object 'custom_on<-' of mode 'function' was not found"
  )

  `custom_on<-` <- function(.x) {
    stop("bad `.on<-` function")
    .x
  }

  # custom_on error path forced
  expect_error(
    plyxp_on(se_simple, .on = custom_on, identity),
    "bad .on function"
  )

  custom_on <- function(.x) {
    .x
  }

  # expect there to be a value argument
  expect_error(
    plyxp_on(se_simple, .on = custom_on, identity),
    "unused argument"
  )

  `custom_on<-` <- function(.x, value) {
    stop("bad `.on<-` function")
    value
  }

  expect_error(
    plyxp_on(se_simple, .on = custom_on, identity),
    "bad `.on<-` function"
  )

  `custom_on<-` <- function(.x, value) {
    value
  }

  expect_identical(
    plyxp_on(se_simple, .on = custom_on, identity),
    se_simple
  )
})


test_that("assay() pulls data", {
  se_assays <- assays(se(se_simple))
  expect_identical(
    assay(se_simple),
    se_assays[[1L]]
  )
  expect_identical(
    assay(se_simple, "counts"),
    se_assays[["counts"]]
  )
  expect_identical(
    assay(se_simple, 2L),
    se_assays[[2L]]
  )
})

test_that("assay<-() sets data", {
  se_assays <- assays(se(se_simple))
  xp <- se_simple
  assay(xp) <- se_assays[[1L]] + 1L
  expect_identical(
    assay(xp),
    se_assays[[1L]] + 1L
  )

  assay(xp, 2L) <- se_assays[[2L]] + 1
  expect_identical(
    assay(xp, 2L),
    se_assays[[2L]] + 1
  )
})


test_that("assays<-() sets data", {
  se_assays <- assays(se(se_simple)) |> lapply(\(x) x + 1)
  xp <- se_simple
  assays(xp) <- se_assays
  expect_identical(
    as.list(assays(xp)),
    se_assays
  )
})

test_that("metadata<-() sets data", {
  xp <- se_simple
  metadata(xp)["foo"] <- "bar"
  expect_identical(
    metadata(xp)$foo,
    "bar"
  )
})

test_that("$.PlySummarizedExperiment operates on colData", {
  expect_identical(
    se_simple$condition,
    colData(se_simple)$condition
  )
  xp <- se_simple
  xp$foo <- "bar"
  expect_identical(
    se_simple$foo,
    colData(se_simple)$foo
  )
})

test_that("can pull names of object", {
  xp <- se_simple
  expect_identical(
    names(xp),
    sprintf("row_%i", 1:5)
  )
  expect_identical(
    dimnames(xp),
    list(
      sprintf("row_%i", 1:5),
      sprintf("col_%i", 1:4)
    )
  )
  expect_no_error(
    dimnames(xp)[[1L]] <- sprintf("new_row_%i", 1:5)
  )
  expect_identical(
    dimnames(xp),
    list(
      sprintf("new_row_%i", 1:5),
      sprintf("col_%i", 1:4)
    )
  )
  expect_no_error(
    dimnames(xp)[[2L]] <- sprintf("new_col_%i", 1:4)
  )
  expect_identical(
    dimnames(xp),
    list(
      sprintf("new_row_%i", 1:5),
      sprintf("new_col_%i", 1:4)
    )
  )
  expect_no_error(
    dimnames(xp) <- NULL
  )
  expect_identical(
    dimnames(xp),
    list(
      NULL,
      NULL
    )
  )
})


test_that("plyxp*() errors correctly", {
  expect_error(
    plyxp(se_simple, \(x) stop("forcing error")),
    "forcing error"
  )

  expect_error(
    plyxp(se_simple, colData),
    "must return a .*SummarizedExperiment"
  )

  do_colData <- function(x) {
    stop("forcing error")
  }

  expect_error(
    plyxp_on(se_simple, do_colData, .on = colData),
    "error in `do_colData\\(colData\\(se\\(.data\\)\\), ...\\)`"
  )
})
