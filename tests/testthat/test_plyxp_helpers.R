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
