test_that("unused arguments notify user", {
  expect_warning(
    pull(se_simple, name = "foo"),
    "arg `name` is not used in pull.SummarizedExperiment()"
  )
})

test_that("pull from assays", {
  expect_identical(
    assay(se(se_simple), 1),
    pull(se_simple, 1)
  )

  expect_identical(
    assay(se(se_simple), "counts"),
    pull(se_simple, counts)
  )

  expect_identical(
    assay(se(se_simple), length(assays(se_simple))),
    pull(se_simple, -1)
  )
})

test_that("pull from rows", {
  expect_identical(
    rowData(se(se_simple))[[1]],
    pull(se_simple, rows(1))
  )

  expect_identical(
    rowData(se(se_simple))[["direction"]],
    pull(se_simple, rows(direction))
  )

  expect_identical(
    rowData(se(se_simple))[[length(rowData(se(se_simple)))]],
    pull(se_simple, rows(-1))
  )

  expect_error(
    pull(se_simple, rows(.features))
  )
})


test_that("pull from rows", {
  expect_identical(
    colData(se(se_simple))[[1]],
    pull(se_simple, cols(1))
  )

  expect_identical(
    colData(se(se_simple))[["condition"]],
    pull(se_simple, cols(condition))
  )

  expect_identical(
    colData(se(se_simple))[[length(colData(se(se_simple)))]],
    pull(se_simple, cols(-1))
  )

  expect_error(
    pull(se_simple, cols(.samples))
  )
})
